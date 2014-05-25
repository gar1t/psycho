-module(psycho_tests).

-export([run/0]).

run() ->
    test_parse_request_path(),
    test_ensure_parsed_request_path(),
    test_routes(),
    test_crypto(),
    test_validate(),
    test_simple_multipart(),
    test_more_multipart().

test_parse_request_path() ->
    io:format("parse_request_path: "),
    P = fun(S) -> psycho_util:parse_request_path(S) end,

    %% Empty case
    {"", "", []} = P(""),

    %% Simple paths
    {"/foo", "", []} = P("/foo"),
    {"/foo/bar", "", []} = P("/foo/bar"),

    %% Path with simple query strings
    {"/foo", "bar=123", [{"bar", "123"}]} =
        P("/foo?bar=123"),
    {"/foo", "bar=123&baz=456", [{"bar", "123"}, {"baz", "456"}]} =
        P("/foo?bar=123&baz=456"),

    %% Query strings with names only
    {"/foo", "bar=123&baz", [{"bar", "123"}, {"baz", ""}]} =
        P("/foo?bar=123&baz"),
    {"/foo", "baz&bar=123", [{"baz", ""}, {"bar", "123"}]} =
        P("/foo?baz&bar=123"),

    %% Query strings with multiple values
    {"/foo", "bar=123&bar=456", [{"bar", "123"}, {"bar", "456"}]} =
        P("/foo?bar=123&bar=456"),

    io:format("OK~n").

test_ensure_parsed_request_path() ->
    io:format("ensure_parsed_request_path: "),
    E = fun(Env) -> psycho_util:ensure_parsed_request_path(Env) end,

    Path = "/foo?bar=123&bar=456",
    Parsed = psycho_util:parse_request_path(Path),
    Env0 = [{request_path, Path}],

    {Parsed, Env1} = E(Env0),
    Env1 = [{parsed_request_path, Parsed}|Env0],
    {Parsed, Env1} = E(Env1),

    io:format("OK~n").

test_routes() ->
    io:format("routes: "),
    R = fun(Routes, Env) -> psycho_route:route(Routes, Env) end,
    R2 = fun(Routes, Env, Opts) -> psycho_route:route(Routes, Env, Opts) end,
    App = fun(Result) -> fun(_Env) -> Result end end,
    Env = fun(Path) -> [{request_path, Path}] end,
    Env2 = fun(Method, Path) -> [{request_method, Method},
                                 {request_path, Path}]
           end,

    NotFoundHandlerOpts = [{not_found_handler, App(not_found)}],

    Routes =
        [{"/", App(root)},
         {"/foo", App(foo)},
         {{exact, "/bar"}, App(bar)},
         {{starts_with, "/bar"}, App(starts_with_bar)},
         {{matches, "^/baz/(bam|BAM)$"}, App(baz_bam)},
         {{matches, "^/baz/"}, App(baz_other)},
         {"POST", "/bam", App(bam_post)},
         {"PUT", {starts_with, "/bam/"}, App(bam_other_put)}],

    %% Test routes to app proxies
    root = R(Env("/"), Routes),
    foo = R(Env("/foo"), Routes),
    bar = R(Env("/bar"), Routes),
    starts_with_bar = R(Env("/bar/baz"), Routes),
    baz_bam = R(Env("/baz/bam"), Routes),
    baz_bam = R(Env("/baz/BAM"), Routes),
    baz_other = R(Env("/baz/bAm"), Routes),
    baz_other = R(Env("/baz/bam/foo"), Routes),
    not_found = R2(Env("baz/bam"), Routes, NotFoundHandlerOpts),
    bam_post = R(Env2("POST", "/bam"), Routes),
    bam_other_put = R(Env2("PUT", "/bam/foo"), Routes),
    not_found = R2(Env("/not_handled"), Routes, NotFoundHandlerOpts),

    %% Default not found handler
    {{404, "Not Found"}, _, _} = R(Env("/not_handled"), Routes),

    io:format("OK~n").

test_crypto() ->
    io:format("crypto: "),

    E = fun(Data, Key) -> psycho_util:encrypt(Data, Key) end,
    D = fun(Data, Key) -> psycho_util:decrypt(Data, Key) end,

    Data1 = <<"hello">>,
    Data2 = <<"there">>,
    Key1 = <<"sesame">>,
    Key2 = <<"letmein">>,

    {ok, Data1} = D(E(Data1, Key1), Key1),
    {ok, Data1} = D(E(Data1, Key2), Key2),
    {ok, Data2} = D(E(Data2, Key1), Key1),
    {ok, Data2} = D(E(Data2, Key2), Key2),

    error = D(E(Data1, Key1), Key2),
    error = D(E(Data1, Key2), Key1),
    error = D(E(Data2, Key1), Key2),
    error = D(E(Data2, Key2), Key1),

    io:format("OK~n").

test_validate() ->
    io:format("validate: "),

    V = fun(Data, Schema) -> psycho_util:validate(Data, Schema) end,

    %% Empty / base case (first arg is passed through on pass)
    {ok, []} = V([], []),
    {ok, data_pass_through} = V(data_pass_through, []),

    %% required
    {error, {"foo", required}} = V([], [{"foo", [required]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", [required]}]),

    %% must_equal, literal
    {error, {"foo", {must_equal, "FOO"}}} =
        V([], [{"foo", [{must_equal, "FOO"}]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", [{must_equal, "FOO"}]}]),

    %% must_equal, reference to another field value
    {ok, _} = V([], [{"foo", [{must_equal, {field, "bar"}}]}]),
    {error, {"foo", {must_equal, {field,"bar"}}}} =
        V([{"foo", "FOO"}], [{"foo", [{must_equal, {field, "bar"}}]}]),
    {error, {"foo", {must_equal, {field,"bar"}}}} =
        V([{"foo", "FOO"}, {"bar", "BAR"}],
          [{"foo", [{must_equal, {field, "bar"}}]}]),
    {ok, _} =
        V([{"foo", "FOO"}, {"bar", "FOO"}],
          [{"foo", [{must_equal, {field, "bar"}}]}]),

    %% min_length
    {error, {"foo", {min_length, 4}}} = V([], [{"foo", [{min_length, 4}]}]),
    {error, {"foo", {min_length, 4}}} =
        V([{"foo", "FOO"}], [{"foo", [{min_length, 4}]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", [{min_length, 3}]}]),

    io:format("OK~n").

test_simple_multipart() ->
    io:format("simple_multipart: "),

    Boundary = <<"------------------------65d0128e83f480f8">>,
    Data =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.txt\"\r\n"
           "Content-Type: text/plain\r\n"
           "\r\n"
           "Hi there.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>,
        <<>>],

    All = apply_data(Data, psycho_multipart:new(Boundary)),

    [{"msg",
      {[{"Content-Disposition",
         "form-data; name=\"msg\"; filename=\"msg.txt\""},
        {"Content-Type","text/plain"}],
       <<"Hi there.\n">>}}] = psycho_multipart:form_data(All),

    io:format("OK~n").

test_more_multipart() ->
    io:format("more_multipart: "),

    New = fun(Boundary) -> psycho_multipart:new(Boundary) end,
    New3 =
        fun(Boundary, Callback, Data) ->
                psycho_multipart:new(Boundary, Callback, Data)
        end,
    FormData = fun(MP) -> psycho_multipart:form_data(MP) end,
    UserData = fun(MP) -> psycho_multipart:user_data(MP) end,

    Boundary = <<"----WebKitFormBoundaryDr6DS6tqR3sKzPnI">>,
    Data =
        [<<"------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten">>,
         <<"t-Disposition: form-data; name=\"name\"\r\n\r\nBob\r\n">>,
         <<"------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten">>,
         <<"t-Disposition: form-data; name=\"awesome\"\r\n\r\non\r\n">>,
         <<"------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten">>,
         <<"t-Disposition: form-data; name=\"file1\"; filename">>,
         <<"=\"file1\"\r\nContent-Type: application/octet-stream">>,
         <<"\r\n\r\nThis is\nfile 1.\n\r\n------WebKitFormBoundaryDr">>,
         <<"6DS6tqR3sKzPnI\r\nContent-Disposition: form-data; ">>,
         <<"name=\"file2\"; filename=\"file2\"\r\nContent-Type: ap">>,
         <<"plication/octet-stream\r\n\r\nThis\nis\nfile 2.\n\r\n----">>,
         <<"--WebKitFormBoundaryDr6DS6tqR3sKzPnI--\r\n">>],

    %% Same as Data but as a single chunk
    Data2 = [iolist_to_binary(Data), <<>>, <<>>, <<>>, <<>>],

    %% Handle all parts (default behavior with no callback)

    All = apply_data(Data, New(Boundary)),
    All = apply_data(Data2, New(Boundary)),

    [{"name",
      {[{"Content-Disposition","form-data; name=\"name\""}],
       <<"Bob">>}},
     {"awesome",
      {[{"Content-Disposition","form-data; name=\"awesome\""}],
       <<"on">>}},
     {"file1",
      {[{"Content-Disposition","form-data; name=\"file1\"; "
         "filename=\"file1\""},
        {"Content-Type","application/octet-stream"}],
       <<"This is\nfile 1.\n">>}},
     {"file2",
      {[{"Content-Disposition","form-data; name=\"file2\"; "
         "filename=\"file2\""},
        {"Content-Type","application/octet-stream"}],
       <<"This\nis\nfile 2.\n">>}}] = FormData(All),

    %% Use part handler callback to drop the two files

    DropFilesHandler = drop_handler(["file1", "file2"]),
    DropFiles = apply_data(Data, New3(Boundary, DropFilesHandler, [])),

    [{"name",
      {[{"Content-Disposition","form-data; name=\"name\""}],
       <<"Bob">>}},
     {"awesome",
      {[{"Content-Disposition","form-data; name=\"awesome\""}],
       <<"on">>}}] = FormData(DropFiles),

    [{keep,"name"},
     {data,"name",<<"Bob">>},
     {data,"name",<<>>},
     {keep,"awesome"},
     {data,"awesome",<<"on">>},
     {data,"awesome",<<>>},
     {drop,"file1"},
     {drop,"file2"}] = lists:reverse(UserData(DropFiles)),

    %% Use a part handler to keep only the two files

    KeepFilesHandler = drop_handler(["name", "awesome"]),
    KeepFiles = apply_data(Data, New3(Boundary, KeepFilesHandler, [])),

    [{"file1",
      {[{"Content-Disposition","form-data; name=\"file1\"; "
         "filename=\"file1\""},
        {"Content-Type","application/octet-stream"}],
       <<"This is\nfile 1.\n">>}},
     {"file2",
      {[{"Content-Disposition","form-data; name=\"file2\"; "
         "filename=\"file2\""},
        {"Content-Type","application/octet-stream"}],
       <<"This\nis\nfile 2.\n">>}}] = FormData(KeepFiles),

    [{drop,"name"},
     {drop,"awesome"},
     {keep,"file1"},
     {data,"file1",<<"This is\nfile 1.\n">>},
     {data,"file1",<<>>},
     {keep,"file2"},
     {data,"file2",<<"This\nis\nfile 2.\n">>},
     {data,"file2",<<>>}] = lists:reverse(UserData(KeepFiles)),

    %% Use a part handler to modify a part

    ModAwesomeHandler = rename_handler("awesome", "lame"),
    ModAwesome = apply_data(Data, New3(Boundary, ModAwesomeHandler, [])),

    [{"lame",
      {[{"Content-Disposition","form-data; name=\"lame\""}],
       <<"on">>}}] = FormData(ModAwesome),

    [{drop,"name"},
     {rename,"awesome","lame"},
     {data,"lame",<<"on">>},
     {data,"lame",<<>>},
     {drop,"file1"},
     {drop,"file2"}] = lists:reverse(UserData(ModAwesome)),

    io:format("OK~n").

drop_handler(Drop) ->
    fun(Part, Acc) -> handle_drop_part(Part, Acc, Drop) end.

handle_drop_part({part, Name, _Headers}, Acc, Drop) ->
    maybe_drop_part(lists:member(Name, Drop), Name, Acc);
handle_drop_part(Part, Acc, _Drop) ->
    {continue, [Part|Acc]}.

maybe_drop_part(true, Name, Acc) ->
    {drop, [{drop, Name}|Acc]};
maybe_drop_part(false, Name, Acc) ->
    {continue, [{keep, Name}|Acc]}.

rename_handler(From, To) ->
    fun(Part, Acc) -> handle_rename(Part, Acc, From, To) end.

handle_rename({part, Name, _Headers}, Acc, From, To) ->
    maybe_rename_part(Name == From, Acc, Name, To);
handle_rename(Part, Acc, _From, _To) ->
    {continue, [Part|Acc]}.

maybe_rename_part(true, Acc, From, To) ->
    Headers = [{"Content-Disposition", "form-data; name=\"" ++ To ++ "\""}],
    {continue, {To, Headers}, [{rename, From, To}|Acc]};
maybe_rename_part(false, Acc, Name, _) ->
    {drop, [{drop, Name}|Acc]}.

apply_data([Data|Rest], MP) ->
    apply_data(Rest, psycho_multipart:data(Data, MP));
apply_data([], MP) -> MP.
