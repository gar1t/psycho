-module(psycho_tests).

-export([run/0]).

run() ->
    try
        test_parse_request_path(),
        test_ensure_parsed_request_path(),
        test_routes(),
        test_crypto(),
        test_validate(),
        test_multipart_simplest(),
        test_multipart_splits(),
        test_multipart_multiple(),
        test_multipart_filtering(),
        test_dispatch_on(),
        test_encode_decode_url(),
        test_chain_apps()
    catch
        _:Err ->
            io:format("ERROR~n~p~n~p~n", [Err, erlang:get_stacktrace()])
    end.

%% ===================================================================
%% parse request path
%% ===================================================================

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

%% ===================================================================
%% ensure parsed request path
%% ===================================================================

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

%% ===================================================================
%% routes
%% ===================================================================

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
    root = R(Routes, Env("/")),
    foo = R(Routes, Env("/foo")),
    bar = R(Routes, Env("/bar")),
    starts_with_bar = R(Routes, Env("/bar/baz")),
    baz_bam = R(Routes, Env("/baz/bam")),
    baz_bam = R(Routes, Env("/baz/BAM")),
    baz_other = R(Routes, Env("/baz/bAm")),
    baz_other = R(Routes, Env("/baz/bam/foo")),
    not_found = R2(Routes, Env("baz/bam"), NotFoundHandlerOpts),
    bam_post = R(Routes, Env2("POST", "/bam")),
    bam_other_put = R(Routes, Env2("PUT", "/bam/foo")),
    not_found = R2(Routes, Env("/not_handled"), NotFoundHandlerOpts),

    %% Default not found handler
    {{404, "Not Found"}, _, _} = R(Routes, Env("/not_handled")),

    io:format("OK~n").

%% ===================================================================
%% crypto
%% ===================================================================

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

%% ===================================================================
%% validate
%% ===================================================================

test_validate() ->
    io:format("validate: "),

    V = fun(Data, Schema) -> psycho_util:validate(Data, Schema) end,

    %% Empty / base case

    {ok, []} = V([], []),

    %% required

    {error, {"foo", required}} = V([], [{"foo", [required]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", [required]}]),

    %% must_equal, literal

    {error, {"foo", {must_equal, "FOO"}}} =
        V([], [{"foo", [{must_equal, "FOO"}]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", [{must_equal, "FOO"}]}]),
    {ok, _} = V([{"foo", "FOO"}], [{"foo", ["FOO"]}]),

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

    %% Any
    {error, {"foo", {any, ["456","789"]}}} =
        V([{"foo", "123"}],
          [{"foo", [{any, ["456", "789"]}]}]),
    {ok, [{"foo", "456"}]} =
        V([{"foo", "456"}],
          [{"foo", [{any, ["456", "789"]}]}]),

    %% Conversions

    {ok, [{"foo", 123}]} = V([{"foo", "123"}], [{"foo", [integer]}]),
    {ok, [{"foo", 123.0}]} = V([{"foo", "123"}], [{"foo", [float]}]),
    {ok, [{"foo", 123.456}]} = V([{"foo", "123.456"}], [{"foo", [float]}]),
    {ok, [{"foo", 123}]} = V([{"foo", "123"}], [{"foo", [number]}]),
    {ok, [{"foo", 123.456}]} = V([{"foo", "123.456"}], [{"foo", [number]}]),

    {error, {"foo", integer}} = V([{"foo", "xxx"}], [{"foo", [integer]}]),
    {error, {"foo", float}} = V([{"foo", "xxx"}], [{"foo", [float]}]),
    {error, {"foo", number}} = V([{"foo", "xxx"}], [{"foo", [number]}]),

    {ok, [{"foo", <<"FOO">>}]} = V([{"foo", "FOO"}], [{"foo", [binary]}]),

    % Optional

    {ok, []} = V([{"foo", "FOO"}], []),

    io:format("OK~n").

%% ===================================================================
%% multipart (simplest)
%% ===================================================================

test_multipart_simplest() ->
    io:format("multipart_simplest: "),

    %% This is the simplest test case of a multipart processing:
    %%
    %% - A single part (content generated from curl)
    %% - No filtering via callbacks
    %% - Assert the resulting form data
    %%

    Boundary = <<"------------------------65d0128e83f480f8">>,
    Data =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.txt\"\r\n"
           "Content-Type: text/plain\r\n"
           "\r\n"
           "Hi there.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Data, psycho_multipart:new(Boundary)),

    [{"msg",
      {[{"Content-Disposition",
         "form-data; name=\"msg\"; filename=\"msg.txt\""},
        {"Content-Type","text/plain"}],
       <<"Hi there.\n">>}}] = psycho_multipart:form_data(All),

    io:format("OK~n").

%% ===================================================================
%% multipart (splits)
%% ===================================================================

test_multipart_splits() ->
    io:format("multipart_splits: "),

    %% This test is uses the same data as simplest above but splits it
    %% across various divisions to show that the processing is not
    %% sensitive to how data is fed during processing.

    Boundary = <<"------------------------65d0128e83f480f8">>,
    MP = psycho_multipart:new(Boundary),

    Data =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.txt\"\r\n"
           "Content-Type: text/plain\r\n"
           "\r\n"
           "Hi there.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Data, MP),

    [{"msg",
      {[{"Content-Disposition",
         "form-data; name=\"msg\"; filename=\"msg.txt\""},
        {"Content-Type","text/plain"}],
       <<"Hi there.\n">>}}] = psycho_multipart:form_data(All),

    %% Split in the middle of the headers

    Split1 =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.t">>,
         <<"xt\"\r\n"
           "Content-Type: text/plain\r\n"
           "\r\n"
           "Hi there.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Split1, MP),

    %% Split in the middle of the header delimiter

    Split2 =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.txt\"\r\n"
           "Content-Type: text/plain\r\n">>,
         <<"\r\n"
           "Hi there.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Split2, MP),

    %% Split in the middle of the body

    Split3 =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.txt\"\r\n"
           "Content-Type: text/plain\r\n"
           "\r\n"
           "Hi th">>,
         <<"ere.\n"
           "\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Split3, MP),

    %% Split both headers and body

    Split4 =
        [<<"--------------------------65d0128e83f480f8\r\n"
           "Content-Disposition: form-data; name=\"msg\";"
           " filename=\"msg.">>,
         <<"txt\"\r\n"
           "Content-Type: text/plain\r\n">>,
         <<"\r\n"
           "Hi there.\n">>,
         <<"\r\n"
           "--------------------------65d0128e83f480f8--\r\n">>],

    All = apply_data(Split4, MP),

    io:format("OK~n").

%% ===================================================================
%% multipart (multiple)
%% ===================================================================

test_multipart_multiple() ->
    io:format("multipart_multiple: "),

    Boundary = <<"----WebKitFormBoundaryDr6DS6tqR3sKzPnI">>,
    MP = psycho_multipart:new(Boundary),

    Split =
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

    Data = [iolist_to_binary(Split)],

    All = apply_data(Split, MP),
    All = apply_data(Data, MP),

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
       <<"This\nis\nfile 2.\n">>}}] = psycho_multipart:form_data(All),

    io:format("OK~n").

%% ===================================================================
%% multipart (filtering)
%% ===================================================================

test_multipart_filtering() ->
    io:format("multipart_filtering: "),

    Boundary = <<"----WebKitFormBoundaryDr6DS6tqR3sKzPnI">>,
    Data =
        [<<"------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten"
           "t-Disposition: form-data; name=\"name\"\r\n\r\nBob\r\n"
           "------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten"
           "t-Disposition: form-data; name=\"awesome\"\r\n\r\non\r\n"
           "------WebKitFormBoundaryDr6DS6tqR3sKzPnI\r\nConten"
           "t-Disposition: form-data; name=\"file1\"; filename"
           "=\"file1\"\r\nContent-Type: application/octet-stream"
           "\r\n\r\nThis is\nfile 1.\n\r\n------WebKitFormBoundaryDr"
           "6DS6tqR3sKzPnI\r\nContent-Disposition: form-data; "
           "name=\"file2\"; filename=\"file2\"\r\nContent-Type: ap"
           "plication/octet-stream\r\n\r\nThis">>,
         %% Splits here simulates how a large file might be sent
         <<"\nis\nfile ">>,
         <<"2.\n\r\n----">>,
         <<"--WebKitFormBoundaryDr6DS6tqR3sKzPnI--\r\n">>],

    %% Handle all parts (default behavior with no callback)

    All = apply_data(Data, psycho_multipart:new(Boundary)),

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
       <<"This\nis\nfile 2.\n">>}}] = psycho_multipart:form_data(All),

     %% Use callback to drop the two files

    DropFilesHandler = drop_handler(["file1", "file2"]),
    DropFilesMP = psycho_multipart:new(Boundary, DropFilesHandler, []),
    DropFiles = apply_data(Data, DropFilesMP),

    [{"name",
      {[{"Content-Disposition","form-data; name=\"name\""}],
       <<"Bob">>}},
     {"awesome",
      {[{"Content-Disposition","form-data; name=\"awesome\""}],
       <<"on">>}}] = psycho_multipart:form_data(DropFiles),

    Events = fun(MP) -> lists:reverse(psycho_multipart:user_data(MP)) end,

    [{keep,"name"},
     {data,"name",<<"Bob">>},
     {data,"name",eof},
     {keep,"awesome"},
     {data,"awesome",<<"on">>},
     {data,"awesome",eof},
     {drop,"file1"},
     {drop,"file2"}] = Events(DropFiles),

    %% Use a callback to keep only the two files

    KeepFilesHandler = drop_handler(["name", "awesome"]),
    KeepFilesMP = psycho_multipart:new(Boundary, KeepFilesHandler, []),
    KeepFiles = apply_data(Data, KeepFilesMP),

    [{"file1",
      {[{"Content-Disposition","form-data; name=\"file1\"; "
         "filename=\"file1\""},
        {"Content-Type","application/octet-stream"}],
       <<"This is\nfile 1.\n">>}},
     {"file2",
      {[{"Content-Disposition","form-data; name=\"file2\"; "
         "filename=\"file2\""},
        {"Content-Type","application/octet-stream"}],
       <<"This\nis\nfile 2.\n">>}}] = psycho_multipart:form_data(KeepFiles),

    [{drop,"name"},
     {drop,"awesome"},
     {keep,"file1"},
     {data,"file1",<<"This is\nfile 1.\n">>},
     {data,"file1",eof},
     {keep,"file2"},
     {data,"file2",<<"This">>},
     {data,"file2",<<"\nis\nfile ">>},
     {data,"file2",<<"2.\n">>},
     {data,"file2",eof}] = Events(KeepFiles),

    %% Use a part handler to modify a part

    RenameHandler = rename_handler("awesome", "lame"),
    RenameMP = psycho_multipart:new(Boundary, RenameHandler, []),
    Rename = apply_data(Data, RenameMP),

    [{"lame",
      {[{"Content-Disposition","form-data; name=\"lame\""}],
       <<"on">>}}] = psycho_multipart:form_data(Rename),

    [{drop,"name"},
     {rename,"awesome","lame"},
     {data,"lame",<<"on">>},
     {data,"lame",eof},
     {drop,"file1"},
     {drop,"file2"}] = Events(Rename),

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

%% ===================================================================
%% dispatch on
%% ===================================================================

test_dispatch_on() ->
    io:format("dispatch_on: "),
    Env =
        [{request_method, "GET"},
         {request_path, "/foo/bar?a=1&b=2"},
         {http_headers,
          [{"Cookie", "A=1; B=2"}]}],
    D = fun(Parts) -> psycho_util:dispatch_on(Parts, Env) end,

    [Env]                      = D([env]),
    ["GET"]                    = D([method]),
    ["/foo/bar"]               = D([path]),
    [["foo", "bar"]]           = D([split_path]),
    [[{"a","1"}, {"b","2"}]]   = D([parsed_query_string]),
    [{"/foo/bar",
      "a=1&b=2",
      [{"a","1"}, {"b","2"}]}] = D([parsed_path]),
    ["a=1&b=2"]                = D([query_string]),
    [[{"A","1"}, {"B","2"}]]   = D([parsed_cookie]),
    ["Literal"]                = D(["Literal"]),
    ["GET",
     "/foo/bar",
     [{"a","1"}, {"b","2"}]]   = D([method, path, parsed_query_string]),

    io:format("OK~n").

%% ===================================================================
%% encode/decode url
%% ===================================================================

test_encode_decode_url() ->
    io:format("encode_decode_url: "),

    Encode = fun(Base, Params) ->
                     binary_to_list(
                       iolist_to_binary(
                         psycho_util:encode_url(Base, Params)))
             end,
    Decode = fun psycho_util:decode_url_part/1,

    "/foo?" = Encode("/foo", []),
    "/foo?a=A" = Encode("/foo", [{"a", "A"}]),
    "/foo?a%26b=c%7Cd" = Encode("/foo", [{"a&b", "c|d"}]),
    "/foo?a%26b=c%7Cd&e%3Df&g%2Fh"
        = Encode("/foo", [{"a&b", "c|d"}, "e=f", "g/h"]),

    "a" = Decode("a"),
    "a&b" = Decode("a%26b"),
    "c|d" = Decode("c%7Cd"),
    "e=f" = Decode("e%3Df"),
    "g/h" = Decode("g%2Fh"),

    io:format("OK~n").

%% ===================================================================
%% chain apps
%% ===================================================================

test_chain_apps() ->
    io:format("chain_apps: "),

    App = psycho_util:chain_apps(
            base_app(),
            [middleware(m1), middleware(m2), middleware(m3)]),

    [base, m1, m2, m3] = App([]),

    io:format("OK~n").

base_app() ->
    fun(Env) -> [base|Env] end.

middleware_app(Tag, Upstream) ->
    fun(Env) -> Upstream([Tag|Env]) end.

middleware(Tag) ->
    fun(Upstream) -> middleware_app(Tag, Upstream) end.
