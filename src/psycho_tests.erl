-module(psycho_tests).

-export([run/0]).

run() ->
    test_parse_request_path(),
    test_ensure_parsed_request_path(),
    test_routes(),
    test_crypto(),
    test_validate().

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
