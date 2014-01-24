-module(sample_middleware).

-export([header_footer/3,
         basic_auth/4,
         cookie_auth/2, user_cookie/1, clear_user_cookie/0]).

%%%===================================================================
%%% Header / Footer
%%%===================================================================

header_footer(Header, Footer, App) ->
    fun(Env) -> header_footer_app(Header, Footer, App, Env) end.

header_footer_app(Header, Footer, App, Env) ->
    handle_header_footer_app(psycho:call_app(App, Env), Header, Footer).

handle_header_footer_app({{200, "OK"}, Headers, Body}, Header, Footer) ->
    {{200, "OK"}, Headers, [Header, Body, Footer]};
handle_header_footer_app(AppResp, _Header, _Footer) ->
    AppResp.

%%%===================================================================
%%% Basic Authentication
%%%===================================================================

basic_auth(Realm, User, Password, App) ->
    UserBin = iolist_to_binary(User),
    PasswordBin = iolist_to_binary(Password),
    fun(Env) -> basic_auth_app(Realm, UserBin, PasswordBin, App, Env) end.

basic_auth_app(Realm, User, Password, App, Env) ->
    Creds = basic_credentials(Env),
    CheckResult = check_basic_creds(Creds, User, Password),
    handle_basic_auth_request(CheckResult, App, Env, Realm).

basic_credentials(Env) ->
    basic_creds_from_auth_header(psycho:env_header("Authorization", Env)).

basic_creds_from_auth_header("Basic " ++ Base64Encoded) ->
    parse_decoded_auth_header(base64:decode(Base64Encoded));
basic_creds_from_auth_header(undefined) -> undefined.

parse_decoded_auth_header(Header) ->
    [User|PwdParts] = binary:split(Header, <<":">>),
    {User, iolist_to_binary(PwdParts)}.

check_basic_creds({User, Password}, User, Password) -> {pass, User};
check_basic_creds(_Creds, _User, _Password) -> fail.

handle_basic_auth_request({pass, User}, App, Env, _Realm) ->
    handle_basic_authorized(User, App, Env);
handle_basic_auth_request(fail, _App, _Env, Realm) ->
    handle_basic_unauthorized(Realm).

handle_basic_authorized(User, App, Env) ->
    psycho:call_app(App, add_user_to_env(User, Env)).

add_user_to_env(User, Env) ->
    [{remote_user, User}|Env].

handle_basic_unauthorized(Realm) ->
    Headers =
        [{"WWW-Authenticate", ["Basic realm=", Realm]},
         {"Content-Type", "text/plain"}],
    Msg = "Not Authorized",
    {{401, "Not Authorized"}, Headers, Msg}.

%%%===================================================================
%%% Cookie Authentication
%%%===================================================================

-define(USER_KEY, <<1,2,3,4,5,6,7,8>>).

cookie_auth(Form, App) ->
    fun(Env) -> cookie_auth_app(Form, App, Env) end.

cookie_auth_app(Form, App, Env) ->
    handle_cookie_user(cookie_user(Env), Form, App).

cookie_user(Env0) ->
    {Cookie, Env} = psycho_util:ensure_parsed_cookie(Env0),
    User = try_decrypt_user_cookie(user_cookie_val(Cookie)),
    {User, Env}.

user_cookie_val(Cookie) ->
    proplists:get_value("user", Cookie).

try_decrypt_user_cookie(undefined) -> undefined;
try_decrypt_user_cookie(Encoded) when is_list(Encoded) ->
    try_decrypt_user_cookie(try_base64_decode(Encoded));
try_decrypt_user_cookie(UserBin) when is_binary(UserBin) ->
    handle_user_decrypt(psycho_util:decrypt(UserBin, ?USER_KEY)).

try_base64_decode(Encoded) ->
    handle_base64_decode(catch base64:decode(Encoded)).

handle_base64_decode({'EXIT', _}) -> undefined;
handle_base64_decode(Decoded) -> Decoded.

handle_user_decrypt({ok, UserBin}) -> binary_to_list(UserBin);
handle_user_decrypt(error) -> undefined.

handle_cookie_user({undefined, Env}, Form, _App) ->
    Form(Env);
handle_cookie_user({User, Env}, _Form, App) ->
    App([{remote_user, User}|Env]).

user_cookie(User) ->
    psycho_util:cookie_header("user", encrypt_user(User)).

encrypt_user(User) ->
    UserBin = list_to_binary(User),
    Encrypted = psycho_util:encrypt(UserBin, ?USER_KEY),
    base64:encode(Encrypted).

clear_user_cookie() ->
    {"Set-Cookie", "user=;"}.
