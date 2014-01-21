-module(sample_middleware).

-export([header_footer/3, basic_auth/4]).

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
    CredsCheckResult = check_credentials(Creds, User, Password),
    handle_auth_request(CredsCheckResult, App, Env, Realm).

basic_credentials(Env) ->
    basic_creds_from_auth_header(psycho:env_header("Authorization", Env)).

basic_creds_from_auth_header("Basic " ++ Base64Encoded) ->
    parse_decoded_auth_header(base64:decode(Base64Encoded));
basic_creds_from_auth_header(undefined) -> undefined.

parse_decoded_auth_header(Header) ->
    [User|PwdParts] = binary:split(Header, <<":">>),
    {User, iolist_to_binary(PwdParts)}.

check_credentials({User, Password}, User, Password) -> {pass, User};
check_credentials(_Creds, _User, _Password) -> fail.

handle_auth_request({pass, User}, App, Env, _Realm) ->
    handle_authorized_request(User, App, Env);
handle_auth_request(fail, _App, _Env, Realm) ->
    handle_unauthorized_request(Realm).

handle_authorized_request(User, App, Env) ->
    psycho:call_app(App, add_user_to_env(User, Env)).

add_user_to_env(User, Env) ->
    [{remote_user, User}|Env].

handle_unauthorized_request(Realm) ->    
    Headers =
        [{"WWW-Authenticate", ["Basic realm=", Realm]},
         {"Content-Type", "text/plain"}],
    Msg = "Not Authorized",
    {{401, "Not Authorized"}, Headers, Msg}.
