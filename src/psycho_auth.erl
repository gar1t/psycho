-module(psycho_auth).

-export([basic_auth_app/3, basic_auth_app/4]).

-record(bauth, {realm, users, next_app, not_auth_app}).

basic_auth_app(Realm, Users, NextApp) ->
    basic_auth_app(Realm, Users, NextApp, not_authorized_app(Realm)).

not_authorized_app(Realm) ->
    fun(_Env) -> not_authorized(Realm) end.

not_authorized(Realm) ->
    Headers =
        [{"WWW-Authenticate", ["Basic realm=", Realm]},
         {"Content-Type", "text/plain"}],
    {{401, "Not Authorized"}, Headers, "Not Authorized\n"}.


basic_auth_app(Realm, Users, NextApp, NotAuthApp) ->
    BAuth = #bauth{realm=Realm,
                   users=Users,
                   next_app=NextApp,
                   not_auth_app=NotAuthApp},
    fun(Env) -> basic_auth(BAuth, Env) end.

basic_auth(BAuth, Env) ->
    AuthResult = basic_authenticate(basic_creds(Env), BAuth),
    handle_basic_authenticate(AuthResult, BAuth, Env).

basic_creds(Env) ->
    basic_creds_from_auth_header(psycho:env_header("Authorization", Env)).

basic_creds_from_auth_header("Basic " ++ Base64Encoded) ->
    parse_decoded_auth_header(base64:decode(Base64Encoded));
basic_creds_from_auth_header(undefined) -> 
    undefined.

parse_decoded_auth_header(Header) ->
    [User|PwdParts] = binary:split(Header, <<":">>),
    {User, iolist_to_binary(PwdParts)}.

basic_authenticate(undefined, _) -> fail;
basic_authenticate({User, Pwd}, #bauth{users=Users}) ->
    case lists:keyfind(User, 1, Users) of
        {_, Pwd} -> {pass, User};
        _ -> fail
    end.

handle_basic_authenticate(fail, BAuth, Env) ->
    handle_basic_authenticate_fail(BAuth, Env);
handle_basic_authenticate({pass, User}, BAuth, Env) ->
    handle_basic_authenticate_pass(User, BAuth, Env).

handle_basic_authenticate_fail(#bauth{not_auth_app=NotAuth}, Env) ->
    NotAuth(Env).

handle_basic_authenticate_pass(User, #bauth{next_app=NextApp}, Env) ->
    psycho:call_app(NextApp, [{remote_user, User}|Env]).
