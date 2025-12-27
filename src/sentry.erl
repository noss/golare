-module(sentry).

-export([get_request/0]).
-export([set_request/1]).
-export([add_request/1]).
-export([add_request_data/1]).
-export([add_request_env/1]).
-export([clear_request/0]).

-export([get_user/0]).
-export([set_user/1]).
-export([add_user/1]).
-export([clear_user/0]).

-export([get_tags/0]).
-export([set_tags/1]).
-export([add_tags/1]).
-export([add_tag/2]).
-export([clear_tags/0]).

-export([clear/0]).

-type request_method() :: atom() | binary().
-type request_url() :: binary().
-type request_qs() :: binary().
-type request_data() :: #{atom() | binary() => binary()}.
-type request_cookies() ::
    binary() | [{atom() | binary(), binary()}] | #{atom() | binary() => binary()}.
-type request_headers() :: #{atom() | binary() => binary()}.
-type request_env() :: #{atom() | binary() => binary()}.

-type request_context() :: #{
    method => request_method(),
    url => request_url(),
    qs => request_qs(),
    data => request_data(),
    cookies => request_cookies(),
    headers => request_headers(),
    env => request_env()
}.

-type user_id() :: binary().
-type user_name() :: binary().
-type user_email() :: binary().
-type user_ip_address() :: binary().

-type user_context() :: #{
    id => user_id(),
    username => user_name(),
    email => user_email(),
    ip => user_ip_address()
}.

-type tag_context() :: #{binary() | atom() => binary()}.

-export_type([request_context/0, request_data/0, request_env/0]).
-export_type([user_context/0]).
-export_type([tag_context/0]).

-spec get_request() -> request_context() | undefined.
get_request() ->
    erlang:get(pdict_key(request)).

-spec set_request(request_context()) -> ok.
set_request(ReqContext) ->
    erlang:put(pdict_key(request), ReqContext),
    ok.

-spec add_request(request_context()) -> ok.
add_request(ReqContext) ->
    case get_request() of
        undefined ->
            set_request(ReqContext);
        PrevReqContext ->
            set_request(maps:merge(PrevReqContext, ReqContext))
    end.

-spec add_request_data(request_data()) -> ok.
add_request_data(ReqData) ->
    case get_request() of
        undefined ->
            set_request(#{data => ReqData});
        PrevReqContext ->
            PrevData = maps:get(data, PrevReqContext, #{}),
            set_request(PrevReqContext#{data := maps:merge(PrevData, ReqData)})
    end.

-spec add_request_env(request_env()) -> ok.
add_request_env(ReqEnv) ->
    case get_request() of
        undefined ->
            set_request(#{env => ReqEnv});
        PrevReqContext ->
            PrevEnv = maps:get(env, PrevReqContext, #{}),
            set_request(PrevReqContext#{env := maps:merge(PrevEnv, ReqEnv)})
    end.

-spec clear_request() -> request_context() | undefined.
clear_request() ->
    erlang:erase(pdict_key(request)).

-spec get_user() -> user_context() | undefined.
get_user() ->
    erlang:get(pdict_key(user)).

-spec set_user(user_context()) -> ok.
set_user(UserContext) ->
    erlang:put(pdict_key(user), UserContext),
    ok.

-spec add_user(user_context()) -> ok.
add_user(UserContext) ->
    case get_user() of
        undefined ->
            set_user(UserContext);
        PrevUser ->
            set_user(maps:merge(PrevUser, UserContext))
    end.

-spec clear_user() -> user_context() | undefined.
clear_user() ->
    erlang:erase(pdict_key(user)).

-spec get_tags() -> tag_context() | undefined.
get_tags() ->
    erlang:get(pdict_key(tags)).

-spec set_tags(tag_context()) -> ok.
set_tags(Tags) ->
    erlang:put(pdict_key(tags), Tags),
    ok.

-spec add_tags(tag_context()) -> ok.
add_tags(Tags) ->
    case get_tags() of
        undefined ->
            set_tags(Tags);
        PrevTags ->
            set_tags(maps:merge(PrevTags, Tags))
    end.

-spec add_tag(atom(), binary()) -> ok.
add_tag(Key, Value) ->
    case get_tags() of
        undefined ->
            set_tags(#{Key => Value});
        PrevTags ->
            set_tags(PrevTags#{Key => Value})
    end.

-spec clear_tags() -> tag_context() | undefined.
clear_tags() ->
    erlang:erase(pdict_key(tags)).

-spec clear() -> ok.
clear() ->
    clear_request(),
    clear_user(),
    clear_tags(),
    ok.

pdict_key(tags) -> sentry_tags;
pdict_key(user) -> sentry_user;
pdict_key(request) -> sentry_request.
