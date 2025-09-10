%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Sentry Mock callbacks
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(sentry_mock_callbacks).
-behavior(cowboy_rest).

-export([init/2]).
-export([terminate/3]).

terminate(_Rsn, _Req, _S) ->
    ok.

init(Req0, S) ->
    <<"POST">> = cowboy_req:method(Req0),
    {ok, Payload, Req1} = cowboy_req:read_body(Req0),
    Headers = #{<<"content-type">> => <<"application/json">>},
    ct:pal(info, "Event posted:~n~s", [Payload]),
    try json:decode_start(Payload, ok, #{}) of
        {#{<<"event_id">> := EventId}, ok, Rest} ->
            Item = {_ItemHeader, _ItemPayload} = read_items(Rest),
            sentry_mock_server:gossip(uuid:string_to_uuid(EventId), Item),
            Req = cowboy_req:reply(200, Headers, json:encode(#{id => EventId}), Req1),
            {ok, Req, S}
    catch
        error:_ ->
            ct:pal(error, "Couldnt decode body~n~p", [Payload]),
            Req = cowboy_req:reply(500, Headers, <<"[]">>, Req1),
            {ok, Req, S}
    end.

read_items(RawItemHeader) ->
    case json:decode_start(RawItemHeader, event, #{}) of
        {#{<<"type">> := <<"event">>, <<"length">> := Len} = ItemHeader, event, RawItems} ->
            <<$\n, RawItem:Len/binary, _/binary>> = RawItems,
            {ItemHeader, json:decode(RawItem)}
    end.
