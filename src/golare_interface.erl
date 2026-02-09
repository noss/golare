-module(golare_interface).

-export([exception/3]).
-export([exception/4]).
-export([message/1]).
-export([message/2]).
-export([request_cowboy/1]).
-export([thread/0]).
-export([thread/1]).
-export([user/1]).
-export([context/2]).
-export([extra/1]).

% Types
-export_type([t/0]).

-type t() :: #{atom() => term()}.

% API

-doc #{equiv => exception(Class, Reason, Stacktrace, #{})}.
exception(Class, Reason, Stacktrace) ->
    exception(Class, Reason, Stacktrace, #{}).

-doc """
Build an [Exception interface](https://develop.sentry.dev/sdk/data-model/event-payloads/exception/).

Captures the exception class, reason, and stacktrace. `Opts` may contain:
- `mechanism` - the capture mechanism type (default `auto`)
- `handled` - whether the exception was caught (default `false`)
- `data` - arbitrary metadata attached to the mechanism
- `thread_id` - pid of the thread where the exception occurred
""".
exception(Class, Reason, Stacktrace, Opts) when is_atom(Class) ->
    #{
        exception => [
            maps:merge(
                #{
                    type => Class,
                    value => format_term(Reason),
                    mechanism => #{
                        type => maps:get(mechanism, Opts, auto),
                        handled => maps:get(handled, Opts, false),
                        data => safe_json_map(maps:get(data, Opts, #{}))
                    },
                    stacktrace => exception_stacktrace(Stacktrace)
                },
                #{thread_id => format_term(Pid) || thread_id := Pid <- Opts}
            )
        ]
    }.

-doc """
Build a [Message interface](https://develop.sentry.dev/sdk/data-model/event-payloads/message/)
from a pre-formatted message.
""".
message(Message) ->
    #{
        logentry => #{
            formatted => iolist_to_binary(Message)
        }
    }.

-doc """
Build a [Message interface](https://develop.sentry.dev/sdk/data-model/event-payloads/message/)
from a format string and parameters.

The raw `Format` string is preserved for grouping, and `Data` terms are
formatted as strings for the `params` list.
""".
message(Format, Data) ->
    #{
        logentry => #{
            formatted => iolist_to_binary(io_lib:format(Format, Data)),
            message => iolist_to_binary(Format),
            params => [format_term(D) || D <- Data]
        }
    }.

-doc """
Build a [Request interface](https://develop.sentry.dev/sdk/data-model/event-payloads/request/)
from a Cowboy request object.

Extracts method, query string, headers, URL, cookies, and remote address.
""".
request_cowboy(Req0) ->
    {RemoteAddr, _Port} = cowboy_req:peer(Req0),
    Scope = #{
        method => cowboy_req:method(Req0),
        query_string => cowboy_req:qs(Req0),
        headers => cowboy_req:headers(Req0),
        env => #{
            ~"REMOTE_ADDR" => iolist_to_binary(inet:ntoa(RemoteAddr))
        }
    },
    {FinalScope, _Req} = lists:foldl(fun(F, Acc) -> F(Acc) end, {Scope, Req0}, [
        fun request_cowboy_cookies/1,
        fun request_cowboy_url/1
    ]),
    #{request => FinalScope}.

-doc #{equiv => thread(#{pid => self()})}.
thread() -> thread(#{pid => self()}).

-doc """
Build a [Threads interface](https://develop.sentry.dev/sdk/data-model/event-payloads/threads/)
for a single thread.

Includes the pid as thread id, and the registered name if the process has one.
Threads are always marked as `current` since Erlang does not have foreground or
background processes.
""".
thread(#{pid := Pid}) when is_pid(Pid) ->
    Named =
        case process_info(Pid, registered_name) of
            {registered_name, Name} -> #{name => Name};
            [] -> #{}
        end,
    #{
        threads => [
            Named#{
                id => format_term(Pid),
                current => true
            }
        ]
    }.

-doc """
Build a [User interface](https://develop.sentry.dev/sdk/data-model/event-payloads/user/).

`Data` must contain at least one field such as `id`, `email`, `ip_address`, or
`username`.
""".
user(Data) when map_size(Data) >= 1 ->
    #{user => safe_json_map(Data)}.

-doc """
Build a [Response context](https://develop.sentry.dev/sdk/data-model/event-payloads/contexts/#response-context)
from a Cowboy request and response metadata.

Extracts the response headers. Status code must be explicitly set as
`status_code` in the context.
""".
context(response_cowboy, {Req, Context}) when is_map(Context) ->
    #{
        contexts => #{
            response => #{
                status_code => maps:get(status_code, Context, null),
                headers => cowboy_req:resp_headers(Req)
            }
        }
    }.

-doc """
Build an [Extra Data](https://develop.sentry.dev/sdk/data-model/event-payloads/#optional-attributes)
interface from an arbitrary map.

Values that are not JSON-encodable are formatted as Erlang terms.
""".
extra(Extra) -> #{extra => safe_json_map(Extra)}.

% Internal

exception_stacktrace(Stacktrace) ->
    #{
        frames => lists:reverse(lists:map(fun exception_stacktrace_frame/1, Stacktrace))
    }.

exception_stacktrace_frame({Module, Function, Args, Meta}) ->
    Arity =
        case Args of
            A when is_integer(A) -> A;
            A when is_list(A) -> length(A)
        end,
    File = proplists:get_value(file, Meta),
    #{
        in_app =>
            case File of
                "/" ++ _ -> true;
                _ -> false
            end,
        function => iolist_to_binary(io_lib:format("~p/~p", [Function, Arity])),
        filename => Module,
        abs_path => iolist_to_binary(File),
        lineno => proplists:get_value(line, Meta)
    }.

request_cowboy_url({Scope, Req0}) ->
    URL = uri_string:recompose(#{
        scheme => cowboy_req:scheme(Req0),
        host => cowboy_req:host(Req0),
        port => cowboy_req:port(Req0),
        path => cowboy_req:path(Req0)
    }),
    {Scope#{url => URL}, Req0}.

request_cowboy_cookies({Scope, Req0} = Acc) ->
    case cowboy_req:parse_cookies(Req0) of
        [] -> Acc;
        Cookies -> {Scope#{cookies => maps:from_list(Cookies)}, Req0}
    end.

format_term(Term) -> iolist_to_binary(io_lib:format("~p", [Term])).

safe_json_map(Map) ->
    Encoder = fun(Value, Encode) ->
        try
            json:encode_value(Value, Encode)
        catch
            error:{unsupported_type, Value} ->
                json:encode(format_term(Value))
        end
    end,
    json:decode(iolist_to_binary(json:encode(Map, Encoder))).
