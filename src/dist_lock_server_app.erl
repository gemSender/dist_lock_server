-module(dist_lock_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    global:sync(),
    {ok, _} = ranch:start_listener(tcp_echo, 100,
        ranch_tcp, [{port, 5555}],
        conn_handler, []
    ),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", dls_ws_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 5556}],
        [{env, [{dispatch, Dispatch}]}]),
    dist_lock_server_sup:start_link().

stop(_State) ->
    ok.
