-module(ecache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(lager),
    application:ensure_all_started(ranch),
    Opts = [{active, true}, 
    %Opts = [{active, false}, 
    %Opts = [{active, once}, 
            binary, 
            {reuseaddr, true},
            {high_watermark, 131072},
            {low_watermark, 65536},
            {packet, 2}],
    {ok, _} = ranch:start_listener(ecache, 10,
                ranch_tcp, [{port, 5554}, {max_connections, infinity}], ecache_protocol, Opts),
    ecache_sup:start_link().



stop(_State) ->
    ok.
