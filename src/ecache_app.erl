-module(ecache_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:ensure_all_started(ranch),
    {ok, _} = ranch:start_listener(ecache,
		ranch_tcp, [{port, 5554}], ecache_protocol, []),
    ecache_sup:start_link().



stop(_State) ->
    ok.
