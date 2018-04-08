%% Feel free to use, reuse and abuse the code in this file.

-module(ecache_protocol).
-behaviour(gen_server).
-behaviour(ranch_protocol).

%% API.
-export([start_link/4]).

%% gen_server.
-export([init/1]).
-export([init/4]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(K_TIMEOUT, 90000).

% ---------------------------------------------------------------------------
% @doc 进程状态
% ---------------------------------------------------------------------------
-define(STATUS_WAITING_AUTH,            'STATUS_WAITING_AUTH').         % 等待认证
-define(STATUS_AUTHED,                  'STATUS_AUTHED').               % 认证成功
-define(STATUS_WAITING_SHUTDOWN,        'STATUS_WAITING_SHUTDOWN').     % 验证失败，程序等待被杀死

-record(sock_state, {socket, transport, auth, state}).

-define(LIFE_SPAN, 6).
-define(POLL_NEXT_MESSAGE_TTL, 15000).




%% API.

start_link(Ref, Socket, Transport, Opts) ->
	proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

%% gen_server.

%% This function is never called. We only define it so that
%% we can use the -behaviour(gen_server) attribute.
init([]) -> {ok, undefined}.

init(Ref, Socket, Transport, _Opts = []) ->
    lager:debug("Ref = ~p, Socket = ~p, Transport = ~p", [Ref, Socket, Transport]),
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    case Transport of 
        ranch_tcp ->
            ok = Transport:setopts(Socket, [{active, true}, 
                                            binary, 
                                            {reuseaddr, true},
                                            {high_watermark, 131072},
                                            {low_watermark, 65536},
                                            {packet, 2}]);
        ranch_ssl ->
            ok = Transport:setopts(Socket, [{active, once}, 
                                            binary, 
                                            {packet, 2}])
    end,

    gen_server:enter_loop(?MODULE, [], #sock_state{socket = Socket, 
                                                   auth = ?STATUS_WAITING_AUTH,
                                                   transport = Transport}, ?K_TIMEOUT).

handle_info({_TcpOrSSL, Socket, Data}, #sock_state{socket = Socket, 
                                             auth = ?STATUS_WAITING_AUTH, 
                                             transport = Transport} = SockState ) ->
    Transport:setopts(Socket, [{active, once}]),
    NewSockState = 
        case check_data(Data) of
            true ->
                lager:debug("There check auth Status = ~p, Data = ~p, Socket = ~p", [?STATUS_WAITING_AUTH, Data, Socket]),
                Message = decode(Data),
                case proplists:get_value(<<"msg_type">>, Message) of
                    <<"check_auth">> ->
                        process_data(<<"check_auth">>, Socket, Transport, Message, SockState);
                    _OtherMessageType ->
                        lager:debug("There is no auth Status = ~p, Data = ~p, Socket = ~p", [?STATUS_WAITING_AUTH, Data, Socket]),
                        Transport:send(Socket, no_auth()),
                        SockState
                end;
            false ->
                lager:debug("There is no auth Status = ~p, Data = ~p, Socket = ~p", [?STATUS_WAITING_AUTH, Data, Socket]),
                 JsonError = get_parse_json_error(Data),
                 lager:debug("JsonError = ~p", [JsonError]),
                 Transport:send(Socket, encode(JsonError)),
                 SockState
        end,
    lager:debug("There is no auth Status = ~p, Data = ~p, Socket = ~p", [?STATUS_WAITING_AUTH, Data, Socket]),
    {noreply, NewSockState, ?K_TIMEOUT};
handle_info({_TcpOrSSL, Socket, Data}, #sock_state{socket = Socket, 
                                             transport = Transport} = SockState) ->
    Transport:setopts(Socket, [{active, once}]),

    Message = decode(Data),
    NewSockState = 
        case is_list(Message) of
            true ->
                MsgType = proplists:get_value(<<"msg_type">>, Message, <<>>),
                process_data(MsgType, Socket, Transport, Message, SockState);
            false ->
                JsonError = get_parse_json_error(Data),
                Transport:send(Socket, JsonError),
                SockState
        end,
    {noreply, NewSockState, ?K_TIMEOUT};
handle_info({tcp_closed, Socket}, SockState) ->
    lager:warning("tcp_closed, Socket = ~p, SockState = ~p", [Socket, SockState]),
    {stop, normal, SockState};
handle_info({tcp_error, Socket, Reason}, SockState) ->
    lager:warning("tcp_error, Reason = ~p, Socket = ~p, SockState = ~p", [Socket, Reason, SockState]),
    {stop, Reason, SockState};
handle_info(timeout, SockState) ->
    lager:warning("timeout=~p, SockState = ~p", [self(), SockState]),
    {stop, timeout, SockState};
handle_info({ok, <<>>}, SockState) ->
    {noreply, SockState, ?K_TIMEOUT};
handle_info({ok, []}, SockState) ->
    {noreply, SockState, ?K_TIMEOUT};
handle_info({ok, MessageInfo}, #sock_state{socket = Socket, 
                                           transport = Transport} = SockState) ->
    case MessageInfo of
        []->
            {noreply, SockState, ?K_TIMEOUT};
        _ ->
            MessageJson = jsx:encode(MessageInfo),
            Transport:send(Socket, MessageJson),
            {noreply, SockState, ?K_TIMEOUT}
    end;
handle_info({shutdown, Reason}, SockState) ->
    lager:warning("shut_down, Reqson = ~p, SockState = ~p", [Reason, SockState]),
    {stop, Reason, SockState};
handle_info(Info, SockState) ->
    lager:warning("Info = ~p, SockState = ~p", [Info, SockState]),
    {stop, error_message, SockState}.

handle_call(Msg, From, SockState) ->
    lager:debug("Msg = ~p, From = ~p, SockState = ~p", [Msg, From, SockState]),
    {reply, ok, SockState}.

handle_cast(Msg, SockState) ->
    lager:debug("Msg = ~p, SockState = ~p", [Msg, SockState]),
    {noreply, SockState}.

terminate(Reason, #sock_state{socket = _Socket, 
                              auth = ?STATUS_WAITING_AUTH,
                              transport = _Transport} =SockState) ->
    lager:error("Reason = ~p, SockState = ~p", [Reason, SockState]),
    ok;
terminate(Reason, #sock_state{socket = _Socket, 
                              transport = _Transport} = SockState) ->
    lager:error("Reason = ~p, SockState = ~p", [Reason, SockState]),
    ok.



code_change(OldVsn, SockState, Extra) ->
    lager:warning("OldVsn= ~p, SockState = ~p, Extra = ~p", [OldVsn, SockState, Extra]),
    {ok, SockState}.

%% Internal.

process_data(<<"check_auth">>, Socket, Transport, Message, SockState) ->
    case proplists:get_value(<<"key">>, Message) of
        <<"EFFC047E2A226960B3EF1E81992CBECD">> ->
            Return = return_data(<<"check_auth">>),
            Transport:send(Socket, Return),
            SockState#sock_state{auth = ?STATUS_AUTHED};
        _Other ->
            Transport:send(Socket, no_auth()),
            SockState
    end;

process_data(<<"execute">>, Socket, Transport, Data, SockState) ->
    lager:debug("Transport = ~p, Socket=~p, Data = ~p", [Transport, Socket, Data]),
    M = proplists:get_value(<<"m">>, Data),
    F = proplists:get_value(<<"f">>, Data),
    A = proplists:get_value(<<"a">>, Data),
    apply(M, F, A),
    Transport:send(Socket, encode(Data)),
    SockState;
process_data(MsgType, Socket, Transport, Data, SockState) ->
    lager:debug("Transport = ~p, Socket=~p, Data = ~p", [Transport, Socket, Data]),
    Transport:send(Socket, encode(Data)),
    SockState;
process_data(MsgType, Socket, Transport, Data, SockState)->
    lager:debug("Transport = ~p, Socket=~p, Data = ~p", [Transport, Socket, Data]),
    SockState.

reverse_binary(B) when is_binary(B) ->
    lager:debug("B = ~p", [B]),
    [list_to_binary(lists:reverse(binary_to_list(
            binary:part(B, {0, byte_size(B)-2})
    ))), "\r\n"];
reverse_binary(B) ->
    [B].

no_auth() ->
    Msg = [{<<"msg_type">>, <<"check_auth">>},
           {<<"result">>, <<"error">>},
           {<<"data">>, [{<<"code">>, <<"401">>},
                         {<<"desc">>, <<"no auth">>}]}],
    encode(Msg).

return_data(MsgType) ->
    Msg = [{<<"msg_type">>, MsgType},
           {<<"result">>, <<"ok">>},
           {<<"data">>, [{<<"code">>, <<"0">>},
                         {<<"desc">>, <<"authed">>}]}],
    encode(Msg).



encode(Msg) ->
    term_to_binary(Msg).

decode(Data) ->
    Message = binary_to_term(Data),
    lager:debug("Data = ~p", [Data]),
    lager:debug("Message = ~p", [Message]),
    Message.

check_data(_Data) ->
    true.

get_parse_json_error(_Data) ->
    <<"data error">>.


