%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game network information handler
%%% @end
%%% Created : 19. 十二月 2017 10:26
%%%-------------------------------------------------------------------
-module(game_handler).
-author("ninggq").

-behaviour(gen_server).

-type(handle_result(State) :: {ok, State}
| {reply, Message::any(), State}
| {stop, Reason::term(), State}).

-type(handle_reply_result(State) :: {ok, Reply::term(), State}
| {reply, Reply::term(), Message::any(), State}
| {stop, Reason::term(), Reply::term(), State}).

-type(ack_resutl(State) :: {ok, State} | {reply, Message::binary(), State} | ok).

%% 初始化进程内存
-callback(init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary(), State::any()}
| {already_started, Handler::pid()} | {already_started, Handler::pid(), Event::term()}
| {stop, Error::term()}).

%% 重连
-callback(reconnect(State::any()) -> {ok, State::any()} | {reply, Reply::binary(), State::any()}
| {stop, Error::term()}).

%% 消息处理
-callback(handle_tcp(term(), State) -> handle_result(State) when State::any()).
-callback(handle_cast(term(), State) -> handle_result(State) when State::any()).
-callback(handle_call(term(), State) -> handle_reply_result(State) when State::any()).
-callback(handle_info(term(), State) -> handle_result(State) when State::any()).

%% ping(消息已回复)
-callback(ping(Payload::binary(), State) -> ack_resutl(State) when State::any()).

%% pong(消息已回复)
-callback(pong(Payload::binary(), State) -> ack_resutl(State) when State::any()).

%% 通信结束(服务器主动断开情况下，返回消息可以发送；非主动断开，返回消息会忽略)
-callback(close(Reason::any(), State) -> 
    {ok, State} | 
    {reply, Message::binary()} | 
    {reply, Message::binary(), State} | 
    ok 
    when State::any()).

%% 服务结束
-callback(terminate(Reason::any(), State) -> ok when State::any()).

-optional_callbacks([ping/2, pong/2, close/2, terminate/2]).

%% API
-export([start_link/3, init/4]).
-export([event/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-compile({inline, [send_msg/2, disconnect/2]}).

-define(SERVER, ?MODULE).

-record(state, {
    monitor_ref = undefined :: undefined | reference(),
    shutdown = 0 :: integer(),
    socket_pid = undefined :: undefined | pid(),
    handler = undefined :: atom(),
    handler_state = #{} :: map(),
    close = false :: false | term(),
    close_timer = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================
%% 给网络服务进程发送事件消息
event(Server, Event) ->
    gen_server:cast(Server, {event, Event}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(atom(), any(), pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Handler, SocketState, ParentPid) ->
    proc_lib:start_link(?MODULE, init, [self(), Handler, SocketState, ParentPid]).

-spec(init(Starter :: pid(), Handler :: atom(), Socket :: map(), ParentPid :: pid()) -> no_return()).
init(Starter, Handler, Socket, ParentPid) ->
    {Init, Socket1} = maps:take(init, Socket),
    {Shutdown, NewSocket} = maps:take(shutdown, Socket1),
    case Handler:init(Init, self()) of
    {ok, HandleState} ->
        Ref = erlang:monitor(process, ParentPid),
        proc_lib:init_ack(Starter, {ok, self(), NewSocket}),
        gen_server:enter_loop(?MODULE, [], #state{
            monitor_ref = Ref,
            shutdown = Shutdown,
            socket_pid = ParentPid,
            handler = Handler,
            handler_state = HandleState
        });
    {reply, Reply, HandleState} ->
        Ref = erlang:monitor(process, ParentPid),
        proc_lib:init_ack(Starter, {ok, self(), NewSocket#{reply => Reply}}),
        gen_server:enter_loop(?MODULE, [], #state{
            monitor_ref = Ref,
            shutdown = Shutdown,
            socket_pid = ParentPid,
            handler = Handler,
            handler_state = HandleState
        });
    {already_started, Pid} ->
        case gen_server:call(Pid, {sys_message, {reconnect, Handler, ParentPid}}) of
            ok ->
                proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket}});
            {reply, Reply} ->
                proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket#{reply => Reply}}});
            {error, Error} ->
                proc_lib:init_ack(Starter, {error, Error})
        end;
    {stop, Reason} ->
        proc_lib:init_ack(Starter, {error, Reason})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(_) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% 系统消息处理
handle_call({sys_message, {reconnect, Handler, ParentPid}}, _From, #state{
    monitor_ref = OldRef,
    socket_pid = SocketPid,
    handler = Handler,
    close_timer = CloseTimer
} = State) ->
    %% 关闭其它连接
    case do_close(State#state{close = reconnect}) of
        {ok, TempState} ->
            CloseMsg = "";
        {reply, CloseMsg, TempState} ->
            ok
    end,
    disconnect(SocketPid, CloseMsg),
    %% 等待socket进程关闭消息
    receive
        {'DOWN', OldRef, process, _Object, _Reason} ->
            case is_reference(CloseTimer) of
                true -> erlang:cancel_timer(CloseTimer);
                false -> skip
            end,
            Ref = erlang:monitor(process, ParentPid),
            NewState = TempState#state{
                monitor_ref = Ref,
                socket_pid = ParentPid,
                close_timer = undefined
            },
            %% 重连初始化，成功重连后需要将关闭标志位置false
            case Handler:reconnect(NewState#state.handler_state) of
                {ok, HandleState} ->
                    {reply, ok, NewState#state{handler_state = HandleState, close = false}};
                {reply, Reply, HandleState} ->
                    {reply, {reply, Reply}, NewState#state{handler_state = HandleState, close = false}};
                {stop, Reason} ->
                    {reply, {error, Reason}, NewState}
            end
    after 3000 ->
        %% 超时没收到socket进程关闭消息，可能已经关闭，但是一样当成失败处理
        {reply, {error, timeout}, TempState}
    end;
handle_call({sys_message, {reconnect, _Handler, _ParentPid}}, _From, State) ->
    %% 处理模块名与初始化模块名不一致
    {reply, {error, fail}, State};

%% 强制关闭进程
handle_call({sys_message, {soft_stop_immediately, SupPid}}, From, #state{socket_pid = SocketPid} = State) ->
    gen_server:reply(From, ok),
    case do_close(State#state{close = {shutdown, SupPid}}) of
        {ok, NewState} ->
            CloseMsg = "";
        {reply, CloseMsg, NewState} ->
            ok
    end,
    disconnect(SocketPid, CloseMsg),
    {stop, normal, NewState};

handle_call(Msg, _From, #state{
    socket_pid = SocketPid, handler = Handler, handler_state = HandlerState,
    shutdown = Shutdown, close_timer = Old
} = State) ->
    case Handler:handle_call(Msg, HandlerState) of
        {ok, Reply, NewHandlerState} ->
            {reply, Reply, State#state{handler_state = NewHandlerState}};
        {reply, Message, Reply, NewHandlerState} ->
            send_msg(SocketPid, Message),
            {reply, Reply, State#state{handler_state = NewHandlerState}};
        {stop, Reason, Reply, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState, close = Reason},
            case do_close(NewState) of
                {ok, CloseState} ->
                    ok;
                {reply, SendMsg, CloseState} ->
                    send_msg(SocketPid, SendMsg)
            end,
            case proc_stop(Shutdown, Old) of
                stop ->
                    {stop, normal, Reply, CloseState#state{close_timer = undefined}};
                ok ->
                    {reply, Reply, CloseState};
                {ok, CloseTimer} ->
                    {reply, Reply, CloseState#state{close_timer = CloseTimer}}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% 网络信息通信处理
handle_cast({net_message, {ping, Msg}}, State) ->
    ack(ping, Msg, State);
handle_cast({net_message, {pong, Msg}}, State) ->
    ack(pong, Msg, State);
handle_cast({net_message, {tcp, Msg}}, #state{
    socket_pid = SocketPid, handler = Handler, handler_state = HandlerState,
    shutdown = Shutdown, close_timer = Old
} = State) ->
    case Handler:handle_tcp(Msg, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{handler_state = NewHandlerState}};
        {reply, Message, NewHandlerState} ->
            send_msg(SocketPid, Message),
            {noreply, State#state{handler_state = NewHandlerState}};
        {stop, Reason, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState, close = Reason},
            case do_close(NewState) of
                {ok, CloseState} ->
                    ok;
                {reply, SendMsg, CloseState} ->
                    send_msg(SocketPid, SendMsg)
            end,
            case proc_stop(Shutdown, Old) of
                stop ->
                    {stop, normal, CloseState#state{close_timer = undefined}};
                ok ->
                    {noreply, CloseState};
                {ok, CloseTimer} ->
                    {noreply, CloseState#state{close_timer = CloseTimer}}
            end
    end;
%% 转发网络进程消息
handle_cast({event, _Msg}, #state{socket_pid = undefined} = State) ->
    %% socket进程已经关闭
    {noreply, State};
handle_cast({event, Msg}, #state{socket_pid = SocketPid} = State) ->
    erlang:send(SocketPid, Msg),
    {noreply, State};
%% 系统消息
handle_cast(Msg, #state{
    socket_pid = SocketPid, handler = Handler, handler_state = HandlerState,
    shutdown = Shutdown, close_timer = Old
} = State) ->
    case Handler:handle_cast(Msg, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{handler_state = NewHandlerState}};
        {reply, Message, NewHandlerState} ->
            send_msg(SocketPid, Message),
            {noreply, State#state{handler_state = NewHandlerState}};
        {stop, Reason, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState, close = Reason},
            case do_close(NewState) of
                {ok, CloseState} ->
                    ok;
                {reply, SendMsg, CloseState} ->
                    send_msg(SocketPid, SendMsg)
            end,
            case proc_stop(Shutdown, Old) of
                stop ->
                    {stop, normal, CloseState#state{close_timer = undefined}};
                ok ->
                    {noreply, CloseState};
                {ok, CloseTimer} ->
                    {noreply, CloseState#state{close_timer = CloseTimer}}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({'DOWN', Ref, process, _Object, Reason}, #state{
    monitor_ref = Ref, shutdown = Shutdown, close_timer = Old
} = State) ->
    %% 通信进程主动关闭，关闭消息不用返回
    case do_close(State#state{close = Reason}) of
        {ok, NewState} -> ok;
        {reply, _, NewState} -> ok
    end,
    case proc_stop(Shutdown, Old) of
        stop ->
            {stop, normal, NewState#state{close_timer = undefined}};
        ok ->
            {noreply, NewState};
        {ok, CloseTimer} ->
            {noreply, NewState#state{close_timer = CloseTimer}}
    end;
handle_info({timeout, TimerRef, stop}, #state{close_timer = TimerRef} = State) ->
    {stop, normal, State#state{close_timer = undefined}};
handle_info(Info, #state{
    socket_pid = SocketPid, handler = Handler, handler_state = HandlerState,
    shutdown = Shutdown, close_timer = Old
} = State) ->
    case Handler:handle_info(Info, HandlerState) of
        {ok, NewHandlerState} ->
            {noreply, State#state{handler_state = NewHandlerState}};
        {reply, Message, NewHandlerState} ->
            send_msg(SocketPid, Message),
            {noreply, State#state{handler_state = NewHandlerState}};
        {stop, Reason, NewHandlerState} ->
            NewState = State#state{handler_state = NewHandlerState, close = Reason},
            case do_close(NewState) of
                {ok, CloseState} ->
                    ok;
                {reply, SendMsg, CloseState} ->
                    send_msg(SocketPid, SendMsg)
            end,
            case proc_stop(Shutdown, Old) of
                stop ->
                    {stop, normal, CloseState#state{close_timer = undefined}};
                ok ->
                    {noreply, CloseState};
                {ok, CloseTimer} ->
                    {noreply, CloseState#state{close_timer = CloseTimer}}
            end
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, #state{
    socket_pid = SocketPid,
    handler = Handler,
    handler_state = State,
    close = Reason
} = State) ->
    %% 保证通信进程断开
    disconnect(SocketPid, ""),
    %% 先发送断线消息，再处理进程内容
    case erlang:function_exported(Handler, terminate, 2) of
        true ->
            Handler:terminate(Reason, State);
        false ->
            ok
    end,
    case Reason of
        {shutdown, Pid} ->
            %% 强制关闭需要回复消息
            Pid ! ok,
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% 关闭进程
proc_stop(0, CloseTimer) ->
    case is_reference(CloseTimer) of
        true -> erlang:cancel_timer(CloseTimer);
        false -> skip
    end,
    stop;
proc_stop(Shutdown, CloseTimer) ->
    case is_reference(CloseTimer) andalso (false =/= erlang:read_timer(CloseTimer)) of
        true -> 
            ok;
        false ->
            {ok, erlang:start_timer(Shutdown, self(), stop)}
    end.

do_close(#state{handler = Handler, handler_state = HandlerState, close = Reason} = State) ->
    case erlang:function_exported(Handler, close, 2) of
        true ->
            case Handler:close(Reason, HandlerState) of
                {ok, NewHandlerState} ->
                    {ok, State#state{handler_state = NewHandlerState}};
                {reply, ReplyMsg} ->
                    {reply, ReplyMsg, State};
                {reply, ReplyMsg, NewHandlerState} ->
                    {reply, ReplyMsg, State#state{handler_state = NewHandlerState}};
                ok ->
                    {ok, State}
            end;
        false ->
            {ok, State}
    end.

ack(Fun, Msg, #state{handler = Handler, handler_state = HandlerState} = State) ->
    case erlang:function_exported(Handler, Fun, 2) of
        true ->
            case Handler:Fun(Msg, HandlerState) of
                {ok, NewHandlerState} ->
                    {reply, ok, State#state{handler_state = NewHandlerState}};
                {reply, Message, NewHandlerState} ->
                    ReplyMsg = Handler:serialize(Message),
                    {reply, {reply, ReplyMsg}, State#state{handler_state = NewHandlerState}};
                ok ->
                    {reply, ok, State}
            end;
        false ->
            {reply, ok, State}
    end.

send_msg(SocketPid, Message) ->
    erlang:send(SocketPid, {"send_msg", Message}).

disconnect(SocketPid, Message) ->
    erlang:send(SocketPid, {"disconnect", Message}).