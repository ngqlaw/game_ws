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

-type (call_result(State) :: {ok, State}
| {reply, binary(), State}
| {stop, State}).

%% 初始化进程内存
-callback (init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary()}
| {error, {already_started, pid()} | term()}).

%% 调用game_ws:control/2时候产生的消息在这里处理
-callback (control(term(), State) -> call_result(State) when State::any()).

%% 通信消息处理
-callback (handle(binary(), State) -> call_result(State) when State::any()).

%% 结束通信
-callback (terminate(Reason::any(), State) -> ok when State::any()).
-optional_callbacks([terminate/2]).

%% API
-export([start_link/2, init/3]).
-export([handle_inside/2, handle_outside/3, handle_control/3, stop/4]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).
-define(FLAG_STOP, -1).

-record(state, {
  monitor_ref = undefined :: reference(),
  close_arg = undefined :: any(),
  flag = 0 :: any(),
  socket_pid = undefined :: undefined | pid(),
  handler_state = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================
%% 给网络服务进程发送事件消息
handle_inside(Server, Event) when Server == self() ->
  inside(Event);
handle_inside(Server, Event) ->
  gen_server:call(Server, {handle_inside, Event}).

%% 处理通信事件消息
handle_outside(Server, Handler, Msg) ->  
  gen_server:call(Server, {handle_outside, Handler, Msg}).

%% 自定义消息
handle_control(Server, Handler, Msg) ->  
  gen_server:call(Server, {handle_control, Handler, Msg}).  

%% 停止服务
stop(Server, Handler, Shutdown, Reason) ->
  gen_server:call(Server, {stop, Handler, Shutdown, Reason}).  

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(any(), pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SocketState, ParentPid) ->
  proc_lib:start_link(?MODULE, init, [self(), SocketState, ParentPid]).

-spec(init(ParentPid :: pid(), Handler :: map(), SocketPid :: pid()) -> no_return()).
init(ParentPid, #{handler := Handler} = Socket, SocketPid) ->
  erlang:put(socket_pid, {ok, SocketPid}),
  {Init, NewSocket} = maps:take(init, Socket),
  case Handler:init(Init, self()) of
    {ok, HandleState} ->
      Ref = erlang:monitor(process, ParentPid),
      proc_lib:init_ack(ParentPid, {ok, self(), NewSocket}),
      gen_server:enter_loop(?MODULE, [], #state{
        monitor_ref = Ref,
        socket_pid = SocketPid,
        handler_state = HandleState
      });
    {reply, Reply, HandleState} ->
      Ref = erlang:monitor(process, ParentPid),
      proc_lib:init_ack(ParentPid, {ok, self(), NewSocket#{reply => Reply}}),
      gen_server:enter_loop(?MODULE, [], #state{
        monitor_ref = Ref,
        socket_pid = SocketPid,
        handler_state = HandleState
      });
    {error, {already_started, Pid}} ->
      case gen_server:call(Pid, {reconnect, Handler, ParentPid}) of
        ok ->
          proc_lib:init_ack(ParentPid, {error, {already_started, Pid, NewSocket}});
        {ok, Reply} ->
          proc_lib:init_ack(ParentPid, {error, {already_started, Pid, NewSocket#{reply => Reply}}});
        stop ->
          proc_lib:init_ack(ParentPid, {error, stop})
      end;
    {error, Reason} ->
      proc_lib:init_ack(ParentPid, {error, Reason})
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
handle_call({reconnect, Handler, SocketPid}, From, State) ->
  Ref = erlang:monitor(process, SocketPid),
  handle_call({handle_control, Handler, reconnect}, From, State#state{
    monitor_ref = Ref,
    socket_pid = SocketPid
  });
handle_call({handle_control, Handler, Event}, _From, #state{handler_state = HandleState} = State) ->
  case Handler:control(Event, HandleState) of
    {ok, NewHandleState} ->
      {reply, ok, State#state{handler_state = NewHandleState}};
    {reply, Reply, NewHandleState} ->
      {reply, {ok, Reply}, State#state{handler_state = NewHandleState}};
    {stop, NewHandleState} ->
      {reply, stop, State#state{handler_state = NewHandleState}}
  end;
handle_call({handle_outside, Handler, ping}, From, State) ->
  handle_call({handle_control, Handler, ping}, From, State);
handle_call({handle_outside, Handler, {ping, Msg}}, _From, State) ->
  handle_call({handle_control, Handler, ping}, {ping, Msg}, State);
handle_call({handle_outside, Handler, {_, Msg}}, _From, #state{handler_state = HandleState} = State) ->
  case Handler:handle(Msg, HandleState) of
    {ok, NewHandleState} ->
      {reply, ok, State#state{handler_state = NewHandleState}};
    {reply, Reply, NewHandleState} ->
      {reply, {ok, Reply}, State#state{handler_state = NewHandleState}};
    {stop, NewHandleState} ->
      {reply, stop, State#state{handler_state = NewHandleState}}    
  end;
handle_call({handle_inside, Msg}, _From, #state{} = State) ->
  Reply = inside(Msg),
  {reply, Reply, State};  
handle_call({stop, Handler, Shutdown, Reason}, _From, #state{close_arg = Old, flag = Flag} = State) ->
  Timeout = case is_integer(Shutdown) of
    true -> max(0, Shutdown);
    false -> 1000
  end,
  case Old of
    {OldTimer, _, _} when is_reference(OldTimer) -> 
      erlang:cancel_timer(Old);
    _ -> 
      skip
  end,
  case Timeout == 0 orelse Flag == ?FLAG_STOP of
    true ->
      do_stop(State#state{close_arg = {undefined, Handler, Reason}}),
      {stop, Reason, ok, State};
    false ->  
      CloseRef = erlang:send_after(Timeout, self(), stop),
      {reply, ok, State#state{close_arg = {CloseRef, Handler, Reason}}}
  end;    
handle_call(_Request, _From, State) ->
  %% !!!应该不会执行
  {reply, ok, State}.

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
handle_cast(_Request, State) ->
  {noreply, State}.

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
handle_info({'DOWN', Ref, process, _Object, _Reason}, #state{monitor_ref = Ref} = State) ->
  erlang:erase(parent_pid),
  {noreply, State};
handle_info({soft_stop_immediately, ParentPid}, #state{close_arg = {Timer, _, Reason}} = State) ->
  case is_reference(Timer) of
    true -> 
      erlang:cancel_timer(Timer);
    false -> 
      skip
  end,
  do_stop(State),
  ParentPid ! ok,
  {stop, Reason, State};
handle_info({soft_stop_immediately, ParentPid}, State) ->
  {noreply, State#state{flag = {?FLAG_STOP, ParentPid}}};
handle_info(stop, #state{close_arg = {_, _, Reason}, flag = Flag} = State) ->
  do_stop(State),
  case Flag of
    {?FLAG_STOP, ParentPid} -> ParentPid ! ok;
    _ -> skip 
  end,
  {stop, Reason, State};
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
terminate(_Reason, _State) ->
  game_ws:disconnect(self()),
  ok.

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
inside(Msg) ->
  case erlang:get(parent_pid) of
    {ok, Pid} ->
      erlang:send(Pid, Msg),
      ok;
    _ ->
      error
  end.

do_stop(#state{close_arg = {_, Handler, Reason}, handler_state = State}) ->
  case erlang:function_exported(Handler, terminate, 2) of
    true ->
      Handler:terminate(Reason, State);
    false ->
      ok
  end.
