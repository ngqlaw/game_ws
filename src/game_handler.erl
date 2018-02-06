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

-type (handle_result(State) :: {ok, Reply::term(), Message::binary(), State}
| {ok, Reply::term(), State}
| {ok, State}
| {reply, Message::binary(), State}
| {stop, Reason::term(), Reply::term(), State}
| {stop, Reason::term(), State}).

-type (ack_resutl(State) :: {ok, State} | {reply, Message::binary(), State} | any()).

%% 初始化进程内存
-callback (init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary(), State::any()}
| {already_started, Handler::pid()} | {already_started, Handler::pid(), Event::term()}
| {stop, Error::term()}).

%% 消息处理
-callback (handle({tcp, binary()} | term(), State) -> handle_result(State) when State::any()).

%% ping(消息已回复)
-callback (ping(Payload::binary(), State) -> ack_resutl(State) when State::any()).
-optional_callbacks([ping/2]).

%% pong(消息已回复)
-callback (pong(Payload::binary(), State) -> ack_resutl(State) when State::any()).
-optional_callbacks([pong/2]).

%% 通信结束
-callback (close(Reason::any(), State) -> {ok, State} | any() when State::any()).
-optional_callbacks([close/2]).

%% 服务结束
-callback (terminate(Reason::any(), State) -> any() when State::any()).
-optional_callbacks([terminate/2]).

%% API
-export([start_link/3, init/4]).
-export([event/2, net_message/2, sys_message/2]).

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

-record(state, {
  monitor_ref = undefined :: reference(),
  shutdown = 0 :: integer(),
  socket_pid = undefined :: undefined | pid(),
  handler = undefined :: atom(),
  handler_state = #{} :: map(),
  close_timer = undefined :: any(),
  close_ref = undefined :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================
%% 给网络服务进程发送事件消息
event(Server, Event) when Server == self() ->
  Pid = get_socket(),
  erlang:send(Pid, Event),
  ok;
event(Server, Event) ->
  gen_server:cast(Server, {event, Event}).

%% 处理通信事件消息
net_message(Server, Msg) ->  
  gen_server:call(Server, {net_message, Msg}).

%% 自定义消息
sys_message(Server, Msg) ->  
  gen_server:call(Server, {sys_message, Msg}).    

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
  set_socket(ParentPid),
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
        {error, Error} ->
          proc_lib:init_ack(Starter, {error, Error})
      end;
    {already_started, Pid, Event} ->
      case gen_server:call(Pid, {sys_message, {reconnect, Handler, ParentPid}, Event}) of
        ok ->
          proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket}});
        {ok, {error, Error}} ->
          proc_lib:init_ack(Starter, {error, Error});
        {ok, _} ->
          proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket}});
        {ok, {error, Error}, _Message} ->
          proc_lib:init_ack(Starter, {error, Error});
        {ok, _, Message} ->
          proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket#{reply => Message}}});
        {reply, Message} ->
          proc_lib:init_ack(Starter, {error, {already_started, Pid, NewSocket#{reply => Message}}});
        {stop, Reply} ->
          proc_lib:init_ack(Starter, {error, Reply});
        {error, Error} ->
          proc_lib:init_ack(Starter, {error, Error});
        stop ->
          proc_lib:init_ack(Starter, {error, fail})
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
%% 网络信息通信处理
handle_call({net_message, ping}, _From, State) ->
  ack(ping, <<>>, State);
handle_call({net_message, {ping, Msg}}, _From, State) ->
  ack(ping, Msg, State);
handle_call({net_message, pong}, _From, State) ->
  ack(pong, <<>>, State);
handle_call({net_message, {pong, Msg}}, _From, State) ->
  ack(pong, Msg, State);
handle_call({net_message, {_, Msg}}, _From, State) ->
  handle_message({tcp, to_binary(Msg)}, State);

%% 系统消息处理
handle_call({sys_message, {reconnect, Handler, ParentPid}}, _From, #state{
    monitor_ref = OldRef,
    handler = Handler
  } = State) ->
  %% 关闭其它连接
  game_ws:disconnect(self()),
  receive
    {'DOWN', OldRef, process, _Object, _Reason} ->
      Ref = erlang:monitor(process, ParentPid),
      set_socket(ParentPid),
      {reply, ok, State#state{
        monitor_ref = Ref,
        socket_pid = ParentPid
      }}
  after 3000 -> 
    {reply, {error, timeout}, State}
  end;
handle_call({sys_message, {reconnect, _Handler, _ParentPid}}, _From, State) ->
  {reply, {error, fail}, State};
handle_call({sys_message, {reconnect, Handler, ParentPid}, Msg}, _From, #state{
    monitor_ref = OldRef,
    handler = Handler
  } = State) ->
  %% 关闭其它连接
  game_ws:disconnect(self()),
  receive
    {'DOWN', OldRef, process, _Object, _Reason} ->
      Ref = erlang:monitor(process, ParentPid),
      set_socket(ParentPid),
      handle_message(Msg, State#state{
        monitor_ref = Ref,
        socket_pid = ParentPid
      })
  after 3000 -> 
    {reply, {error, reconnect_fail}, State}
  end;
handle_call({sys_message, {reconnect, _Handler, _ParentPid}, _Msg}, _From, State) ->
  {reply, {error, fail}, State};
handle_call({sys_message, Msg}, _From, State) ->
  handle_message(Msg, State);

%% 强制关闭进程
handle_call({soft_stop_immediately, SupPid}, From, State) ->
  gen_server:reply(From, ok),
  do_stop(State, shutdown),
  SupPid ! ok,
  {stop, normal, State};

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
handle_cast({event, Msg}, #state{socket_pid = Pid} = State) ->
  erlang:send(Pid, Msg),
  {noreply, State};
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
handle_info({'DOWN', Ref, process, _Object, Reason}, #state{
    monitor_ref = Ref,
    shutdown = 0, 
    close_timer = Old
  } = State) ->
  case do_close(State, Reason) of
    {ok, NewState} -> ok;
    _ -> NewState = State
  end,
  case is_reference(Old) of
    true -> erlang:cancel_timer(Old);
    false -> skip
  end,
  do_stop(NewState, Reason),
  {stop, normal, ok, NewState};
handle_info({'DOWN', Ref, process, _Object, Reason}, #state{
    monitor_ref = Ref,
    shutdown = Shutdown, 
    close_timer = Old
  } = State) ->
  case do_close(State, Reason) of
    {ok, NewState} -> ok;
    _ -> NewState = State
  end,
  case is_reference(Old) andalso (false == erlang:read_timer(Old)) of
    true -> 
      {noreply, NewState};
    false -> 
      CloseRef = erlang:make_ref(),
      CloseTimer = erlang:send_after(Shutdown, self(), {stop, CloseRef, Reason}),
      {noreply, NewState#state{close_timer = CloseTimer, close_ref = CloseRef}}
  end;
handle_info({stop, CloseRef, Reason}, #state{close_ref = CloseRef} = State) ->
  do_stop(State, Reason),
  {stop, normal, State};
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
do_close(#state{handler = Handler, handler_state = State}, Reason) ->
  case erlang:function_exported(Handler, close, 2) of
    true ->
      Handler:close(Reason, State);
    false ->
      ok
  end.

do_stop(#state{handler = Handler, handler_state = State}, Reason) ->
  case erlang:function_exported(Handler, terminate, 2) of
    true ->
      Handler:terminate(Reason, State);
    false ->
      ok
  end.

ack(Fun, Msg, #state{handler = Handler, handler_state = HandlerState} = State) ->
  case erlang:function_exported(Handler, Fun, 2) of
    true ->
      case Handler:Fun(to_binary(Msg), HandlerState) of
        {ok, NewHandlerState} ->
          {reply, ok, State#state{handler_state = NewHandlerState}};
        {reply, Reply, NewHandlerState} ->
          {reply, {reply, Reply}, State#state{handler_state = NewHandlerState}};
        _ ->
          {reply, ok, State}
      end;
    false ->
      {reply, ok, State}
  end.

handle_message(Msg, #state{handler = Handler, handler_state = HandlerState} = State) ->
  case Handler:handle(Msg, HandlerState) of
    {ok, NewHandlerState} ->
      {reply, ok, State#state{handler_state = NewHandlerState}};
    {ok, Reply, NewHandlerState} ->
      {reply, {ok, Reply}, State#state{handler_state = NewHandlerState}};
    {ok, Reply, Message, NewHandlerState} ->
      {reply, {ok, Reply, Message}, State#state{handler_state = NewHandlerState}};
    {reply, Message, NewHandlerState} ->
      {reply, {reply, Message}, State#state{handler_state = NewHandlerState}};
    {stop, Reason, Reply, NewHandlerState} ->
      do_stop(NewHandlerState, Reason),
      {stop, normal, {stop, Reply}, State#state{handler_state = NewHandlerState}};
    {stop, Reason, NewHandlerState} ->
      do_stop(NewHandlerState, Reason),
      {stop, normal, stop, State#state{handler_state = NewHandlerState}}    
  end.

get_socket() ->
  erlang:get({?MODULE, local, socket_pid}).
set_socket(Pid) ->
  erlang:put({?MODULE, local, socket_pid}, Pid).

to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V. 
