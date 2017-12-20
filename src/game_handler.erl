%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% Game network information handler
%%% @end
%%% Created : 19. 十二月 2017 10:26
%%%-------------------------------------------------------------------
-module(game_handler).
-author("Administrator").

-behaviour(gen_server).

-type call_result(State) :: {ok, State}
| {reply, binary(), State}
| {stop, State}.

-callback init() -> {ok, State::any()} | stop.

-callback ping(binary(), State) -> ok | stop when State::any().
-optional_callbacks([ping/2]).

-callback handle(binary(), State) -> call_result(State) when State::any().

-callback terminate(Reason::any(), State) -> ok when State::any().
-optional_callbacks([terminate/2]).

%% API
-export([start_link/2]).
-export([handle_inside/2, handle_outside/3, stop/4]).

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
  handler_state = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================
handle_inside(Server, Event) when Server == self() ->
  inside(Event);
handle_inside(Server, Event) ->
  gen_server:call(Server, {handle_inside, Event}).

handle_outside(Server, Handler, Msg) ->  
  gen_server:call(Server, {handle_outside, Handler, Msg}).

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
start_link(Handler, ParentPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Handler, ParentPid], []).

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
init([Handler, ParentPid]) ->
  erlang:put(parent_pid, {ok, ParentPid}),
  case Handler:init() of
    {ok, HandleState} ->
      Ref = erlang:monitor(process, ParentPid),
      {ok, #state{
        monitor_ref = Ref,
        handler_state = HandleState
      }};
    stop ->
      {stop, init_fail}
  end.

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
handle_call({handle_outside, Handler, ping}, _From, State) ->
  Reply = ping(Handler, <<>>, State),
  {reply, Reply, State};
handle_call({handle_outside, Handler, {ping, Msg}}, _From, State) ->
  Reply = ping(Handler, Msg, State),
  {reply, Reply, State};  
handle_call({handle_outside, Handler, {_, Msg}}, _From, State) ->
  case Handler:handle(Msg, State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    {reply, Reply, NewState} ->
      {reply, {ok, Reply}, NewState};
    {stop, NewState} ->
      {reply, stop, NewState}    
  end;
handle_call({handle_inside, Msg}, _From, #state{} = State) ->
  Reply = inside(Msg),
  {reply, Reply, State};
handle_call({stop, Handler, 0, Reason}, _From, #state{} = State) ->
  do_stop(Handler, Reason, State),
  {stop, Reason, ok, State};
handle_call({stop, Handler, Shutdown, Reason}, _From, #state{} = State) ->
  Timeout = case is_integer(Shutdown) of
    true -> Shutdown;
    false -> 1000
  end,
  erlang:send_after(Timeout, self(), {stop, Handler, Reason}),
  {reply, ok, State};    
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
handle_info({stop, Handler, Reason}, State) ->
  do_stop(Handler, Reason, State),
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
ping(Handler, Msg, State) ->
  case erlang:function_exported(Handler, ping, 2) of
    true ->
      Handler:ping(Msg, State);
    false ->
      ok
  end.

inside(Msg) ->
  case erlang:get(parent_pid) of
    {ok, Pid} ->
      erlang:send(Pid, Msg),
      ok;
    _ ->
      error
  end.

do_stop(Handler, Reason, State) ->
  case erlang:function_exported(Handler, terminate, 2) of
    true ->
      Handler:terminate(Reason, State);
    false ->
      ok
  end.
