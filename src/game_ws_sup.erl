%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% websocket for game
%%% @end
%%% Created : 19. 十二月 2017 10:13
%%%-------------------------------------------------------------------
-module(game_ws_sup).
-author("ninggq").

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod, Handler), {Mod, {Mod, start_link, [Handler]}, temporary, 5000, worker, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_child(State, ParentPid) ->
  {Srever, NewState} = maps:take(sup_pid, State),
  supervisor:start_child(Srever, [NewState, ParentPid]).

stop(Srever) ->
  Children = supervisor:which_children(Srever),
  N = lists:foldl(fun
    ({_, Pid, _, _}, Acc) when is_pid(Pid) ->
      case catch gen_server:call(Pid, {soft_stop_immediately, self()}) of
        ok -> Acc + 1;
        _ -> Acc
      end
  end, 0, Children),
  loop_stop(N). 

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Opt :: list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opt) ->
  supervisor:start_link(?MODULE, [Opt]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([Opt]) ->
  %% 启动网络进程
  Ref = proplists:get_value(ref, Opt, undefined),
  Module = proplists:get_value(module, Opt, undefined),
  Port = proplists:get_value(port, Opt, 8080),
  Host = proplists:get_value(host, Opt, '_'),
  Path = proplists:get_value(path, Opt, "/"),
  Dispatch = cowboy_router:compile([
    {Host, [
      {Path, game_ws_handler, [{sup_pid, self()}]}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(Ref, [{port, Port}], #{
    env => #{dispatch => Dispatch}
  }),
  {ok, {{simple_one_for_one, 3, 10}, [?CHILD(game_handler, Module)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
loop_stop(N) when N > 0 ->
  receive
    ok -> loop_stop(N - 1)
  after 5000 ->
    {error, timeout}  
  end;
loop_stop(_) ->
  ok.