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
-export([start_link/1, start_child/3, stop/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, temporary, 5000, worker, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_child(Server, State, SocketPid) ->
    supervisor:start_child(Server, [State, SocketPid]).

stop(Server, StopMsg) ->
    Children = supervisor:which_children(Server),
    N = lists:foldl(
        fun({_, Pid, _, _}, Acc) when is_pid(Pid) ->
            gen_server:cast(Pid, {stop, StopMsg}),
            Acc + 1;
            (_, Acc) -> Acc
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
    Timeout = proplists:get_value(timeout, Opt, 60000),
    Port = proplists:get_value(port, Opt, 8080),
    Host = proplists:get_value(host, Opt, '_'),
    Path = proplists:get_value(path, Opt, "/"),
    BaseOtp = normalize_options(Opt, [{sup_pid, self()}]),
    Dispatch = cowboy_router:compile([
        {Host, [
            {Path, game_ws_handler, BaseOtp}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Ref, [{port, Port}], #{
        env => #{dispatch => Dispatch},
        idle_timeout => Timeout
    }),
    {ok, {{simple_one_for_one, 3, 10}, [?CHILD(Module)]}}.

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

normalize_options([{shutdown, V}|T], Opt) ->
    normalize_options(T, [{shutdown, V}|Opt]);
normalize_options([{msg_type, V}|T], Opt) ->
    normalize_options(T, [{msg_type, V}|Opt]);
normalize_options([{serialize, V}|T], Opt) ->
    normalize_options(T, [{serialize, V}|Opt]);
normalize_options([{connect, V}|T], Opt) ->
    normalize_options(T, [{connect, V}|Opt]);
normalize_options([_|T], Opt) ->
    normalize_options(T, Opt);
normalize_options([], Opt) -> Opt.
