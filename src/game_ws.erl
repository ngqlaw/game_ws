%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% game handler
%%% @end
%%% Created : 19. 十二月 2017 16:34
%%%-------------------------------------------------------------------
-module(game_ws).

%% API exports
-export([start_server/2, start_server/3, send_msg/2, disconnect/1]).

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, 5000, supervisor, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================
%% 启动服务
-spec(start_server(Port :: integer(), CallbackMod :: atom()) ->
  {ok, pid()} | {ok, pid(), Info :: term()} |
  {error, already_present | {already_started, pid()}}).
start_server(Port, CallbackMod) when is_integer(Port) andalso is_atom(CallbackMod) ->
  start_server(Port, CallbackMod, 0).

-spec(start_server(Port :: integer(), CallbackMod :: atom(), ShutdownType :: integer()) ->
  {ok, pid()} | {ok, pid(), Info :: term()} |
  {error, already_present | {already_started, pid()}}).  
start_server(Port, CallbackMod, ShutdownType) when is_integer(Port) andalso is_atom(CallbackMod) andalso
  is_integer(ShutdownType) andalso ShutdownType >= 0 ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/socket", game_ws_handler, [{handler, CallbackMod}, {shutdown, ShutdownType}]}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch}
  }),
  Child = ?CHILD(game_ws_sup),
  game_ws_sup_sup:start_child(Child).

%% 发送信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
send_msg(Server, Msg) ->
  game_handler:handle_inside(Server, {"send_msg", Msg}).

%% 关闭连接
-spec(disconnect(Server :: pid()) -> ok).
disconnect(Server) ->
  game_handler:handle_inside(Server, "disconnect").

%%====================================================================
%% Internal functions
%%====================================================================
