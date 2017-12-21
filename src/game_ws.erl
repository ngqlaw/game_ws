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
-export([start_server/2, stop_server/1]).
-export([send_msg/2, disconnect/1, control/2]).

-define(CHILD(Id, Mod, Opt), {Id, {Mod, start_link, [Opt]}, permanent, 5000, supervisor, [Mod]}).

%%====================================================================
%% API functions
%%====================================================================
%% 启动服务
-spec(start_server(Ref :: any(), Opt :: list()) ->
  {ok, pid()} |
  {error, already_present | {already_started, pid()}}).
start_server(Ref, Opt) ->
  Child = ?CHILD(Ref, game_ws_sup, [{ref, Ref}|Opt]),
  game_ws_sup_sup:start_child(Child).

%% 停止服务
-spec(stop_server(Ref :: any()) -> ok | {error, not_found | timeout}).
stop_server(Ref) ->
  case cowboy:stop_listener(Ref) of
    ok ->
      game_ws_sup_sup:stop_child(Ref);
    Error ->
      Error  
  end.

%% 发送信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
send_msg(Server, Msg) ->
  game_handler:handle_inside(Server, {"send_msg", Msg}).

%% 关闭连接
-spec(disconnect(Server :: pid()) -> ok).
disconnect(Server) ->
  game_handler:handle_inside(Server, "disconnect").

%% 自定义控制
-spec(control(Server :: pid(), Control :: any()) -> ok).
control(Server, Control) ->
  game_handler:handle_inside(Server, {control, Control}).

%%====================================================================
%% Internal functions
%%====================================================================
