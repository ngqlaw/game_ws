%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% cowboy websocket handler
%%% @end
%%% Created : 19. 十二月 2017 16:34
%%%-------------------------------------------------------------------
-module(game_ws_handler).

%% cowboy callback function
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, Opts) ->
  Handler = proplists:get_value(handler, Opts),
  Shutdown = proplists:get_value(shutdown, Opts),
  SupPid = proplists:get_value(sup_pid, Opts),
  State = #{
    handler => Handler,
    shutdown => Shutdown,
    sup_pid => SupPid
  },
  {cowboy_websocket, Req, State}.

websocket_init(#{handler := Handler, sup_pid := SupPid} = State) ->
  case game_ws_sup:start_child(SupPid, Handler, self()) of
    {ok, HandlePid} ->
      {ok, State#{pid => HandlePid}};
    {ok, HandlePid, Reply} ->
      {reply, {binary, Reply}, State#{pid => HandlePid}};
    {error, {already_started, HandlePid}} ->
      {ok, State#{pid => HandlePid}};
    {error, {already_started, HandlePid, Reply}} ->
      {reply, {binary, Reply}, State#{pid => HandlePid}};
    _ ->
      {stop, State}
  end.

websocket_handle(Msg, #{handler := Handler, pid := Pid} = State) ->
  case game_handler:handle_outside(Pid, Handler, Msg) of
    ok ->
      {ok, State};
    {ok, Reply} ->
      {reply, {binary, Reply}, State};
    stop ->
      {stop, State}
  end.

websocket_info({"send_msg", Msg}, State) ->
  {reply, {binary, Msg}, State};
websocket_info("disconnect", State) ->
  {stop, State};
websocket_info({control, Control}, #{handler := Handler, pid := Pid} = State) ->
  case game_handler:handle_control(Pid, Handler, Control) of
    ok ->
      {ok, State};
    {ok, Reply} ->
      {reply, {binary, Reply}, State};
    stop ->
      {stop, State}
  end;  
websocket_info(_Info, State) ->
  {ok, State}.

terminate(Reason, _Req, #{handler := Handler, pid := Pid, shutdown := Shutdown}) ->
  game_handler:stop(Pid, Handler, Shutdown, Reason),
  ok.