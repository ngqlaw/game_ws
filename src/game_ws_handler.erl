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
  Shutdown = case proplists:get_value(shutdown, Opts, 0) of
    N when is_integer(N) -> N;
    _ -> 0
  end,
  SupPid = proplists:get_value(sup_pid, Opts),
  MsgType = case proplists:get_value(msg_type, Opts, text) of
    text -> text;
    binary -> binary;
    _ -> text
  end,
  State = #{
    init => Req,
    sup_pid => SupPid,
    shutdown => Shutdown,
    msg_type => MsgType
  },
  {cowboy_websocket, Req, State}.

websocket_init(State) ->
  case game_ws_sup:start_child(State, self()) of
    {ok, HandlePid, NewState} ->
      do_reply(NewState, HandlePid);
    {error, {already_started, HandlePid, NewState}} ->
      do_reply(NewState, HandlePid);
    {error, {reply, Message}} ->
      {reply, {close, 1000, Message}, State};
    _ ->
      {stop, State}
  end.

do_reply(#{msg_type := MsgType} = State, Pid) ->
  case maps:take(reply, State) of
    {Reply, NewState} ->
      {reply, {MsgType, Reply}, NewState#{pid => Pid}};
    error ->
      {ok, State#{pid => Pid}}
  end.

websocket_handle(Msg, #{pid := Pid, msg_type := MsgType} = State) when is_pid(Pid) ->
  case catch game_handler:net_message(Pid, Msg) of
    ok ->
      {ok, State};
    {ok, _Reply} -> %% tcp消息忽略返回
      {ok, State};
    {ok, _Reply, Message} -> %% tcp消息忽略返回
      {reply, {MsgType, Message}, State};
    {reply, Message} ->
      {reply, {MsgType, Message}, State};
    {stop_and_reply, _Reply, Message} ->
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop_and_reply, Message} ->
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop, _Reply} ->
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    stop ->
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    _ ->
      {stop, State}
  end;
websocket_handle(_, State) ->
  {ok, State}.

websocket_info({"send_msg", Msg}, #{msg_type := MsgType} = State) when is_binary(Msg) ->
  {reply, {MsgType, Msg}, State};
websocket_info({"disconnect", Msg}, State) ->
  {reply, {close, 1000, Msg}, State};
websocket_info({call, ReplyPid, Ref, SysMsg}, #{pid := Pid, msg_type := MsgType} = State) when is_pid(Pid) ->
  case catch game_handler:sys_message(Pid, SysMsg) of
    ok ->
      ReplyPid ! {ok, Ref, ok},
      {ok, State};
    {ok, Reply} -> 
      ReplyPid ! {ok, Ref, Reply},
      {ok, State};
    {ok, Reply, Message} ->
      ReplyPid ! {ok, Ref, Reply},
      {reply, {MsgType, Message}, State};
    {reply, Message} ->
      ReplyPid ! {ok, Ref, ok},
      {reply, {MsgType, Message}, State};
    {stop_and_reply, Reply, Message} ->
      ReplyPid ! {ok, Ref, Reply},
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop_and_reply, Message} ->
      ReplyPid ! {ok, Ref, process_down},
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop, Reply} ->
      ReplyPid ! {ok, Ref, Reply},
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    stop ->
      ReplyPid ! {ok, Ref, process_down},
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    _ ->
      {stop, State}
  end;
websocket_info({cast, SysMsg}, #{pid := Pid, msg_type := MsgType} = State) when is_pid(Pid) ->
  case catch game_handler:sys_message(Pid, SysMsg) of
    ok ->
      {ok, State};
    {ok, _Reply} -> 
      {ok, State};
    {ok, _Reply, Message} ->
      {reply, {MsgType, Message}, State};
    {reply, Message} ->
      {reply, {MsgType, Message}, State};
    {stop_and_reply, _Reply, Message} ->
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop_and_reply, Message} ->
      {reply, {close, 1000, Message}, State#{pid => undefined}};
    {stop, _Reply} ->
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    stop ->
      {reply, {close, 1000, ""}, State#{pid => undefined}};
    _ ->
      {stop, State}
  end;
websocket_info(_, State) ->
  {ok, State}.

terminate(_Reason, _Req, _State) ->
  ok.
