%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2017-2018, ninggq <ngq_scut@126.com>
%%% @doc
%%% cowboy websocket handler
%%% @end
%%% Created : 19. 十二月 2017 16:34
%%%-------------------------------------------------------------------
-module(game_ws_handler).

-behaviour(cowboy_websocket).

%% cowboy callback function
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, Opts) ->
    %% 应用在断开连接后自动关闭时间间隔，0表示立即关闭
    Shutdown =
        case proplists:get_value(shutdown, Opts, 0) of
            N when is_integer(N) -> N;
            _ -> 0
        end,
    SupPid = proplists:get_value(sup_pid, Opts),
    %% 通信消息格式
    MsgType =
        case proplists:get_value(msg_type, Opts, text) of
            text -> text;
            binary -> binary;
            _ -> text
        end,
    %% 序列化模块
    SerializeMod =
        case proplists:get_value(serialize, Opts, game_ws_serialize) of
            Mod when is_atom(Mod) -> Mod;
            _ -> game_ws_serialize
        end,
    State = #{
        init => Req,
        sup_pid => SupPid,
        shutdown => Shutdown,
        serialize => SerializeMod,
        msg_type => MsgType
    },
    {cowboy_websocket, Req, State}.

websocket_init(#{serialize := Mod} = State) ->
    case game_ws_sup:start_child(State, self()) of
        {ok, HandlePid, NewState} ->
            do_reply(NewState, HandlePid);
        {error, {already_started, HandlePid, NewState}} ->
            do_reply(NewState, HandlePid);
        {error, {reply, Reply}} ->
            Message = Mod:serialize(Reply),
            {reply, {close, 1000, Message}, State};
        _ ->
            {stop, State}
    end.

do_reply(#{msg_type := MsgType, serialize := Mod} = State, Pid) ->
    case maps:take(reply, State) of
        {Reply, NewState} ->
            Message = Mod:serialize(Reply),
            {reply, {MsgType, Message}, NewState#{pid => Pid}};
        error ->
            {ok, State#{pid => Pid}}
    end.

websocket_handle(Msg, #{pid := Pid, serialize := Mod} = State) when is_pid(Pid) ->
    Message = deserialize(Msg, Mod),
    gen_server:cast(Pid, {net_message, Message}),
    {ok, State};
websocket_handle(_, State) ->
    {ok, State}.

deserialize(ping, _Mod) ->
    {ping, []};
deserialize({ping, Msg}, Mod) ->
    {ping, Mod:deserialize(to_binary(Msg))};
deserialize(pong, _Mod) ->
    {pong, []};
deserialize({pong, Msg}, Mod) ->
    {pong, Mod:deserialize(to_binary(Msg))};
deserialize({_, Msg}, Mod) ->
    {tcp, Mod:deserialize(to_binary(Msg))}.

to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V. 

websocket_info({"send_msg", Msg}, #{msg_type := MsgType, serialize := Mod} = State) ->
    Message = Mod:serialize(Msg),
    {reply, {MsgType, Message}, State};
websocket_info({"reconnect", Pid}, #{pid := CurrentPid, serialize := Mod} = State) ->
    SocketPid = self(),
    case gen_server:call(Pid, {sys_message, {reconnect, SocketPid}}) of
        ok ->
            gen_server:cast(CurrentPid, {sys_message, {reconnect_done, SocketPid}}),
            {ok, State#{pid => Pid}};
        {reply, Reply} ->
            do_reply(State#{reply => Reply}, Pid);
        {error, {reply, Reply}} ->
            Message = Mod:serialize(Reply),
            {reply, {close, 1000, Message}, State};
        _ ->
            {reply, {close, 1000, <<>>}, State}
    end;
websocket_info({"disconnect", ""}, State) ->
    {reply, {close, 1000, <<>>}, State};
websocket_info({"disconnect", Msg}, #{serialize := Mod} = State) ->
    Message = Mod:serialize(Msg),
    {reply, {close, 1000, Message}, State};
websocket_info(_, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
