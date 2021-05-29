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
-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

%%%===================================================================
%%% API
%%%===================================================================
init(Req, Opts) ->
    %% supervisor pid
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
            SMod when is_atom(SMod) -> SMod;
            _ -> game_ws_serialize
        end,
    %% 登陆模块
    ConnectMod =
        case proplists:get_value(connect, Opts, game_ws_connect) of
            CMod when is_atom(CMod) -> CMod;
            _ -> game_ws_connect
        end,
    State = #{
        sup_pid => SupPid,
        connect => ConnectMod,
        data => Req,
        serialize => SerializeMod,
        msg_type => MsgType
    },
    {cowboy_websocket, Req, State}.

websocket_init(#{
    connect := ConnectMod, data := Req, serialize := Mod
} = State) ->
    case ConnectMod:init(Req) of
        {ok, ConnectState} ->
            {ok, State#{data => ConnectState}};
        {error, Msg} ->
            Message = Mod:serialize(Msg),
            {reply, {close, 1000, Message}, State};
        _ ->
            {stop, State}
    end.

websocket_handle({_, Msg}, #{
    pid := Pid, serialize := Mod
} = State) when is_pid(Pid) ->
    Message = Mod:deserialize(to_binary(Msg)),
    gen_server:cast(Pid, {msg, Message}),
    {ok, State};
websocket_handle({_, Msg}, #{
    connect := ConnectMod, data := Data,
    msg_type := MsgType, serialize := Mod
} = State) ->
    Message = Mod:deserialize(to_binary(Msg)),
    case ConnectMod:handle(Message, Data) of
        {Res, Msg, NewData} ->
            Message = Mod:serialize(Msg),
            case handle_res(Res, State#{data => NewData}) of
                {ok, NewState} ->
                    {reply, {MsgType, Message}, NewState};
                {stop, NewState} ->
                    {reply, {close, 1000, Message}, NewState};
                stop ->
                    {stop, State}
            end;
        {Res, NewData0} ->
            case handle_res(Res, NewData0) of
                {ok, NewState} ->
                    {ok, NewState};
                {stop, NewState} ->
                    {stop, NewState};
                stop ->
                    {stop, State}
            end
    end;
websocket_handle(_, State) ->
    {ok, State}.

websocket_info({"send_msg", Msg}, #{
    msg_type := MsgType, serialize := Mod
} = State) ->
    Message = Mod:serialize(Msg),
    {reply, {MsgType, Message}, State};
websocket_info({"disconnect", ""}, State) ->
    {reply, {close, 1000, <<>>}, State};
websocket_info({"disconnect", Msg}, #{serialize := Mod} = State) ->
    Message = Mod:serialize(Msg),
    {reply, {close, 1000, Message}, State};
websocket_info(_, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_res(continue, State) ->
    {ok, State};
handle_res(ok, #{sup_pid := SupPid, data := Data} = State) ->
    case game_ws_sup:start_child(SupPid, Data, self()) of
        {ok, HandlePid} ->
            {ok, State#{data => undefined, pid => HandlePid}};
        _ ->
            stop
    end;
handle_res(reconnect, #{data := HandlePid} = State) ->
    {ok, State#{data => undefined, pid => HandlePid}};
handle_res(stop, State) ->
    {stop, State}.

to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V. 
