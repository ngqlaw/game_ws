%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2021-2022, ninggq <ngq_scut@126.com>
%%% @doc
%%%     默认登陆模块
%%% @end
%%% Created : 02. 五月 2021 16:43
%%%-------------------------------------------------------------------
-module(game_ws_connect).

%% 消息处理
-callback(init(Req :: map()) -> {ok, State::term()}).
-callback(handle(Msg, State) ->
    {ok, State} | {ok, Msg, State}                  %% 连接握手正常完成
    | {continue, State} | {continue, Msg, State}    %% 继续连接握手
    | {reconnect, Pid} | {reconnect, Msg, State}    %% 重连
    | {stop, State} | {stop, Msg, State}            %% 关闭连接
    when Msg::term(), State::term(), Pid::pid()).

%% API exports
-export([init/1, handle/2]).

-record(state, {}).

%%====================================================================
%% API functions
%%====================================================================
%% @doc 连接成功初始化
init(_Req) ->
    {ok, #state{}}.

%% @doc 连接握手协议处理
handle(_Msg, State) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
