%%%-------------------------------------------------------------------
%%% @author ninggq <ngq_scut@126.com>
%%% @copyright (C) 2020-2021, ninggq <ngq_scut@126.com>
%%% @doc
%%% 默认序列化模块
%%% @end
%%% Created : 26. 八月 2020 20:29
%%%-------------------------------------------------------------------
-module(game_ws_serialize).

%% API exports
-export([serialize/1, deserialize/1]).

%%====================================================================
%% API functions
%%====================================================================
%% @doc 序列化
-spec(serialize(Msg :: any()) -> Binary :: binary()).
serialize(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
serialize(Msg) when is_binary(Msg) ->
    Msg.

%% @doc 反序列化
-spec(deserialize(Binary :: binary()) -> Msg :: any()).
deserialize(Binary) ->
    Binary.

%%====================================================================
%% Internal functions
%%====================================================================
