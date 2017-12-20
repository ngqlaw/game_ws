= game_ws

An websocket library for special game

== Usage

使用的时候需要先分配端口和设置回调模块
```
-callback init(pid()) -> {ok, any()} | stop.

-callback ping(binary(), State) -> ok | stop when State::any().
-optional_callbacks([ping/2]).

-callback handle(binary(), State) -> call_result(State) when State::any().

-callback terminate(Reason::any(), State) -> ok when State::any().
-optional_callbacks([terminate/3]).
```
然后调用
```
%% 启动服务
-spec(start_server(Port :: integer(), CallbackMod :: atom()) ->
  {ok, pid()} | {ok, pid(), Info :: term()} |
  {error, already_present | {already_started, pid()}}).

-spec(start_server(Port :: integer(), CallbackMod :: atom(), ShutdownType :: integer()) ->
  {ok, pid()} | {ok, pid(), Info :: term()} |
  {error, already_present | {already_started, pid()}}).    
```
返回值中的pid可用来支持以下调用
```
%% 发送信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
%% 关闭连接
-spec(disconnect(Server :: pid()) -> ok).
```