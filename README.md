game_ws
=====

An websocket library for special game

Usage
-----

使用的时候需要先设置连接回调模块
```
-type(handle_reply() :: ok | continue | reconnect | stop).

-type(handle_result(State) :: {handle_reply(), State}
| {handle_reply(), Message::any(), State}).

%% 初始化进程内存
-callback(init(Req::map()) -> {ok, State::any()} | {error, Reply::binary()}).

%% 网络消息处理
-callback(handle(term(), State) -> handle_result(State) when State::any()).

函数传入的pid可用来支持以下调用以及gen_server的cast/call/info消息：
%% 发送网络信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
%% 关闭网络连接
-spec(disconnect(Server :: pid()) -> ok).
```

```
%% 序列化模块要求回调
-callback(serialize(Msg :: any()) -> Binary :: binary()).
-callback(deserialize(Binary :: binary()) -> Msg :: any()).
```

然后调用
```
%% 启动服务
-spec(start_server(Ref :: any(), Opt :: list()) ->
  {ok, pid()} |
  {error, already_present | {already_started, pid()}}).

  Opt可以配置的参数：
  module    - 回调模块（gen_server),默认undefined
  connect   - 连接回调模块,默认game_ws_connect
  serialize - 序列化模块,默认game_ws_serialize
  shutdown  - 进程延时关闭时间(毫秒),默认0
  timeout   - 网络进程待机时间(毫秒),默认60000
  msg_type  - 返回消息类型: text | binary,默认text
  port      - 服务端口,例如:9999,默认8080
  host      - 域,例如:'_' (参照cowboy路由设置),默认'_'
  path      - 路径,例如:"/path" (参照cowboy路由设置),默认"/"
```

关闭服务可以调用
```
%% 停止服务
-spec(stop_server(Ref :: any()) -> ok | {error, not_found | timeout}).
```
