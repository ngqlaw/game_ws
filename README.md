game_ws
=====

An websocket library for special game

Usage
-----

使用的时候需要先设置回调模块
```
-type (handle_result(State) :: {ok, Reply::term(), Message::binary(), State}
| {ok, Reply::term(), State}
| {ok, State}
| {reply, Message::binary(), State}
| {stop, Reason::term(), Reply::term(), State}
| {stop, Reason::term(), State}).

-type (ack_resutl(State) :: {ok, State} | {reply, Message::binary(), State} | any()).

%% 初始化进程内存
-callback (init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary(), State::any()}
| {already_started, Handler::pid()} | {already_started, Handler::pid(), Event::term()}
| {stop, Error::term()}).

函数传入的pid可用来支持以下调用：
%% 发送信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
%% 关闭连接
-spec(disconnect(Server :: pid()) -> ok).
%% 发送阻塞消息
-spec(call(Server :: pid(), Event :: any()) -> {error, timeout} | term()).
%% 发送非阻塞消息
-spec(cast(Server :: pid(), Event :: any()) -> ok).

%% 消息处理
-callback (handle({tcp, binary()} | term(), State) -> handle_result(State) when State::any()).

%% ping(消息已回复)
-callback (ping(Payload::binary(), State) -> ack_resutl(State) when State::any()).
-optional_callbacks([ping/2]).

%% pong(消息已回复)
-callback (pong(Payload::binary(), State) -> ack_resutl(State) when State::any()).
-optional_callbacks([pong/2]).

%% 通信结束(服务器主动断开情况下，返回消息可以发送；非主动断开，返回消息会忽略)
-callback (close(Reason::any(), State) -> 
  {ok, State} | 
  {reply, Message::binary()} | 
  {reply, Message::binary(), State} | 
  any() 
  when State::any()).
-optional_callbacks([close/2]).

%% 服务结束
-callback (terminate(Reason::any(), State) -> ok when State::any()).
-optional_callbacks([terminate/2]).
```

然后调用
```
%% 启动服务
-spec(start_server(Ref :: any(), Opt :: list()) ->
  {ok, pid()} |
  {error, already_present | {already_started, pid()}}).

  Opt可以配置的参数：
  module    - 回调模块
  shutdown  - 进程延时关闭时间(毫秒),默认0
  timeout   - 网络进程待机时间(毫秒),默认60000
  msg_type  - 返回消息类型: text | binary,默认text
  port      - 服务端口,例如:9999
  host      - 域,例如:'_' (参照cowboy路由设置)
  path      - 路径,例如:"/path" (参照cowboy路由设置)
```

关闭服务可以调用
```
%% 停止服务
-spec(stop_server(Ref :: any()) -> ok | {error, not_found | timeout}).
```
