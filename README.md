game_ws
=====

An websocket library for special game

Usage
-----

使用的时候需要先设置回调模块
```
-type call_result(State) :: {ok, State}
| {reply, binary(), State}
| {stop, State}.

%% 初始化进程内存
-callback (init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary(), State::any()}
| {already_started, Handler::pid()} | {already_started, Handler::pid(), Event::term()}
| {error, Error::term()}).

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

%% 结束通信
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
  shutdown  - 进程延时关闭时间(秒)
  port      - 服务端口,例如:9999
  host      - 域,例如:'_' (参照cowboy路由设置)
  path      - 路径,例如:"/path" (参照cowboy路由设置)
```

关闭服务可以调用
```
%% 停止服务
-spec(stop_server(Ref :: any()) -> ok | {error, not_found | timeout}).
```
