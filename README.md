= game_ws

An websocket library for special game

== Usage

使用的时候需要先设置回调模块
```
-type call_result(State) :: {ok, State}
| {reply, binary(), State}
| {stop, State}.

%% @doc 初始化
-callback init(Req::map(), pid()) -> {ok, State::any()} | {reply, Reply::binary()}
| {error, {already_started, pid()} | term()}.

函数传入的pid可用来支持以下调用：
%% 发送信息
-spec(send_msg(Server :: pid(), Msg :: binary()) -> ok).
%% 关闭连接
-spec(disconnect(Server :: pid()) -> ok).
%% 自定义控制
-spec(control(Server :: pid(), Control :: any()) -> ok).

%% @doc 控制信息处理
-callback control(Control::term(), State) -> call_result(State) when State::any().
Control包含内置的：
ping                   --- 心跳
{ping, Msg::binary()}  --- 心跳
reconnect              --- 重连
其它可以自定义(通过control/2函数)

%% @doc 消息处理
-callback handle(binary(), State) -> call_result(State) when State::any().

%% @doc 进程关闭
-callback terminate(Reason::any(), State) -> ok when State::any().
-optional_callbacks([terminate/2]).
```

然后调用
```
%% 启动服务
-spec(start_server(Ref :: any(), Opt :: list()) ->
  {ok, pid()} | {ok, pid(), Info :: term()} |
  {error, undefined_module | bad_arg | already_present | {already_started, pid()}}). 

  Opt可以配置的参数：
  module    - 回调模块
  shutdown  - 进程延时关闭时间(毫秒)
  port      - 服务端口,例如:9999
  host      - 域,例如:'_' (参照cowboy路由设置)
  path      - 路径,例如:"/path" (参照cowboy路由设置)
```

关闭服务可以调用
```
%% 停止服务
-spec(stop_server(Ref :: any()) -> ok | {error, not_found | timeout}).
```
