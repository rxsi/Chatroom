%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 十一月 2018 11:12
%%%-------------------------------------------------------------------
-module(chat_client).
-author("Rxsi").
-define(SERVER, ?MODULE).
-compile(export_all).
-include("socket_info.hrl").
-include("proto.hrl").
-record(state, {datalist = []}).

start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  get_socket(),
  {ok, #state{}}.

%获取socket
get_socket() ->
  register(client, spawn(fun() -> {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 0}]),
    handle(Socket) end)).
%----------------------自动压测----------------------
%自动压测
%显示最大连接数: 8187
%单纯测试最大连接数:当使用test(),会在num=8187时触发system_limit,而使用spawn(fun()-> test() end)时,不会触发,
% 且在server端会发现触发了 {tcp_closed, Socket} -> io:format(......) 语句,貌似进入了无限关闭和创建的状态.
test() ->
  case gen_tcp:connect("localhost", 2345, [binary, {packet, 0}]) of
    {ok, Socket} -> Socket;
    {error, Reason} -> Reason
  end.

auto_test(Num) ->
  String = string:concat("atom_", integer_to_list(Num)),
  Atom = list_to_atom(String),
  register(Atom, spawn(fun() -> {ok, Socket} = gen_tcp:connect("localhost", 2345, [binary, {packet, 0}]),
    handle(Socket) end)),
  case auto_account(Atom, Num, Num + 1) of
    {"create successfully "} ->
      io:format("~p  create success~n", [Num]),
      case auto_login(Atom, Num, Num + 1) of
        {"you have login successfully "} ->
          io:format("~p  login success~n", [Num]),
          case auto_enter(Atom, 123) of
            {"now you in the room "} ->
              io:format("~p  enter success ~n", [Num]);
            _ ->
              io:format("fail ~n")
          end;
        _ ->
          io:format("login fail~n")
      end;
    _ ->
      io:format("create fail~n")
  end,
  auto_test(Num + 1).

%-----------------------自动压测------------------------
%<<-------------------------回调函数----------------------------->>
%登录的时候添加socket
handle_cast({addSocket, Username, Socket}, State) ->
  {ok, NewState} = state_manager:set_client_state(Username, Socket, State),
  {noreply, NewState};

%退出的时候，删除socket
handle_cast({deleteSocket, Username, Socket}, State) ->
  NewState = state_manager:delete_client_name(Username, State),
  {noreply, NewState}.

handle_call({get, Socket}, _From, State) ->
  Reply = case state_manager:get_client_socket(Socket, State) of
            false -> "not the socket";
            {socket, A, B} -> A
          end,
  {reply, Reply, State}.

%<<-------------------------回调函数----------------------------->>
%登录接口
login(Name, Password) ->
  client ! {self(), {login, Name, Password}},
  receive
    Response -> Response
  end.

auto_login(Atom, Name, Password) ->
  Atom ! {self(), {login, Name, Password}},
  receive
    Response -> Response
  end.

%聊天发送接口
send_message(Msg) ->
  client ! {self(), {msg, Msg}},
  receive
    Response -> Response
  end.

auto_message(Atom, Msg) ->
  Atom ! {self(), {msg, Msg}},
  receive
    Response -> Response
  end.

%创建房间
create_room(Number) ->
  client ! {self(), {create, Number}},
  receive
    Response -> Response
  end.

%创建用户
new_account(Name, Password) ->
  client ! {self(), {new, Name, Password}},
  receive
    Response -> Response
  end.

auto_account(Atom, Name, Password) ->
  Atom ! {self(), {new, Name, Password}},
  receive
    Response -> Response
  end.

%加入房间
enter_room(Number) ->
  client ! {self(), {enter, Number}},
  receive
    Response -> Response
  end.

auto_enter(Atom, Number) ->
  Atom ! {self(), {enter, Number}},
  receive
    Response -> Response
  end.

%禁言他人
silent(Name) ->
  client ! {self(), {silent, Name}},
  receive
    Response -> Response
  end.

%解除禁言
unsilent(Name) ->
  client ! {self(), {unsilent, Name}},
  receive
    Response -> Response
  end.

%踢人
kick(Name) ->
  client ! {self(), {kick, Name}},
  receive
    Response -> Response
  end.

%给别人房主
grant(Name) ->
  client ! {self(), {grant, Name}},
  receive
    Response -> Response
  end.

%退出房间
exit_room() ->
  client ! {self(), {exit}},
  receive
    Response -> Response
  end.

%退出接口
logout(Name) ->
  client ! {self(), {logout, Name}},
  receive
    Response -> Response
  end.

search_room() ->
  client ! {self(),{search}},
  receive
    Response -> Response
  end.

handle(Socket) ->
  receive
  %来自控制进程的请求
    {From, Request} ->
      case Request of
        {new, Name, Password} ->
          {ok, Packet} = packet:client(?NEW, [Name, Password]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin, %状态码
              <<Size1:16, Date1/binary>> = Date,  %登录成功信息长度
              case binary_to_term(Date1) of
                "success" ->
                  From ! {"create successfully "};
                "fail" ->
                  From ! {"fail "}
              end
          end,
          handle(Socket);

        {login, Name, Password} ->
          {ok, Packet} = packet:client(?LOGIN, [Name, Password]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              case binary_to_term(Date1) of
                "success" ->
                  gen_server:cast(?SERVER, {addSocket, Name, Socket}),
                  From ! {"you have login successfully "};
                "fail" ->
                  From ! {"you haved login failed,please try again "}
              end
          after 5000 ->
            io:format("overTime1 ~n")
          end,
          handle(Socket);

        {silent, Name} ->
          Name1 = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?SILENT, [Name1, Name]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"Silent success "};
                "fail" -> From ! {"you are not the owner "};
                "cant" -> From ! {"you can't silent yourself"}
              end
          end,
          handle(Socket);

        {unsilent, Name} ->
          Name1 = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?UNSILENT, [Name1, Name]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"unSilent success "};
                "fail" -> From ! {"you are not the owner "};
                "cant" -> From ! {"you can't unsilent yourself"}
              end
          end,
          handle(Socket);

        {kick, Name} ->
          Name1 = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?KICK, [Name1, Name]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"kick success "};
                "fail" -> From ! {"you are not the owner "};
                "cant" -> From ! {"you can't kick yourself"}
              end
          end,
          handle(Socket);

        {grant, Name} ->
          Name1 = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?GRANT, [Name1, Name]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"success "};
                "fail" -> From ! {"you are not the owner "};
                "cant" -> From ! {"you can't grant yourself"}
              end
          end,
          handle(Socket);

        {create, Number} ->
          Name = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?NEWROOM, [Name, Number]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"create success "};
                "fail" -> From ! {"create fail "}
              end
          end,
          handle(Socket);

        {enter, Number} ->
          Name = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?ENTERROOM, [Name, Number]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin, %状态码
              <<Size1:16, Date1/binary>> = Date,  %消息长度
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" ->
                  From ! {"now you in the room "};
                "fail" ->
                  From ! {"the room is not existence "}
              end
          after 5000 ->
            ok
          end,
          handle(Socket);

        {exit} ->
          Name = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?EXITROOM, [Name]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin,
              <<Size1:16, Date1/binary>> = Date,
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                "success" -> From ! {"exit success "};
                "fail" -> From ! {"you are not in the room "}
              end
          end,
          handle(Socket);

        {msg, Msg} ->
          Name = gen_server:call(?MODULE, {get, Socket}),
          {ok, Packet} = packet:client(?SEND, [Name, Msg]),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              <<State:16, Date/binary>> = Bin, %状态码
              <<Size1:16, Date1/binary>> = Date,  %消息长度
              ReceiveMsg = binary_to_term(Date1),
              case ReceiveMsg of
                {"ok", "received"} ->
                  From ! {"send success"};
                {"failed", "noLogin"} ->
                  From ! {"you don't have  logined "};
                {"silent"} ->
                  From ! {"you are silented "};
                Fail ->
                  io:format(" ~p~n", [ReceiveMsg]),
                  From ! {"failed "}
              end
          after 3000 ->
            io:format("overTime2 ~n")
          end,
          handle(Socket);

        {search} ->
          {ok, Packet} = packet:client(?SEARACH),
          gen_tcp:send(Socket, Packet),
          receive
            {tcp, Socket, Bin} ->
              ReceiveMsg = binary_to_term(Bin),
              io:format("The roomlists: ~w~n",[ReceiveMsg]),
              From ! {"success"}
          end,
          handle(Socket)
      end;

    {tcp, Socket, Bin} ->
      <<State:16, Date/binary>> = Bin, %状态码
      <<Size1:16, Date1/binary>> = Date,  %用户长度
      <<User:Size1/binary, Date2/binary>> = Date1,    %用户
      <<Size2:16, Date3/binary>> = Date2,  %消息的长度
      <<Msg:Size2/binary, Date4/binary>> = Date3, %消息
      io:format("~p : ~p~n", [binary_to_term(User), binary_to_term(Msg)]),
      handle(Socket);

    {tcp_closed, Socket} ->
      io:format("receive server don't accept connection!~n")
  end.
