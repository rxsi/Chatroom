%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 十一月 2018 15:47
%%%-------------------------------------------------------------------

-module(chat_server).
-author("Rxsi").
-compile(export_all).
-define(SERVER, ?MODULE).
-include("user_info.hrl").
-include("proto.hrl").
-record(state, {datalist = []}).

%-----------------初始函数-----------------
start() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  start_parallel_server(),
  user_manager:start_link(),
  room_manager:start_link(),
  room_host:start_link(),
  {ok, #state{}}.

start_parallel_server() ->
  {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]),
  spawn(fun() -> per_connect(Listen) end).

per_connect(Listen) ->
  {ok, Socket} = gen_tcp:accept(Listen),
  spawn(fun() -> per_connect(Listen) end),
  loop(Socket).

%-----------------初始函数----------------
handle_call(_Requset, _From, State) ->
  {reply, ok, State}.
handle_cast(_Request, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%-----------------房间销毁----------------
ready_destroy(Number) ->
  io:format("ready_destroy~n"),
  StringNumber = integer_to_list(Number),
  Atom = string:concat("Room_", StringNumber),
  AnAtom = list_to_atom(Atom),
  register(AnAtom, spawn(fun() -> countdown(Number) end)).

countdown(Number) ->
  {ok, TRef} = timer:apply_after(10000, ?MODULE, real_destroy, [Number]),
  io:format("countdown~n"),
  receive
    "cancel" ->
      timer:cancel(TRef)
  after 18000 ->
    void
  end.

real_destroy(Number) ->
  StringNumber = integer_to_list(Number),
  Atom = string:concat("Room_", StringNumber),
  AnAtom = list_to_atom(Atom),
  gen_server:call(room_host, {destroy, AnAtom}),
  io:format("room:~p has been destroy~n", [Number]),
  unregister(AnAtom).
%------------------房间销毁---------------------

%---------------------sendback函数--------------------
sendback(Socket, State, Message) ->
  gen_server:cast(user_manager, {sendback, Socket, State, Message}).
%---------------------sendback函数----------------------

%-----------------接收函数-----------------
loop(Socket) ->
  io:format("<--------receiving the message-------->~n"),
  receive
    {tcp, Socket, Bin} ->
      <<State:16, Date/binary>> = Bin, %将前4个字节作为状态码,其余部分作为二进制类型保存在Date中
      <<Size1:16, Date1/binary>> = Date,  %Size1第一个信息的长度的二进制格式
      <<Str1:Size1/binary, Date2/binary>> = Date1,%Str1第一个信息的二进制格式
      <<Size2:16, Date3/binary>> = Date2,  %Size2第二个信息的长度的二进制格式
      <<Str2:Size2/binary, Date4/binary>> = Date3,%Str2第二个信息的二进制格式

      case State of
        %创建用户
        ?NEW ->
          Username1 = binary_to_term(Str1),
          Password = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {new_account, Username1, Password}),
          case Reply of
            "success" ->
              io:format("User: ~p create successfully~n",[Username1]),
              Message2 = "success",
              sendback(Socket, State, Message2),
              loop(Socket);
            "fail" ->
              io:format("User: ~p create failed~n",[Username1]),
              Message2 = "fail",
              sendback(Socket, State, Message2),
              loop(Socket)
          end;

        %用户登录
        ?LOGIN ->
          Name = binary_to_term(Str1),
          Password = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {login, Name, Password, Socket}),
          case Reply of
            "success" ->
              Message = "success",
              sendback(Socket, State, Message),
              io:format("~p has logged~n", [Name]),
              loop(Socket);
            "fail" ->
              io:format("~p logged failed~n", [Name]),
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %禁言操作
        ?SILENT ->
          Name1 = binary_to_term(Str1),
          Name2 = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {silent, Name1, Name2}),
          case Reply of
            "success" ->
              io:format("~p silent ~p successfully~n",[Name1,Name2]),
              Message = "success",
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              Message = "fail",
              io:format("~p silent ~p failed~n",[Name1,Name2]),
              sendback(Socket, State, Message),
              loop(Socket);
            "cant" ->
              Message = "cant",
              io:format("~p silent ~p failed~n",[Name1,Name2]),
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %解除禁言
        ?UNSILENT ->
          Name1 = binary_to_term(Str1),
          Name2 = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {unsilent, Name1, Name2}),
          case Reply of
            "success" ->
              io:format("~p unsilent ~p successfully~n",[Name1,Name2]),
              Message = "success",
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              io:format("~p unsilent ~p failed~n",[Name1,Name2]),
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket);
            "cant" ->
              io:format("~p unsilent ~p failed~n",[Name1,Name2]),
              Message = "cant",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %踢人操作
        ?KICK ->
          Name1 = binary_to_term(Str1),
          Name2 = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {kick, Name1, Name2}),
          case Reply of
            "success" ->
              io:format("~p kick ~p successfully~n",[Name1,Name2]),
              Message = "success",
              gen_server:call(room_manager, {delete_room, Name2}),
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              io:format("~p kick ~p failed~n",[Name1,Name2]),
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket);
            "cant" ->
              io:format("~p kick ~p failed~n",[Name1,Name2]),
              Message = "cant",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %过让房主
        ?GRANT ->
          Name1 = binary_to_term(Str1),
          Name2 = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {grant, Name1, Name2}),
          case Reply of
            "success" ->
              io:format("~p grant ~p successfully~n",[Name1,Name2]),
              Message = "success",
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              io:format("~p grant ~p failed~n",[Name1,Name2]),
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket);
            "cant" ->
              io:format("~p grant ~p failed~n",[Name1,Name2]),
              Message = "cant",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %创建房间
        ?NEWROOM ->
          Name = binary_to_term(Str1),
          Number = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {create_room, Name, Number}),
          case Reply of
            "success" ->
              io:format("~p create the room: ~p~n ",[Name,Number]),
              StringNumber = integer_to_list(Number),
              Atom = string:concat("Room_", StringNumber),
              AnAtom = list_to_atom(Atom),
              gen_server:call(room_host, {add_host, AnAtom, Number}),
              gen_server:call(room_manager, {add_room, Name, Number}),
              Message = "success",
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %进入房间
        ?ENTERROOM ->
          Name = binary_to_term(Str1),
          Number = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {enter_room, Name, Number}),
          case Reply of
            "success" ->
              io:format("~p enter the room ~p~n ",[Name,Number]),
              case gen_server:call(room_manager, {check_room, Number}) of
                "empty" ->
                  case gen_server:call(room_host, {existence, Number}) of
                    "existence" ->
                      StringNumber = integer_to_list(Number),
                      Atom = string:concat("Room_", StringNumber),
                      AnAtom = list_to_atom(Atom),
                      AnAtom ! "cancel",
                      gen_server:call(user_manager, {change_owner, Name, Number});
                    _ ->
                      true
                  end;
                _ ->
                  true
              end,
              gen_server:call(room_manager, {add_room, Name, Number}),
              Message = "success",
              sendback(Socket, State, Message),
              loop(Socket);
            "fail" ->
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %退出房间
        ?EXITROOM ->
          Name = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {exit_room, Name}),
          case Reply of
            {"success", Number} ->
              io:format("~p exit the room ~p ~n",[Name,Number]),
              Message = "success",
              gen_server:call(room_manager, {delete_room, Name}),
              case gen_server:call(room_manager, {check_room, Number}) of
                "empty" ->
                  case gen_server:call(room_host, {existence, Number}) of
                    "existence" ->
                      ready_destroy(Number);
                    _ ->
                      true
                  end;
                _ ->
                  true
              end,
              sendback(Socket, State, Message),
              loop(Socket);
            _ ->
              Message = "fail",
              sendback(Socket, State, Message),
              loop(Socket)
          end;

        %发送信息
        ?SEND ->
          Name = binary_to_term(Str1),
          Msg = binary_to_term(Str2),
          Reply = gen_server:call(user_manager, {msg, Name}),
          case Reply of
            "fail" ->
              Message = {"failed", "noLogin"},
              sendback(Socket, State, Message),
              io:format("user ~p  no login", [Name]),
              loop(Socket);
            "silent" ->
              Message = {"silent"},
              sendback(Socket, State, Message),
              loop(Socket);
            "success" ->
              Message = {"ok", "received"},
              sendback(Socket, State, Message),
              io:format("message : ~p ~n", [Msg]),
              gen_server:call(user_manager, {sendAllMessage, Name, Msg}),
              loop(Socket)
          end;

        %房间搜索
        ?SEARACH ->
          Reply = gen_server:call(room_host,{search}),
          Packet=term_to_binary(Reply),
          gen_tcp:send(Socket,Packet),
          loop(Socket)
      end;

    {tcp_closed, Socket} ->
      io:format("Server socket closed~n")
  end.

