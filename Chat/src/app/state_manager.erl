%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2018 16:42
%%%-------------------------------------------------------------------
-module(state_manager).
-author("Rxsi").
-include("user_info.hrl").
-include("room_info.hrl").
-include("socket_info.hrl").
-include("room_host.hrl").
-record(state, {datalist = []}).
-compile(export_all).

%--------------client:server:state操作函数-------------
get_from_name(Name, #state{datalist = DataList} = State) ->
  lists:keyfind(Name, #user.username, DataList).

get_from_socket(Socket, #state{datalist = DataList} = State) ->
  lists:keyfind(Socket, #user.socket, DataList).

get_from_owner(Owner, #state{datalist = DataList} = State) ->
  lists:keyfind(Owner, #user.owner, DataList).

register_user(Name, Passwd, #state{datalist = DataList} = State) ->
  NewDataList = lists:keystore(Name, #user.username, DataList, #user{username = Name, passwd = Passwd}),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

set_socket(Name, Socket, #state{datalist = DataList} = State) ->
  NewDataList = case lists:keyfind(Name, #user.username, DataList) of
                  #user{} = User ->
                    lists:keystore(Name, #user.username, DataList, User#user{socket = Socket});
                  _ ->
                    DataList
                end,
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

set_talk(Name, Talk, #state{datalist = DataList} = State) ->
  NewDataList = case lists:keyfind(Name, #user.username, DataList) of
                  #user{} = User ->
                    lists:keystore(Name, #user.username, DataList, User#user{talk = Talk});
                  _ ->
                    DataList
                end,
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

set_roomnumber(Name, #state{datalist = DataList} = State) ->
  NewDataList = case lists:keyfind(Name, #user.username, DataList) of
                  #user{} = User ->
                    lists:keystore(Name, #user.username, DataList, User#user{roomnumber = 00});
                  _ ->
                    DataList
                end,
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

set_owner(Name1, Name2, Owner, #state{datalist = DataList} = State) ->
  NewDataList1 = case lists:keyfind(Name1, #user.username, DataList) of
                   #user{} = User ->
                     lists:keystore(Name1, #user.username, DataList, User#user{owner = 1});
                   _ ->
                     DataList
                 end,

  NewDataList2 = case lists:keyfind(Name2, #user.username, NewDataList1) of
                   #user{} = User2 ->
                     lists:keystore(Name2, #user.username, NewDataList1, User2#user{owner = Owner});
                   _ ->
                     NewDataList1
                 end,
  NewState = State#state{datalist = NewDataList2},
  {ok, NewState}.

create_room(Name, Roomnumber, #state{datalist = DataList} = State) ->
  NewDataList = case lists:keyfind(Name, #user.username, DataList) of
                  #user{} = User ->
                    lists:keystore(Name, #user.username, DataList, User#user{username = Name, roomnumber =
                    Roomnumber, owner = Roomnumber});
                  _ ->
                    DataList
                end,
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

enter_room(Name, Roomnumber, #state{datalist = DataList} = State) ->
  NewDataList = case lists:keyfind(Name, #user.username, DataList) of
                  #user{} = User ->
                    lists:keystore(Name, #user.username, DataList, User#user{username = Name, roomnumber = Roomnumber});
                  _ ->
                    DataList
                end,
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

%-------------------room_manager:state操作函数-----------------------
add_room(Username, Roomnumber, #state{datalist = DataList} = State) ->
  NewDataList = lists:keystore(Username, #room.username, DataList, #room{username =
  Username, roomnumber = Roomnumber}),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

delete_room(Username, #state{datalist = DataList} = State) ->
  NewDataList = lists:keydelete(Username, #room.username, DataList),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

check_room(Roomnumber, #state{datalist = DataList} = State) ->
  NewDataList = [{room, Username, Number} || {room, Username, Number} <- DataList,
    Number =:= Roomnumber],
  case length(NewDataList) of
    0 ->
      "empty";
    _ ->
      "not empty"
  end.

%-------------------chat_client:state-------------------------
get_client_socket(Socket, #state{datalist = DataList} = State) ->
  lists:keyfind(Socket, #socket.socket, DataList).

set_client_state(Name, Socket, #state{datalist = DataList} = State) ->
  NewDataList = lists:keystore(Name, #socket.username, DataList, #socket{username =
  Name, socket = Socket}),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

delete_client_name(Name, #state{datalist = DataList} = State) ->
  #state{datalist = lists:keydelete(Name, #socket.username, DataList)}.

%--------------------room_host:state------------------------
add_host(Roomname, Roomnumber, #state{datalist = DataList} = State) ->
  NewDataList = lists:keystore(Roomnumber, #host.roomnumber, DataList, #host{roomname =
  Roomname, roomnumber = Roomnumber}),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.

existence_room(Roomnumber, #state{datalist = DataList} = State) ->
  Reply = case lists:keyfind(Roomnumber, #host.roomnumber, DataList) of
            false ->
              "inexistence";
            _ ->
              "existence"
          end,
  {ok, Reply}.

destroy_room(Roomname, #state{datalist = DataList} = State) ->
  NewDataList = lists:keydelete(Roomname, #host.roomname, DataList),
  NewState = State#state{datalist = NewDataList},
  {ok, NewState}.



search_room(#state{datalist = DataList} = State) ->
  NewDataList=[Room#host.roomnumber || Room <- DataList].