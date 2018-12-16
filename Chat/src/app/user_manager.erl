%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 十二月 2018 下午 03:05
%%%-------------------------------------------------------------------

-module(user_manager).
-author("Rxsi").
-compile(export_all).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("user_info.hrl").
-include("proto.hrl").
-record(state, {datalist = []}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

%<<-------------------------回调函数----------------------------->>
handle_call({new_account, Username, Passwd}, _From, State) ->
  Reply = case state_manager:get_from_name(Username, State) of
            false ->
              {ok, NewState} = state_manager:register_user(Username, Passwd, State),
              "success";
            _ ->
              NewState = State,
              "fail"
          end,
  {reply, Reply, NewState};

handle_call({login, Username, Passwd, Socket}, _From, State) ->
  Reply = case state_manager:get_from_name(Username, State) of
            false ->
              NewState = State,
              "fail";
            {user, Username2, Passwd2, Socket2, Roomnumber, Owner, Talk} ->
              if
                Passwd2 =:= Passwd ->
                  {ok, NewState} = state_manager:set_socket(Username, Socket, State);
                true -> NewState = State
              end,
              "success"
          end,
  {reply, Reply, NewState};

handle_call({create_room, Username, Roomnumber}, _From, State) ->
  Reply = case gen_server:call(room_host, {existence, Roomnumber}) of
            "inexistence" ->
              {ok, NewState} = state_manager:create_room(Username, Roomnumber, State),
              "success";
            _ ->
              NewState = State,
              "fail"
          end,
  {reply, Reply, NewState};

handle_call({enter_room, Username, Roomnumber}, _From, State) ->
  Reply = case gen_server:call(room_manager, {check_room, Roomnumber}) of
            "empty" ->
              NewState = State,
              "fail";
            _ ->
              {ok, NewState} = state_manager:enter_room(Username, Roomnumber, State),
              "success"
          end,
  {reply, Reply, NewState};

handle_call({msg, Username}, _From, State) ->
  Reply = case state_manager:get_from_name(Username, State) of
            false ->
              "fail";
            {user, Username2, Passwd, Socket, Roomnumber, Owner, Talk} ->
              if
                Socket =/= "", Talk =:= 1 -> "success";
                Roomnumber =/= 00, Talk =:= 0 -> "silent";
                true -> "success"
              end
          end,
  {reply, Reply, State};

handle_call({sendAllMessage, Name, Msg}, _From, State) ->
  #state{datalist = DataList} = State,
  {user, Username1, Passwd1, Socket1, Roomnumber1, Owner1, Talk1} = state_manager:get_from_name(Name, State),
  Socketlist = [{user, Username2, Passwd2, Socket2, Roomnumber2, Owner2, Talk2} || {user, Username2, Passwd2, Socket2,
    Roomnumber2, Owner2, Talk2} <- DataList, Username2 =/= Name, Roomnumber2 =:= Roomnumber1],
  lists:foreach(
    fun({user, Username3, Passwd3, Socket3, Roomnumber3, Owner3, Talk3}) ->
      {ok,Packet}=packet:sendAll(?SENDALL,[Name,Msg]),
      gen_tcp:send(Socket3, Packet)
    end,
    Socketlist
  ),
  {reply, ok, State};

handle_call({silent, Username1, Username2}, _From, State) ->
  Reply = if
            Username1 =:= Username2 ->
              NewState = State,
              "cant";
            true ->
              case state_manager:get_from_name(Username1, State) of
                false ->
                  NewState = State,
                  "fail";
                {user, Username3, Passwd, Socket, Roomnumber, Owner, Talk1} ->
                  if
                    Roomnumber =:= Owner ->
                      Talk = 0,
                      {ok, NewState} = state_manager:set_talk(Username2, Talk, State),
                      "success";
                    true ->
                      NewState = State,
                      "fail"
                  end
              end
          end,
  {reply, Reply, NewState};

handle_call({unsilent, Username1, Username2}, _From, State) ->
  Reply = if
            Username1 =:= Username2 ->
              NewState = State,
              "cant";
            true ->
              case state_manager:get_from_name(Username1, State) of
                false ->
                  NewState = State,
                  "fail";
                {user, Username3, Passwd, Socket, Roomnumber, Owner, Talk1} ->
                  if
                    Roomnumber =:= Owner ->
                      Talk = 1,
                      {ok, NewState} = state_manager:set_talk(Username2, Talk, State),
                      "success";
                    true ->
                      NewState = State,
                      "fail"
                  end
              end
          end,
  {reply, Reply, NewState};

handle_call({kick, Username1, Username2}, _From, State) ->
  Reply = if
            Username1 =:= Username2 ->
              NewState = State,
              "cant";
            true ->
              case state_manager:get_from_name(Username1, State) of
                false ->
                  NewState = State,
                  "fail";
                {user, Username3, Passwd, Socket, Roomnumber, Owner, Talk1} ->
                  if
                    Roomnumber =:= Owner ->
                      io:format("~p,~p~n", [Roomnumber, Owner]),
                      {ok, NewState} = state_manager:set_roomnumber(Username2, State),
                      "success";
                    true ->
                      NewState = State,
                      "fail"
                  end
              end
          end,
  {reply, Reply, NewState};

handle_call({grant, Username1, Username2}, _From, State) ->
  Reply = if
            Username1 =:= Username2 ->
              NewState = State,
              "cant";
            true ->
              case state_manager:get_from_name(Username1, State) of
                false ->
                  NewState = State,
                  "fail";
                {user, Username3, Passwd, Socket, Roomnumber, Owner2, Talk} ->
                  if
                    Owner2 =:= Roomnumber ->
                      {ok, NewState} = state_manager:set_owner(Username1, Username2, Owner2, State),
                      "success";
                    true ->
                      NewState = State,
                      "fail"
                  end
              end
          end,
  {reply, Reply, NewState};

handle_call({change_owner, Username, Roomnumber}, _From, State) ->
  Reply = case state_manager:get_from_owner(Roomnumber, State) of
            false ->
              NewState = State,
              "fail";
            {user, Username1, Passwd, Socket, Roomnumber1, Owner, Talk} ->
              {ok, NewState} = state_manager:set_owner(Username1, Username, Roomnumber, State),
              "success"
          end,
  {reply, Reply, NewState};

handle_call({exit_room, Username}, _From, State) ->
  Reply = case state_manager:get_from_name(Username, State) of
            false ->
              NewState = State,
              "fail";
            {user, Username1, Passwd, Socket, Roomnumber, Owner, Talk} ->
              if
                Roomnumber =/= 00 ->
                  {ok, NewState} = state_manager:set_roomnumber(Username, State),
                  {"success", Roomnumber};
                true ->
                  NewState = State,
                  "fail"
              end
          end,
  {reply, Reply, NewState}.

handle_cast({sendback, Socket, State, Message}, Tab) ->
  N = term_to_binary(Message),
  Packet = <<State:16, (byte_size(N)):16, N/binary>>,
  gen_tcp:send(Socket, Packet),
  {noreply, Tab}.

handle_info(_Info, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
