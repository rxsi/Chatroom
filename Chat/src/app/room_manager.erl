%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 十二月 2018 18:23
%%%-------------------------------------------------------------------
-module(room_manager).
-author("Rxsi").
-behaviour(gen_server).
-compile(export_all).
-define(SERVER, ?MODULE).
-include("room_info.hrl").
-include("room_host.hrl").
-record(state, {datalist = []}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({add_room, Username, Roomnumber}, _From, State) ->
  {ok, NewState} = state_manager:add_room(Username, Roomnumber, State),
  {reply, ok, NewState};

handle_call({delete_room, Username}, _From, State) ->
  {ok, NewState} = state_manager:delete_room(Username, State),
  {reply, ok, NewState};

handle_call({check_room, Roomnumber}, _From, State) ->
  Reply = case state_manager:check_room(Roomnumber, State) of
            "empty" ->
              "empty";
            _ ->
              "fail"
          end,
  {reply, Reply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

