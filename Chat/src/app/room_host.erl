%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 十二月 2018 9:41
%%%-------------------------------------------------------------------
-module(room_host).
-author("Rxsi").
-behaviour(gen_server).
-compile(export_all).
-define(SERVER, ?MODULE).
-include("room_host.hrl").
-record(state, {datalist = []}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({add_host, Roomname, Roomnumber}, _From, State) ->
  {ok, NewState} = state_manager:add_host(Roomname, Roomnumber, State),
  {reply, ok, NewState};

handle_call({destroy, Roomname}, _From, State) ->
  {ok, NewState} = state_manager:destroy_room(Roomname, State),
  {reply, ok, NewState};

handle_call({existence, Roomnumber}, _From, State) ->
  {ok, Reply} = state_manager:existence_room(Roomnumber, State),
  {reply, Reply, State};

handle_call({search}, _From,State) ->
  Reply = state_manager:search_room(State),
  {reply,Reply,State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
