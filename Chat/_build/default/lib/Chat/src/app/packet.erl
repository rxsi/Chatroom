%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2018 13:43
%%%-------------------------------------------------------------------
-module(packet).
-author("Rxsi").
-compile(export_all).
-include("proto.hrl").

client(?NEW, [Id, Password]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(Password),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?NEW:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?LOGIN, [Id, Password]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(Password),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?LOGIN:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?SILENT, [Name1, Name2]) ->
  Data1 = term_to_binary(Name1),
  Data2 = term_to_binary(Name2),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?SILENT:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?UNSILENT, [Name1, Name2]) ->
  Data1 = term_to_binary(Name1),
  Data2 = term_to_binary(Name2),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?UNSILENT:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?KICK, [Name1, Name2]) ->
  Data1 = term_to_binary(Name1),
  Data2 = term_to_binary(Name2),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?KICK:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?GRANT, [Name1, Name2]) ->
  Data1 = term_to_binary(Name1),
  Data2 = term_to_binary(Name2),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?GRANT:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?NEWROOM, [Id, RoomNumber]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(RoomNumber),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?NEWROOM:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?ENTERROOM, [Id, RoomNumber]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(RoomNumber),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?ENTERROOM:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?EXITROOM, [Id]) ->
  Data1 = term_to_binary("exit"),
  Data2 = term_to_binary(Id),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?EXITROOM:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet};

client(?SEND, [Id, Message]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(Message),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?SEND:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet}.

client(?SEARACH) ->
  Data1 = term_to_binary("search"),
  Data2 = term_to_binary("room"),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?SEARACH:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet}.

sendAll(?SENDALL,[Id,Message]) ->
  Data1 = term_to_binary(Id),
  Data2 = term_to_binary(Message),
  Len1 = byte_size(Data1),
  Len2 = byte_size(Data2),
  Packet = <<?SENDALL:16, Len1:16, Data1/binary, Len2:16, Data2/binary>>,
  {ok, Packet}.



