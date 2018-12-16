%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 十一月 2018 下午 01:46
%%%-------------------------------------------------------------------
-author("Rxsi").
-record(
user, {
  username = "", passwd = "", socket = "", roomnumber = 00, owner = 1, talk = 1
}
).
