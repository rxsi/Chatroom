%%%-------------------------------------------------------------------
%%% @author Rxsi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 十二月 2018 11:34
%%%-------------------------------------------------------------------
-author("Rxsi").

%%--------------User Manager-------
-define(NEW, 0000).%创建用户
-define(LOGIN, 0001).%用户登录
-define(SILENT, 0002).%禁言
-define(UNSILENT, 0003).%解禁
-define(KICK, 0004).%踢人
-define(GRANT, 0005).%赋予权限
-define(NEWROOM, 0006).%创建新房间
-define(ENTERROOM, 0007).%进入房间
-define(EXITROOM, 0008).%退出房间
-define(SEND, 0009).%发送消息
-define(SENDALL,0010).%群发消息
-define(SEARACH,0011).%搜索房间
