%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat client. This module defines a chat client.
%%%     In this version:
%%%         a) It uses the message_router API "send_chat_message" to send a message.
%%% @end
%%%----------------------------------------------------------------------------------


-module(chat_client).

%%
%% Include files
%%

%%
%% Exported functions
%%
-compile(export_all).

%%
%% API functions
%%
send_message(RouterPid, Addressee, MessageBody) ->
    message_router:send_chat_message(RouterPid, Addressee, MessageBody).

%%
%% Local functions
%%
