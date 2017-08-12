%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat client. This module defines a chat client.
%%%     In this version:
%%%         a) Prints the message received (instead of the message_router
%%%             printing it - as done in chat-1 app)
%%%         b) Starting the message_router and passing it a reference to the 
%%%             print_message function (not the best approach since clients should
%%%             not be starting the message_router/server. We'll refactor this 
%%%             later). The code here is to demo "passing function references as 
%%%             arguments to other functions - higher order functions".
%%%         c) Took out RouterPid from send_message (corresponding to change in 
%%%             message_router - registered process). Now we will not need to 
%%%             know the Pid of the message_router since it's a registered process.
%%%             We can just invoke the 
%%%             message_router:send_chat_message(Addressee, MessageBody) function
%%%             and the erlang runtime will send the message to the message_router
%%%             (registered process).
%%%             NOTE: We will still need to pid of the message_router since the
%%%             message_router currently only works for single client (which is
%%%             also the addressee and the message_router delegates the printing
%%%             to the client that sent the message). This will be refactored when
%%%             the message_router begins keeping track of various chat_client pids
%%%             in a dictionary.
%%%
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
send_message(Addressee, MessageBody) ->
    message_router:send_chat_message(Addressee, MessageBody).

%% start the router and pass in the print_message function reference
start_router() ->
    message_router:start(fun chat_client:print_message/1).

%%
%% Local functions
%%
print_message(MessageBody) ->
    io:format("chat_client: Message received: ~p~n", [MessageBody]).