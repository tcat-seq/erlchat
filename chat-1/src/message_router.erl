%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat message router. This module defines a process that routes messages.
%%%         In this version:
%%%             a) 1st draft of message router
%%%             b) routes messages between processes
%%%             c) exposes start / stop functions to start and shutdown the router
%%%             d) exposes send_chat_message function to make it easier to send 
%%%                 messages
%%% @end
%%%----------------------------------------------------------------------------------

-module(message_router).

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

start() ->
    spawn(message_router, route_messages, []).

stop(RouterPid) ->
    RouterPid ! shutdown.

send_chat_message(RouterPid, Addressee, MessageBody) ->
    RouterPid ! {send_chat_msg, Addressee, MessageBody}.

route_messages() ->
    receive
        {send_chat_msg, Addressee, MessageBody} ->
            Addressee ! {recv_chat_msg, MessageBody},
            route_messages();
        {recv_chat_msg, MessageBody} ->
            io:format("Received: ~p~n", [MessageBody]),
            route_messages();
        shutdown ->
            io:format("message_router ~p shutting down ~n", [self()]);
        Oops ->
            io:format("Warning! Received unknown message: ~p~n", [Oops]),
            route_messages()
    end.


    %%
    %% Local functions
    %%

