%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat message router. This module defines a process that routes messages.
%%%         In this version:
%%%             a) printing of the message that is received is moved to chat_client 
%%%                 module using a function as the first class data type.
%%%             b) run message_router as a registered process. A registered process 
%%%                 in erlang is a process that is registered with the runtime under
%%%                 a name so you can then send messages to the process using the 
%%%                 name rather than having to know the pid.
%%% @end
%%%----------------------------------------------------------------------------------

-module(message_router).

-define(SERVER, message_router).

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

start(PrintFun) ->
    io:format("Starting message router..."),
    Pid = spawn(message_router, route_messages, [PrintFun]),
    erlang:register(?SERVER, Pid),
    %% returning the Pid since it's needed as the Addressee in the chat_client:send_message function
    Pid.

stop() ->
    io:format("Stopping message router...~ntoday"),
    ?SERVER ! shutdown.

send_chat_message(Addressee, MessageBody) ->
    ?SERVER ! {send_chat_msg, Addressee, MessageBody}.

route_messages(PrintFun) ->
    receive
        {send_chat_msg, Addressee, MessageBody} ->
            Addressee ! {recv_chat_msg, MessageBody},
            route_messages(PrintFun);
        {recv_chat_msg, MessageBody} ->
            %% delegate the presentation (printing) of the message to the PrintFun function reference
            PrintFun(MessageBody),
            route_messages(PrintFun);
        shutdown ->
            io:format("message_router: ~p shutting down ~n", [self()]);
        Oops ->
            io:format("message_router: Warning - message not recognized: ~p~n", [Oops]),
            route_messages(PrintFun)
    end.


    %%
    %% Local functions
    %%

