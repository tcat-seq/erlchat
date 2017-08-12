%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat message router. This module defines a process that routes messages.
%%%         In this version:
%%%             a) Introduced dictionary to track clients (dictionary has a 
%%%                 client (key) -> display function (value. PrintFun, in this case) mapping.) 
%%%             b) Refactored code to send message to ClientName rather than message_router
%%%                 to message_router. The "{send_chat_msg, Addressee, MessageBody}" in the 
%%%                 earlier piece of code illustrated the problem where we are sending the message 
%%%                 from router to router (Addressee contained the pid of the 
%%%                 message_router) rather than from client to client. SEE MORE DETAILS in the 
%%%                 inline comments.
%%%                 NOTE: This refactoring does not mean the message_router is not handling the
%%%                 message. The message_router is still "receive"-ing the message from a client.
%%%                 It then routes the message to the apprpriate client based on the mapping in 
%%%                 the registry (dictionary). That is, it invokes the target client's display 
%%%                 function to print the message.
%%%             c) Exposed API for clients to register / unregister with the message_router.
%%%
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

start() ->
    io:format("Starting message router..."),
    Pid = spawn(message_router, route_messages, [dict:new()]),
    erlang:register(?SERVER, Pid),
    %% returning the Pid since it's needed as the Addressee in the chat_client:send_message function
    Pid.

stop() ->
    io:format("Stopping message router...~ntoday"),
    ?SERVER ! shutdown.

send_chat_message(Addressee, MessageBody) ->
    ?SERVER ! {send_chat_msg, Addressee, MessageBody}.

register_nick(ClientName, PrintFun) ->
    ?SERVER ! {register_nick, ClientName, PrintFun}.

unregister_nick(ClientName) ->
    ?SERVER ! {unregister_nick, ClientName}.

route_messages(Clients) ->
    receive
        %% Replaced Addressee with ClientName. So this send the message to the 
        %% client (ClientName) rather than a message_router (since Addressee
        %% contained the pid of the message_router in the earlier code.
        %% NOTE: We still send the message to the message_router described 
        %% by the registered process ?SERVER. The message_router "routes" the
        %% message to the appropriate client based on the mapping in the dictionary.
        {send_chat_msg, ClientName, MessageBody} ->
            ?SERVER ! {recv_chat_msg, ClientName, MessageBody},
            route_messages(Clients);
        {recv_chat_msg, ClientName, MessageBody} ->
            %% assumes that dictionary has a client -> display function (PrintFun) mapping. 
            %% Hence we pattern match on "{ok, PrintFun}"
            case dict:find(ClientName, Clients) of
                {ok, PrintFun} ->
                    io:format("message_router: Found chat client in registry ~p~n", [ClientName]),
                    %% delegate the presentation (printing) of the message to the
                    %% PrintFun function reference
                    PrintFun(MessageBody);
                error ->
                    io:format("message_router: Unknown client ~p~n", [ClientName])
            end,
            route_messages(Clients);
        {register_nick, ClientName, PrintFun} ->
            case dict:find(ClientName, Clients) of
                error ->
                    io:format("message_router: registering ~p~n", [ClientName]),
                    %% doing tail recursion (i.e. calling route_messages) by passing 
                    %% in the "###return value###" from the "store" function in the dict
                    %% module. The reason for doing this is because Erlang has "single 
                    %% assignment variables". When you put a new value in a dictionary or
                    %% remove one from a dictionary, Erlang creates an entirely new 
                    %% dictionary. So you need to make sure the new value is what you are
                    %% passing around or the new variable is what you are operating on.
                    route_messages(dict:store(ClientName, PrintFun, Clients));
                {ok, PrintFun} ->
                    io:format("message_router: ~p chat client registered previously. Skipping registration. ~n", [ClientName])
            end,
            route_messages(Clients);
        {unregister_nick, ClientName} ->
            case dict:find(ClientName, Clients) of
                {ok, PrintFun} ->
                    io:format("message_router: unregistering ~p~n", [ClientName]),
                    %% doing tail recursion here as well.
                    route_messages(dict:erase(ClientName, Clients));
                error ->
                    io:format("message_router: Unknown client ~p. Ignoring. ~n", [ClientName])
            end,
            route_messages(Clients);
        shutdown ->
            io:format("message_router: ~p shutting down ~n", [self()]);
        Oops ->
            io:format("message_router: Warning - message not recognized: ~p~n", [Oops]),
            route_messages(Clients)
    end.


    %%
    %% Local functions
    %%

