%%%----------------------------------------------------------------------------------
%%% @author roshando
%%% @copyright 2005 - 2017 roshando
%%% @doc Chat client. This module defines a chat client.
%%%     In this version:
%%%         a) Changes corresponding to changes in message_router.
%%%             - remove passing the reference to the display function in 
%%%               start_router. The display function will be passed/set when
%%%               the client registers with the message_router. Uses anonymous
%%%               function when doing this. SEE MORE DETAILS in inline comments below.
%%%             - expose APIs to register / unregister nickname (i.e. allow for clients
%%%               to register/unregister with the message_router.)
%%%
%%%     NOTE: Some of the refactoring here does not mean the clients are running as
%%%           separate processes.
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
send_message(Who, MessageBody) ->
    message_router:send_chat_message(Who, MessageBody).

%% 
start_router() ->
    message_router:start().

register_nickname(Nickname) ->
    %% not passing a reference to the print_message function directly since we 
    %% will not know which nickname received the message
    %% Hence creating an anonymous function to take care of this:
    %%      fun(Msg) -> chat_client:print_message(Nickname, Msg) end
    %% Taing a closer look at the anonymous function:
    %% The anonymous function takes a single parameter, the message we received. 
    %% We then call the chat_client:print_message function and pass it the "Nickname", 
    %% who received the message, and the message itself. We can reuse Nickname in the
    %% anonymous function because the anonymous function is a closure and it closes over
    %% the value of the Nickname, making it available to us when the function is run.
    message_router:register_nick(Nickname, fun(Msg) -> chat_client:print_message(Nickname, Msg) end).

unregister_nickname(Nickname) ->
    message_router:unregister_nick(Nickname).

%%
%% Local functions
%%
print_message(Who, MessageBody) ->
    io:format("chat_client: ~p received message: ~p~n", [Who, MessageBody]).