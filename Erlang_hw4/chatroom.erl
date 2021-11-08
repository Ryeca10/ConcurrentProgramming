-module(chatroom).

-include_lib("./defs.hrl").

-export([start_chatroom/1]).

-spec start_chatroom(_ChatName) -> _.
-spec loop(_State) -> _.
-spec do_register(_State, _Ref, _ClientPID, _ClientNick) -> _NewState.
-spec do_unregister(_State, _ClientPID) -> _NewState.
-spec do_update_nick(_State, _ClientPID, _NewNick) -> _NewState.
-spec do_propegate_message(_State, _Ref, _ClientPID, _Message) -> _NewState.

start_chatroom(ChatName) ->
    loop(#chat_st{name = ChatName,
		  registrations = maps:new(), history = []}),
    ok.

loop(State) ->
    NewState =
	receive
	    %% Server tells this chatroom to register a client
	    {_ServerPID, Ref, register, ClientPID, ClientNick} ->
		do_register(State, Ref, ClientPID, ClientNick);
	    %% Server tells this chatroom to unregister a client
	    {_ServerPID, _Ref, unregister, ClientPID} ->
		do_unregister(State, ClientPID);
	    %% Server tells this chatroom to update the nickname for a certain client
	    {_ServerPID, _Ref, update_nick, ClientPID, NewNick} ->
		do_update_nick(State, ClientPID, NewNick);
	    %% Client sends a new message to the chatroom, and the chatroom must
	    %% propegate to other registered clients
	    {ClientPID, Ref, message, Message} ->
		do_propegate_message(State, Ref, ClientPID, Message),
		io:format("passing new state to loop from do_prop~n");
	    {TEST_PID, get_state} ->
		TEST_PID!{get_state, State},
		loop(State)
	end,
    loop(NewState).

%% This function should register a new client to this chatroom
do_register(State, Ref, ClientPID, ClientNick) ->
    NewState = 
    #chat_st{
    			name = State#chat_st.name,
    			registrations= maps:put(ClientPID, ClientNick, State#chat_st.registrations),
    			history = State#chat_st.history
    		},
    ClientPID!{self(), Ref, connect, State#chat_st.history},
    NewState.

%% This function should unregister a client from this chatroom
do_unregister(State, ClientPID) ->
    NewState = 
    #chat_st{
    			name = State#chat_st.name,
    			registrations = maps:remove(ClientPID, State#chat_st.registrations),
    			history = State#chat_st.history
    		},
    NewState.

%% This function should update the nickname of specified client.
do_update_nick(State, ClientPID, NewNick) ->
    NewState = 
    #chat_st{
    			name = State#chat_st.name,
    			registrations= maps:update(ClientPID, NewNick, State#chat_st.registrations),
    			history = State#chat_st.history
    		},
    NewState.


%% This function should update all clients in chatroom with new message
%% (read assignment specs for details)
%	registrations: a map from a client's PID to a client's nickname. The map represents all registered clients
%       history: chat history since the beginning of that chatroom. It should be represented as a list of
%					tuples, where each tuple is {Client nickname, Message}, both of which are strings.
do_propegate_message(State, Ref, ClientPID, Message) ->
	ClientPID!{self(),Ref, ack_msg},


	Clinick = error_handler(ClientPID, State),
	PIDList = maps:keys(State#chat_st.registrations),
	NewList = lists:delete(ClientPID,PIDList),
	propagate(NewList , State, ClientPID, Ref, Message, Clinick),
	io:format("past prop~n"),


	NewState = 
	#chat_st{
				name =  State#chat_st.name,
				registrations = State#chat_st.registrations,
				history = State#chat_st.history ++ [{Clinick,Message}]
			},

	io:format("returning new state~n"),
    NewState.

propagate([H|T], State, SendingClientPID, Ref, Message, Clinick) ->
			io:format("chat member ip ~p~n", [H]),
 			H!{request, self(), Ref, {incoming_msg, Clinick, State#chat_st.name, Message}},
			propagate(T, State, SendingClientPID, Ref, Message, Clinick);

propagate([], _State, _SendingClientPID, _Ref, _Message, _Clinick) -> ok.


error_handler(ClientPID, State) ->
	case catch(maps:find(ClientPID,State#chat_st.registrations)) of
		error -> 
			io:format("error in error_handler in chatroom.erl ~n"),
			error;
		{ok , Clinick} -> Clinick
	end.

