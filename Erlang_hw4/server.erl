-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
     #serv_st
     {
		 nicks = maps:new(), %% nickname map. client_pid => "nickname"
		 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
		 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	 }
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
		io:format("entered loop in server~n"),
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
    Result = find(maps:keys(State#serv_st.chatrooms), ChatName),
    case Result of 
    	true ->  io:format("Server: chatroom exists~n"),
    	ChatPID = maps:get(ChatName, State#serv_st.chatrooms),
    	ClientNick = maps:get(ClientPID, State#serv_st.nicks),
    	ChatPID!{self(), Ref, register, ClientPID, ClientNick},
    	NewList = addClientToChatRoomList(State#serv_st.chatrooms, ClientPID),
    	NewState =
				#serv_st{
							nicks = State#serv_st.nicks,
							registrations = maps:update(ChatName, NewList, State#serv_st.registrations),
							chatrooms = State#serv_st.chatrooms
					};
    	false -> io:format("Server: chatroom doesnt exist~n"),
    	ChatPID = spawn(chatroom, start_chatroom, [ChatName]),
    	A = maps:put(ChatName, ChatPID , State#serv_st.chatrooms),
	 	ClientNick = maps:get(ClientPID, State#serv_st.nicks),
		ChatPID!{self(), Ref, register, ClientPID, ClientNick},
		NewState =
		#serv_st{
					nicks = State#serv_st.nicks,
					registrations = maps:put(ChatName, maps:values(A), State#serv_st.registrations),
					chatrooms = A
			}    			 
    end,

    NewState.


addClientToChatRoomList(ChatRoomMap, ClientPID) ->
   case catch(maps:find(ClientPID, ChatRoomMap)) of
        error -> 
        	io:format(" error in addClientToChatRoomList in server.erl ~n"),
        	[];

        {ok, List} ->  List = [ClientPID] ++ List
    end.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
    ChatRoomPID  = error_handler_chatrooms(ChatName, State),
    %lists:delete
    ClientPIDList = error_handler_registrations(ChatName, State),
    NewList = lists:delete(ClientPID,ClientPIDList),

    ChatRoomPID!{self(), Ref, unregister, ClientPID},
    ClientPID!{self(), Ref, ack_leave},

    NewState = 
    #serv_st{
    			nicks = State#serv_st.nicks,
    			registrations = maps:update(ChatName, NewList, State#serv_st.registrations),
    			chatrooms = State#serv_st.chatrooms
    		},
    NewState.

error_handler_chatrooms(ChatName, State) ->
 	case catch(maps:find(ChatName,State#serv_st.chatrooms)) of
 		error -> 
		 		io:format(" error in error_handler_chatrooms in server.erl ~n"),
		 		error;

 		{ok, ChatRoomPID} -> ChatRoomPID
 	end.

error_handler_registrations(ChatName, State) ->
	case catch(maps:find(ChatName,State#serv_st.registrations)) of
 		error -> 
 				io:format(" error in error_handler_registrations in server.erl ~n"),
 				error;
 		{ok, ClientPIDList} -> ClientPIDList
 	end.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	Result = find(maps:values(State#serv_st.nicks), NewNick),
    case Result of
    	true ->    ClientPID!{Ref, used},
    			   State;
    	false -> 
    			ClientPID!{Ref, notused},
    			NewState = 
				#serv_st{
								nicks = maps:update(ClientPID, NewNick, State#serv_st.nicks),
								registrations = State#serv_st.registrations,
								chatrooms = State#serv_st.chatrooms
						},

				Keys = maps:keys(State#serv_st.registrations),
				ChatRoomNames = listOfChatroomNames(Keys, State#serv_st.registrations,ClientPID),
				ChatRoomPIDs = chatroomPIDs(ChatRoomNames,State#serv_st.chatrooms),
				sendToAllChatRooms(ChatRoomPIDs, self(), Ref, ClientPID, NewNick),
				NewState
	 end.

sendToAllChatRooms(ChatRoomPIDs, ServerPID, Ref, ClientPID, NewNick) ->
	case ChatRoomPIDs of
		[H|T] -> H!{ServerPID, Ref, update_nick, ClientPID, NewNick},
				 sendToAllChatRooms(T, ServerPID, Ref, ClientPID, NewNick);
		[] -> ok

	end.
chatroomPIDs(ChatRoomNames,ChatRoomMap) ->
	case ChatRoomNames of
		[H|T] -> ChatPID  = maps:get(H,ChatRoomMap),
				 [ChatPID] ++ chatroomPIDs(T,ChatRoomMap);
		[] -> []
	end.

listOfChatroomNames(Keys, RegMap,ClientPID) ->
	case Keys of
		[H|T] -> List = maps:get(H,RegMap),
				 Result = find(List,ClientPID),
				 case Result of
				 	true -> [H] ++ listOfChatroomNames(T, RegMap,ClientPID);
				 	false -> listOfChatroomNames(T, RegMap,ClientPID)
				 end;
		[] -> []
	end.


find([X|_] , X) -> true;
find([_|T] , X) -> find(T, X);
find([] , _) -> false.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->

	NewNicksMap = maps:remove(ClientPID, State#serv_st.nicks),
	ChatRoomNameList = maps:keys(State#serv_st.registrations),
	ListOfChatRoomsClientIsConnectedTo = listOfChatroomNames(ChatRoomNameList, State#serv_st.registrations, ClientPID),
    callChatRoomsRecursively(ListOfChatRoomsClientIsConnectedTo, Ref, ClientPID),
    NewRegMap = newRegMap(maps:keys(State#serv_st.registrations), State#serv_st.registrations, ClientPID),
    ClientPID!{self(), Ref, ack_quit},
    NewState = 
    #serv_st{
    			nicks = NewNicksMap,
    			registrations = NewRegMap,
    			chatrooms = State#serv_st.chatrooms
    		},
    NewState.

newRegMap(Keys, RegMap, ClientPID) ->
  case Keys of
		[H|T] -> ClientPIDList = maps:get(H, RegMap),
				 Result = find(ClientPIDList,ClientPID),
				 case Result of
				 	true -> NewClientPIDList = lists:delete(ClientPID,ClientPIDList),
				 			NewMap = maps:update(H, NewClientPIDList, RegMap),
				 			newRegMap(T, NewMap, ClientPID);
				 	false -> newRegMap(T, RegMap, ClientPID)
				 end;
		[] -> RegMap
  end.

callChatRoomsRecursively(ListOfChatRoomsClientIsConnectedTo, Ref, ClientPID) ->
 	case ListOfChatRoomsClientIsConnectedTo of
 		[H|T] -> H!{self(), Ref, unregister, ClientPID},
 				  callChatRoomsRecursively(T, Ref, ClientPID);
 		[] -> ok
 	end.


