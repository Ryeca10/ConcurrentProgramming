% This record defines the structure of the client process.
% It contains the following fields:
%	gui: the name (or Pid) of the GUI process.
%	nick: client nick
%	con_ch:  a map from chatroom names (string) to chatroom PIDs; this map models all chatrooms to which the client is registered.

-record(cl_st, {gui, nick, con_ch}).

% This record defines the structure of the server process.
% It contains the following fields:
%	nicks: a map of client pids to their registered nicknames
%	registrations: a map of chatroom names to lists containing all client pids registered in that chatroom
%	chatrooms: a map of chatroom names to that chatroom's pid

-record(serv_st, {nicks, registrations, chatrooms}).

% This record defines the structure of the chatroom process.
% It contains the following fields:
%	name: that chatroom's name
%	registrations: a map from a client's PID to a client's nickname. The map represents all registered clients
%       history: chat history since the beginning of that chatroom. It should be represented as a list of
%					tuples, where each tuple is {Client nickname, Message}, both of which are strings.

-record(chat_st, {name, registrations, history}).
