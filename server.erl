-module(server).
-export([start/0, kill/0, broadcast/4, message/3, server/1, logon/3, logoff/2, get_list/1, get_list/2, join/3, dm/4, dm/5]).

% Start server process
start() ->
    register(server, spawn(server, server, [[]])).

kill() ->
    server ! kill,
    halt(0).
    

% Server loop for persisting the server process
%
% Clients is a list with form [{client_pid, client_name, channel},...]
server(Clients) ->
    receive
        {Client_PID, logon, ClientName} ->  % logon request
            New_Clients = logon(Client_PID, ClientName, Clients),
            io:format("Connected clients: ~p~n", [New_Clients]),
            server(New_Clients);
        {Client_PID, logoff} ->  % logoff request
            New_Clients = logoff(Client_PID, Clients),
            io:format("Connected clients: ~p~n", [New_Clients]),
            server(New_Clients);
        {Client_PID, join, Channel} ->
            New_Clients = join(Client_PID, Channel, Clients),
            io:format("Connected clients: ~p~n", [New_Clients]),
            server(New_Clients);
        {Client_PID, list} ->
            Client_PID ! {channel_list , get_list(Clients)},
            server(Clients);
        {Client_PID, send_dm, ReceiverName, Message} ->  % message request
            dm(Client_PID, ReceiverName, Message, Clients),
            server(Clients);
        {Client_PID, send_message, Message} ->
            message(Client_PID, Message, Clients),
            server(Clients);
        kill ->
            ok
    end.

% Handles logon requests
logon(Client_PID, ClientName, Clients) ->
    case lists:keymember(ClientName, 2, Clients) of
        true ->
            Client_PID ! {server_response, kill, nickname_taken},
            Clients;
        false ->
            Client_PID ! {server_response, logged_on},
            [{Client_PID, ClientName, private} | Clients]
    end.

% Handles logoff requests
logoff(Client_PID, Clients) ->
    lists:keydelete(Client_PID, 1, Clients).

% Handles channel join requests
join(Client_PID, Channel, Clients) ->
    case lists:keyfind(Client_PID, 1, Clients) of
        {Client_PID, ClientName, _} ->
            New_Clients = lists:keydelete(ClientName, 2, Clients),
            Client_PID ! {server_response, list_to_atom(lists:concat(['joined ', Channel]))},
            [{Client_PID, ClientName, Channel} | New_Clients];
        false ->
            Client_PID ! {server_response, veto, not_logged_on},
            Clients
    end.

% Lists active channels
get_list(Clients) ->
    get_list([], Clients).

get_list(List, [{_, _, Chan} | Clients]) ->
    case lists:member(atom_to_list(Chan), List) of
        false ->
            get_list([atom_to_list(Chan) | List], Clients);
        true ->
            get_list(List, Clients)
    end;
get_list(List, []) ->
    List.

% Handles channel messaging
message(Client_PID, Message, Clients) ->
    case lists:keyfind(Client_PID, 1, Clients) of
        {Client_PID, ClientName, Channel} ->
            case Channel == private of
                false ->
                    broadcast(Message, ClientName, Channel, Clients);
                true ->
                    Client_PID ! {server_response, veto, no_channel_joined}
            end;
        false ->
            Client_PID ! {server_response, veto, not_logged_on}
    end.

% Helper method for recursively sending messages to clients 
% of a specific channel
broadcast(Message, SenderName, Chan, [{Client_PID, _, Client_Chan} | Clients]) ->
    case Chan == Client_Chan of
        true ->
            Client_PID ! {receive_message, SenderName, Message},
            broadcast(Message, SenderName, Chan, Clients);
        false ->
            broadcast(Message, SenderName, Chan, Clients)
    end;
broadcast(_, _, _, []) ->
    ok.

% Handles direct messaging requests
dm(Sender_PID, ReceiverName, Message, Clients) ->
    case lists:keyfind(Sender_PID, 1, Clients) of
        {Sender_PID, SenderName, _} ->
            dm(Sender_PID, SenderName, ReceiverName, Message, Clients);
        false ->
            Sender_PID ! {server_response, veto, not_logged_on}
    end.

dm(Sender_PID, SenderName, ReceiverName, Message, Clients) ->
    case lists:keyfind(ReceiverName, 2, Clients) of
        {Receiver_PID, ReceiverName, _} ->
            Receiver_PID ! {receive_dm, SenderName, Message},
            Sender_PID ! {receive_dm, SenderName, Message};
        false ->
            Sender_PID ! {server_response, veto, receiver_not_found}
    end.
