-module(client).
-export([
    server_node/0, 
    logon/1, 
    logoff/0, 
    list/0, 
    join/1, 
    message/1, 
    dm/2, 
    client/1, 
    client/2,
    is_command/1,
    handle_button/1,
    chat_log/1,
    start/0
    ]).

% node name of the server to connect to
% TODO make this dynamic/user assignable
server_node() ->
    'serv@arch-atheris-mobile'.

% Client loop for persisting a local client process
client(ServerName, Nickname) ->
    {server, ServerName} ! {self(), logon, Nickname},
    client(ServerName).

client(ServerName) ->
    receive
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % User Requests
        kill ->
            chat_log ! {server_response, logged_off},
            {server, ServerName} ! {self(), logoff},
            chat_log ! kill,
            exit(normal);
        logoff ->
            chat_log ! {server_response, logged_off},
            {server, ServerName} ! {self(), logoff},
            exit(normal);
        {join, Channel} ->
            {server, ServerName} ! {self(), join, Channel};
        get_list ->
            {server, ServerName} ! {self(), list};
        {send_dm, ReceiverName, Message} ->
            {server, ServerName} ! {self(), send_dm, ReceiverName, Message};
        {send_message, Message} ->
            {server, ServerName} ! {self(), send_message, Message};
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Deliveries
        {channel_list, List} ->
            chat_log ! {channel_list, List};
        {receive_message, SenderName, Message} ->
            chat_log ! {receive_message, SenderName, Message};
        {receive_dm, SenderName, Message} ->
            chat_log ! {receive_dm, SenderName, Message};
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Server responses
        {server_response, kill, Message} ->
            chat_log ! {server_response, kill, Message},
            exit(normal);
        {server_response, veto, Message} ->
            chat_log ! {server_response, veto, Message};
        {server_response, Message} ->
            chat_log ! {server_response, Message}
    end,
    client(ServerName).

% User functions for manipulating local client process

% Trys to log into server as Nickname
logon(Nickname) ->
    case whereis(me) of 
        undefined ->
            register(me, spawn(client, client, [server_node(), Nickname]));
        _ -> already_logged_on
    end.

% Logs off of server
logoff() ->
    me ! logoff.

% Joins a specified channel
join(Channel) ->
    me ! {join, Channel},
    ok.

% Lists active channels on server
list() ->
    me ! get_list,
    ok.

% Sends a direct message to another user
dm(ReceiverName, Message) ->
    case whereis(me) of
        undefined ->
            not_logged_on;
        _ ->
            me ! {send_dm, ReceiverName, Message},
            ok
    end.

% Sends a channel-wide message
message(Message) ->
    case whereis(me) of
        undefined ->
            not_logged_on;
        _ ->
            me ! {send_message, Message},
            ok
    end.

kill() ->
    case whereis(me) of
        undefined ->
            chat_log ! kill;
        _ ->
            me ! kill 
    end.

%%%%%%%%%%%%%
% GUI
start() ->
    % initialize wx app
    Wx = wx:new(),

    % create wx Frame and add a status bar to the Frame
    Frame = wxFrame:new(Wx, -1, "Chat_Application", [{size, {600, 400}}]),
    wxFrame:createStatusBar(Frame, []),

    % create a panel embedded in the Frame (wxPanel is a child of wxFrame)
    Panel = wxPanel:new(Frame),
    % create a message display box w/ parent Panel (32 = wxTE_MULTILINE, 16 = wxTE_READONLY)
    MessageDisplay = wxTextCtrl:new(Panel, -1, [{style, 32 bor 16}, {size, {380, 200}}]),
    % create input field and send button with parent Panel
    InputField = wxTextCtrl:new(Panel, -1, [{size, {300, 30}}, {pos, {10, 210}}]),
    SendButton = wxButton:new(Panel, -1, [{label, "Send"}, {pos, {320, 210}}]),

    % Bind button event
    wxButton:connect(
        SendButton, 
        command_button_clicked, 
        [{callback, 
            fun(_, _) -> 
                handle_button(InputField) 
            end}]),

    % Show frame
    wxFrame:show(Frame),

    % Ending program
    chat_log(MessageDisplay),
    wx:destroy(),
    io:format("Halting...~n"),
    halt(0).

% handle incoming messages from client
chat_log(TextField) ->
    case whereis(chat_log) of
        undefined ->
            register(chat_log, self());
        _ -> ok
    end,

    receive
        {receive_message, SenderName, Message} ->
            Prefix = atom_to_list(SenderName) ++ ": ",
            wxTextCtrl:appendText(TextField, string:join([Prefix, Message, "\n"], "")),
            chat_log(TextField);
        {receive_dm, SenderName, Message} ->
            Prefix = "[dm] " ++ atom_to_list(SenderName) ++ ": ",
            wxTextCtrl:appendText(TextField, string:join([Prefix, Message, "\n"], "")),
            chat_log(TextField);
        {channel_list, List} ->
            Prefix = "server: ",
            wxTextCtrl:appendText(
                TextField, 
                string:join([Prefix, "[", string:join(List, ", "), "]", "\n"], "")),
            chat_log(TextField);
        {server_response, Message} ->
            Prefix = "server: ",
            wxTextCtrl:appendText(TextField, string:join([Prefix, atom_to_list(Message), "\n"], "")),
            chat_log(TextField);
        {server_response, veto, Message} ->
            Prefix = "server: ",
            wxTextCtrl:appendText(TextField, string:join([Prefix, atom_to_list(Message), "\n"], "")),
            chat_log(TextField);
        {server_response, kill, Message} ->
            Prefix = "server: ",
            wxTextCtrl:appendText(TextField, string:join([Prefix, atom_to_list(Message), "\n"], "")),
            chat_log(TextField);
        kill ->
            ok
    end.

% 
handle_button(InputField) ->
    % grab the value from the textbox
    Text = wxTextCtrl:getValue(InputField),
    % converts Text to a list of unicode points
    Text = unicode:characters_to_list(Text),
    
    % resets the InputField to blank
    wxTextCtrl:setValue(InputField, ""),

    case is_command(Text) of
        true ->
            % separate the message and command
            Words = string:split(Text, " ", all),
            Command = lists:nth(1, Words),
            Message = string:join(lists:delete(Command, Words), " "),
            % check the command against the pre-defined commands
            case Command of
                "/logon" ->
                    logon(list_to_atom(Message));
                "/logoff" ->
                    logoff();
                "/list" ->
                    list();
                "/join" ->
                    join(list_to_atom(Message));
                "/message" ->
                    message(Message);
                "/dm" ->
                    % separate the given name from the message to send
                    MoreWords = string:split(Message, " ", all),
                    ReceiverName = lists:nth(1, MoreWords),
                    DM = string:join(lists:delete(ReceiverName, MoreWords), " "),
                    % list_to_atom returns an atomized version of a string (RecieverName)
                    dm(list_to_atom(ReceiverName), DM);
                "/kill" ->
                    kill()
            end;
        false ->
            message(Text)
    end.

% Helper for parsing messages
% 47 is the ascii character for the character '/'
is_command([Leading | _]) ->
    case Leading of
        47 ->
            true;
        _ ->
            false
    end.
