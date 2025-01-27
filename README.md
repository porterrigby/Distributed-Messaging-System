# Distributed Messaging System

## Overview

This program is a distributed messaging system built in Erlang that
has functionality for direct messaging and channels.  It includes client
and server modules and uses the wxErlang library for GUI functionality.

## Compiling and Using

First, start the server by running the server script:   
```./server.sh```  

Then, in a separate terminal window, start the client(s) with the client script,
passing in a unique name for each client:  
```./client.sh <client-name>```  

A wx window will pop up where you can view and send messages!

In the chatbox, log-in with the /logon command and press send.  
```/logon <nickname>```  

The list of commands are as follows:
```
/list                         //lists active channels on the server  // every user is in channel “private” by default
/join <chan_name>             // joins (or creates, if no channel exists) a channel
/message <message>            // broadcasts a message to the current channel
/dm <recipient> <message>     // sends message only to the indicated user, if that user is logged on
```  

To log off the server, use:
```
/logoff
``` 

You can also logoff/kill the GUI by issuing the kill command:
```
/kill
```
----------
