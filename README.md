# Project 5: ErLang Demo Project

* Authors: Brady Driebergen, Ryan McFarland, Porter Rigby, Reggie Wade
* Class: CS354 Section 2
* Semester: Fall 2024

## Overview

Our program is a distributed messaging system built in Erlang that
has functionality for direct messaging and channels.  It includes client
and server modules and uses the wxErlang library for GUI functionality.

## Reflection

When we first chose this project, we weren't aware of all the intricacies that Erlang
had to offer. After researching however, we quickly realized how useful Erlang is for 
distributed projects like the one we chose to build. Erlang is known for its fault 
tolerance, distributed system tools, and concurrency. Looking to use these features, 
we settled on making a messaging application. The creation of this messaging app was 
both exciting and challenging throughout and gave us some solid insights into how Erlang 
works. As none of us had used Erlang before, much of the learning time for building 
this application was spent reading manual pages, browsing syntax, and testing logic 
through trial and error.

The hardest part of this project was learning how Erlang operates with regard to 
processes and nodes. Due to the nature of the program, being able to send messages 
concurrently between nodes was imperative. After plenty of reading, accomplishing this 
task was fairly straightforward. Erlang was designed for this type of application, 
so it was no surprise how well the backend of the application came together once we knew
what we were doing. Another challenge we faced was creating the GUI for this project. 
While Erlang itself is very well documented, the Erlang version of wxWidgets (the 
library used for the GUI) is not. This project was a good exercise in reading 
documentation, and made us all appreciate the value of good documentation when working 
with new code. 

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

## Results

We were able to create a functioning distributed chat app with several commands.  
We were also able to utilize wxErlang to get a functioning GUI for interacting with
client processes.

## Sources used

- https://www.erlang.org/docs/19/man/wxtextctrl
- https://docs.wxwidgets.org/2.8.12/wx_wxtextctrl.html#wxtextctrlwxtextctrl
- https://stackoverflow.com/questions/34298103/how-can-i-get-when-the-button-is-clicked
- https://github.com/yrashk/erlang/blob/master/lib/wx/include/wx.hrl
- https://learnyousomeerlang.com/event-handlers
- https://www.erlang.org/docs/24/man/wxframe#new-3
- https://stackoverflow.com/questions/34997903/errror-in-running-wx-erlang-code
- https://stackoverflow.com/questions/36843920how-to-get-in-erlang-the-pid-of-a-node-name
- https://www.erlang.org/docs/26/getting_started/conc_prog

----------