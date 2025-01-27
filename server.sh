#!/bin/bash

erlc server.erl
erl -sname serv -setcookie nocookie -s server
