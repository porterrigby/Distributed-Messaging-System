#!/bin/bash

if [ -z $1 ]
then
    echo "Usage: ./client.sh <node-name>"
    return
fi

erlc client.erl
erl -noshell -sname $1 -setcookie nocookie -s client &
