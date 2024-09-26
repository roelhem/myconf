#!/usr/bin/env sh

filename="$1"
line="$2"
column="$3"

emacsclient -n -u "+$line:$column" "$filename"
