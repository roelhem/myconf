#!/usr/bin/env zsh

if [ which brew &> /dev/null ]; then
    eval "$(brew shellenv)"
fi
