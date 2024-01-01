#!/usr/bin/env zsh

# Register the homebrew shell environment variables
export HOMEBREW_PREFIX="/usr/local"
export HOMEBREW_CELLAR="/usr/local/Cellar"
export HOMEBREW_REPOSITORY="/usr/local/Homebrew"

# PATH
export MANPATH="/usr/local/share/man${MANPATH+:$MANPATH}:"
export INFOPATH="/usr/local/share/info:${INFOPATH:-}"
