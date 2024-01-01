#!/usr/bin/env zsh

# Register direnv hook
# eval "$(direnv hook zsh)"

# Opam configuration
# [[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh" >/dev/null 2>/dev/null

# Emacs
export EMACS_HOME="$HOME/.emacs.d"
export VOLTA_HOME="$HOME/.volta"
export PYENV_ROOT="$HOME/.pyenv"
export GO_HOME="$HOME/go"
export DOTNET_HOME="$HOME/.dotnet"
export MYCONF_HOME="$HOME/.myconf"
export CABAL_HOME="$HOME/.cabal"
export GHCUP_HOME="$HOME/.ghcup"
export CARGO_HOME="$HOME/.cargo"

export PATH="$MYCONF_HOME/bin:$CABAL_HOME/bin:$GHCUP_HOME/bin:$CARGO_HOME/bin:$VOLTA_HOME/bin:$PYENV_ROOT/bin:$GO_HOME/bin:$DOTNET_HOME/tools:$EMACS_HOME/bin${PATH+:$PATH}"
export PATH="/usr/local/opt/bison/bin:/usr/local/opt/libiconv/bin:$PATH"

export LDFLAGS="-L/usr/local/opt/libiconv/lib $LDFLAGS"
export CPPFLAGS="-I/usr/local/opt/libiconv/include $CPPFLAGS"
# Get the pyenv setup
# eval "$(pyenv init -)"

# Source chruby scripts
# source $HOMEBREW_PREFIX/opt/chruby/share/chruby/chruby.sh
# source $HOMEBREW_PREFIX/opt/chruby/share/chruby/auto.sh
# chruby ruby-3.2.2

# Get luaver setup
# [ -s $HOME/.luaver/luaver ] && . $HOME/.luaver/luaver
# [ -s $HOME/.luaver/completions/luaver.bash ] && . ~/.luaver/completions.zsh
