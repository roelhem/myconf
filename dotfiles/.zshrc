#!/usr/bin/env zsh


# Register direnv hook
if [ type direnv &> /dev/null ]; then
    eval "$(direnv hook zsh)"
fi

# Get the GHC-up Environment
if [ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ]; then
    source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
fi

# Opam configuration
[[ ! -r "$HOME/.opam/opam-init/init.zsh" ]] || source "$HOME/.opam/opam-init/init.zsh"  > /dev/null 2> /dev/null


# Get the NVM setup.
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Setup chruby scripts
source $HOMEBREW_PREFIX/opt/chruby/share/chruby/chruby.sh
source $HOMEBREW_PREFIX/opt/chruby/share/chruby/auto.sh
chruby ruby-3.2.2
