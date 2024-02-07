# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# need to get this set before `compinit` to use homebrew's fpath
if [ -d "/opt/homebrew" ]; then
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:$PATH"
    fpath+=("/opt/homebrew/share/zsh/site-functions")
fi

unsetopt autocd beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '${HOME}/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

zstyle ':completion:*' rehash true matcher-list '' 'm:{a-z}={A-Za-z}'

autoload promptinit; promptinit

if command -v brew &> /dev/null; then
    source $(brew --prefix)/opt/spaceship/spaceship.zsh
else
    source "$HOME/.zsh/spaceship/spaceship.zsh"
fi

# SPACESHIP customization: nerd fonts
export SPACESHIP_JULIA_SYMBOL=" "
export SPACESHIP_PACKAGE_SYMBOL=" "
export SPACESHIP_DOCKER_SYMBOL=" "

export EDITOR="emacsclient -t"
export VISUAL="emacsclient -a emacs"

export PATH="$HOME/bin/:$PATH"

# export PATH="$HOME/.node_modules/bin:$PATH"
# export npm_config_prefix="~/.node_modules"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'

# ls colors (also use for completion)
if [ -f /usr/share/zsh/plugins/zsh-dircolors-solarized/zsh-dircolors-solarized.zsh ]; then
   source /usr/share/zsh/plugins/zsh-dircolors-solarized/zsh-dircolors-solarized.zsh
   alias ls='ls --color=auto'
   zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
fi

# if [ -f ~/.zshrc_local ]; then
#     source .zshrc_local
# fi
export WANDB_API_KEY="op://Private/wandb api key/password"
[[ /usr/bin/kubectl ]] && source <(kubectl completion zsh)

export AWS_REGION=us-east-2
export AWS_DEFAULT_REGION=us-east-2

export AWS_PROFILE="dkleinschmidt"

if [ -f ~/clipboard.zsh ]; then
    source ~/clipboard.zsh
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

eval "$(direnv hook zsh)"

. /opt/homebrew/opt/asdf/libexec/asdf.sh

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

# poetry
export PATH="/Users/dkleinschmidt/.local/bin:$PATH"
