# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt autocd beep nomatch
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/dave/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

zstyle ':completion:*' rehash true matcher-list '' 'm:{a-z}={A-Za-z}'

autoload promptinit; promptinit
prompt spaceship

# SPACESHIP customization: nerd fonts
export SPACESHIP_JULIA_SYMBOL=" "
export SPACESHIP_PACKAGE_SYMBOL=" "
export SPACESHIP_DOCKER_SYMBOL=" "


# from .bashrc
# lolcommits configuration
export LOLCOMMITS_DELAY=1
export LOLCOMMITS_FORK=1
export LOLCOMMITS_STEALTH=1

# for dotfiles, use `config ...` instead of `git ...`
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

export PATH="/home/dave/bin/:$PATH"

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'

# ls colors (also use for completion)
source /usr/share/zsh/plugins/zsh-dircolors-solarized/zsh-dircolors-solarized.zsh
alias ls='ls --color=auto'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

if [ -f .zshrc_local ]; then
    source .zshrc_local
fi

if [ -f clipboard.zsh ]; then
    source clipboard.zsh
fi
