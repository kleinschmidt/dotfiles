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

autoload promptinit; promptinit
prompt spaceship

# from .bashrc
# lolcommits configuration
export LOLCOMMITS_DELAY=1
export LOLCOMMITS_FORK=1
export LOLCOMMITS_STEALTH=1

# for dotfiles, use `config ...` instead of `git ...`
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

# added by Miniconda3 4.3.11 installer
export PATH="/home/dave/miniconda3/bin:$PATH"

export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"

export PATH="/home/dave/bin/:$PATH"



ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=white'


# freesurfer: installed manually by expanding tarball to /opt/
export FREESURFER_HOME=/opt/freesurfer
