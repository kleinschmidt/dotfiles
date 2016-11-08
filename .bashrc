#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

EDITOR=emacs

alias ls='ls --color=auto -aF'
PS1='[\u@\h \W]\$ '

# ls colors
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# lolcommits configuration
export LOLCOMMITS_DELAY=1
# export LOLCOMMITS_FORK=1
# export LOLCOMMITS_STEALTH=1

# git bash completion
source /usr/share/git/completion/git-completion.bash
