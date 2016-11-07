EDITOR=emacs

PATH=${PATH}:~/.cabal/bin

alias ls="ls -aF"

alias amacs="open -a Aquamacs"

export PATH="/usr/local/bin:$PATH"

export R_HOME="/Library/Frameworks/R.framework/Resources/"

# ls colors
export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# set JAVA_HOME
export JAVA_HOME=`/usr/libexec/java_home`

# set amazon command line tools home
export MTURK_CMD_HOME=/Users/dkleinschmidt/code/aws-mturk-clt-1.3.1/

# lolcommits configuration
export LOLCOMMITS_DELAY=1
# export LOLCOMMITS_FORK=1
# export LOLCOMMITS_STEALTH=1

# added by Anaconda 1.9.2 installer
export PATH="/usr/local/share/anaconda/bin:$PATH"

# initialize virtualenvwrapper
# BORKEN BY ANACONDA DANGER DANGER
# export WORKON_HOME=$HOME/.virtualenvs
# source /usr/local/share/anaconda/bin/virtualenvwrapper.sh

# AFNI path
export PATH="$PATH:/usr/local/afni"

# julia and IJulia
export PATH="/Users/dkleinschmidt/code/julia:$PATH"
alias ijulia="ipython notebook --profile julia"

# git tab-completion
source /usr/local/etc/bash_completion.d/git-completion.bash

# Don't include jrnl entries in bash history
HISTIGNORE="jrnl *"

# configuration
alias config='/usr/bin/git --git-dir=/Users/dkleinschmidt/.cfg/ --work-tree=/Users/dkleinschmidt'
