export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR="/usr/bin/emacs -nw"
export ZSH="/home/v/.oh-my-zsh"
ZSH_THEME="robbyrussell"
CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"
DISABLE_AUTO_UPDATE="false"
DISABLE_UPDATE_PROMPT="false"
# export UPDATE_ZSH_DAYS=13
# DISABLE_MAGIC_FUNCTIONS=true
# DISABLE_LS_COLORS="true"
# DISABLE_AUTO_TITLE="true"
# ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"
# DISABLE_UNTRACKED_FILES_DIRTY="true"
# HIST_STAMPS="mm/dd/yyyy"
# ZSH_CUSTOM=/path/to/new-custom-folder
plugins=(git archlinux autojump cargo colorize emacs fd fasd fzf git-prompt gitfast gitignore man)

source $ZSH/oh-my-zsh.sh
source /home/v/.config/broot/launcher/bash/br

export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

export PATH=~/.cargo/bin:$PATH

alias br="br -h"
alias ls="br -dp"
alias ll="exa -alF"
alias tree="br"
alias g="git"
alias mc="ranger"

if [ "$TMUX" = "" ]; then tmux; fi
