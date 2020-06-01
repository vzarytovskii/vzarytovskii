export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR="emacsclient -nw -c"
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
plugins=(git archlinux autojump cargo colorize fd fasd fzf git-prompt gitfast gitignore man z)

source $ZSH/oh-my-zsh.sh
source /home/v/.config/broot/launcher/bash/br

export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

export DOTNET_ROOT=~/.dotnet

export PATH=~/git-fuzzy/bin:~/.dotnet:~/.dotnet/tools:~/.cargo/bin:~/.local/bin/:$PATH

alias cat="bat"
alias br="br -h"
alias lsa="br -dp"
alias ls="exa"
alias la="ls -a"
alias ll="exa -alF"
alias tree="br"
alias g="git"
alias gitf="git fuzzy"
alias gf="git fuzzy"
alias mc="ranger"
alias twitch="kpl"
alias reddit="tuir"
alias youtube="mpsyt"
alias yt="youtube"
alias discord="6cord"
alias emacs="~/.local/bin/emacs"
# alias magit="emacs -e \"(magit-status)\" -e \"(delete-other-windows)\" -e \"(kill-buffer \\\"*spacemacs*\\\")\""
alias magit="emacs --eval \"(magit-status)\""
alias todo="emacs --eval \"(org-todo-list)\""
alias cap="emacs --eval \"(org-capture)\""
alias spotify="spt"
alias cheat="cht.sh"
alias how="howdoi"

if [ "$TMUX" = "" ]; then (tmux has-session && tmux attach || tmux); fi
