setopt HIST_FCNTL_LOCK
export QT_QPA_PLATFORMTHEME="qt5ct"
export EDITOR="emacsclient -nw -c"
export ZSH="/home/u/.oh-my-zsh"
ZSH_THEME="simple"
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
plugins=(zsh-autosuggestions git archlinux colorize fd fasd fzf man z)

source $ZSH/oh-my-zsh.sh
source /home/u/.config/broot/launcher/bash/br

export LANG=en_US.UTF-8
export ARCHFLAGS="-arch x86_64"

export _JAVA_AWT_WM_NONREPARENTING=1

export DISABLEARCADE=1

#export DOTNET_ROOT=~/.dotnet
export DOTNET_USE_POLLING_FILE_WATCHER=true

export PATH=~/.ghcup:~/git-fuzzy/bin:~/.dotnet:~/.dotnet/tools:~/.cargo/bin:~/.local/bin/:$PATH

export MOZ_ENABLE_WAYLAND=1

# export LIBRARY_PATH=/usr/lib/gcc/x86_64-pc-linux-gnu/$(gcc -dumpversion)/:$LIBRARY_PATH

if [[ -n $MY_PS1 ]]; then
	PS1="$bg[yellow]$fg_bold[black] $MY_PS1 $reset_color :: $PS1"
fi

#alias cat="bat"
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
alias rg="rg --no-heading"
alias ff="/usr/bin/fd -t f"
alias fd="/usr/bin/fd -t d"
alias fa="/usr/bin/fd"
alias nohist="PS1=\"(nohist) $PS1\" HISTFILE=/dev/null zsh"
alias yay="yay --mflags --skipinteg"

emv() {
    for file in *.$1; do mv "$file" "$(basename "$file" .$1).$2"; done
}

#if [ "$TMUX" = "" ]; then (tmux has-session && tmux attach || tmux); fi
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# opam configuration
test -r /home/u/.opam/opam-init/init.zsh && . /home/u/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
