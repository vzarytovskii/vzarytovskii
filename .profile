# Fig pre block. Keep at the top of this file.
[[ -f "$HOME/.fig/shell/profile.pre.bash" ]] && builtin source "$HOME/.fig/shell/profile.pre.bash"
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.
export _JAVA_AWT_WM_NONREPARENTING=1
export PRESSURE_VESSEL_FILESYSTEMS_RO=/var/games/pfx_proton-ge-custom
export AVALONIA_SCREEN_SCALE_FACTORS='XWAYLAND0=2'
#export QT_QPA_PLATFORM=wayland
export DOTNET_ROOT=~/.dotnet/
# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.dotnet" ] ; then
    PATH="$HOME/.dotnet:$PATH"
fi

if [ -d "$HOME/.dotnet/tools" ] ; then
    export DOTNET_ROOT=$HOME/.dotnet
    PATH="$HOME/.dotnet/tools:$PATH"
fi

[ -f "$HOME/.nvm/nvm.sh" ] && . "$HOME/.nvm/nvm.sh"
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# Fig post block. Keep at the bottom of this file.
[[ -f "$HOME/.fig/shell/profile.post.bash" ]] && builtin source "$HOME/.fig/shell/profile.post.bash"
