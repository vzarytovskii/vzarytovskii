export _JAVA_AWT_WM_NONREPARENTING=1
export PRESSURE_VESSEL_FILESYSTEMS_RO=/var/games/pfx_proton-ge-custom
export AVALONIA_SCREEN_SCALE_FACTORS='XWAYLAND0=2'
export QT_QPA_PLATFORM=wayland

alias magit="emacs -e \(progn \(magit-status\) \(delete-other-windows\)\)"

if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.dotnet" ] ; then
    export DOTNET_ROOT=$HOME/.dotnet
    PATH="$HOME/.dotnet:$PATH"
fi

if [ -d "$HOME/.dotnet/tools" ] ; then
    PATH="$HOME/.dotnet/tools:$PATH"
fi

[ -f "$HOME/.nvm/nvm.sh" ] && . "$HOME/.nvm/nvm.sh"
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
