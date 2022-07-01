export _JAVA_AWT_WM_NONREPARENTING=1
export PRESSURE_VESSEL_FILESYSTEMS_RO=/var/games/pfx_proton-ge-custom
export AVALONIA_SCREEN_SCALE_FACTORS='XWAYLAND0=2'
export DOTNET_ROOT=~/.dotnet/

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
    PATH="$HOME/.dotnet/tools:$PATH"
fi

[ -f "/home/u/.ghcup/env" ] && source "/home/u/.ghcup/env"
