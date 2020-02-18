#!/usr/bin/env bash

function _pgrep() {
    if ! [[ -x "$(command -v pgrep)" ]]; then
        # Set pgrep fallback
        ps axf | grep "$1" | grep -v grep | awk '{print $1}'
    else
        pgrep -f "$1"
    fi
}

function run {
  if ! _pgrep "$1" ;
    then
      $@&
  fi
}
