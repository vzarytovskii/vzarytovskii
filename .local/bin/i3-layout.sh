#!/usr/bin/env bash
LAYOUT=$( (for x in stacking tabbed; do echo $x; done) | rofi -dmenu -p "Select layout:")
i3-msg layout "${LAYOUT}"
