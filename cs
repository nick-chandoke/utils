#!/bin/sh
# --preview-window ",0" for no window
fd -d1 -tf --base-directory "$HOME/.termcolors/" | fzf -e --preview 'settermcolor {}; colors' --preview-window up,1
