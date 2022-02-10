#!/bin/sh
# TODO: add binds to like, unlike, or delete the selected scheme
# --preview-window ",0" for no window
fzf -e --preview "settermcolor {}; colors" --preview-window up,1 <<< "$(cs-completion)"
