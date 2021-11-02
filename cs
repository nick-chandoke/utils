#!/usr/bin/env bash

settermcolor() {
    kitty @ --to unix:/tmp/kitty set-colors -a "$HOME/.termcolors/$1"
}

if [[ $# -eq 0 ]]; then
    echo "USAGE: colorscheme < list [duration] | colorscheme file >. Scheme files should be found in $HOME/.termcolors/"
    exit 1
fi

if [[ "$1" == "list" ]]; then
    cd "$HOME/.termcolors/"
    colors
    for i in *; do
        echo "$i"
        settermcolor "$i"
        sleep "${2:-3}"
    done
else
    settermcolor "$1"
fi
