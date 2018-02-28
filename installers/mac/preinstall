#!/bin/sh

set -e

installdir=/usr/local/bin

for bin in elm elm-compiler elm-package elm-reactor elm-repl
do
	if [ -f $installdir/$bin ]; then
		sudo rm -f $installdir/$bin
	fi
	if [ -f $installdir/$bin-unwrapped ]; then
		sudo rm -f $installdir/$bin-unwrapped
	fi
done

sharedir=/usr/local/share/elm
sudo rm -rf $sharedir
