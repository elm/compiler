#!/bin/sh

set -e

echo "Warning: You are about to remove all Elm executables!"

installdir=/usr/local/bin

if [ -f $installdir/elm ]; then
	sudo rm -f $installdir/elm
fi
