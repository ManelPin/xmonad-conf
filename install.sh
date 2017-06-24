#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKDIR=$(mktemp -p /tmp -d xmonad-XXXXX)

cp -r $DIR/* $WORKDIR

echo "Building xmonad.hs..."

cd $WORKDIR
ghc --make $WORKDIR/xmonad.hs -i -ilib -dynamic -fforce-recomp -main-is main -v0 -o $WORKDIR/xmonad-x86_64-linux 2>&1 >$WORKDIR/build.log
code=$?

if [[ $code != 0 ]]; then
	echo "Error: ghc exited with code $code"
	cat $WORKDIR/build.log
	rm -rf $WORKDIR
	exit 1
fi

echo "Success!"
echo "Installing to ~/.xmonad..."

rm -rf $HOME/.xmonad
mv $WORKDIR $HOME/.xmonad

echo "Restarting XMonad..."

xmonad --restart
