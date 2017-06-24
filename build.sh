#!/bin/bash

tmpbase="/tmp"
installdir="$HOME/.xmonad"
restart=0
buildonly=0

while getopts "t:i:br" opt; do
	case $opt in
		t)
			tmpbase=$OPTARG
			;;
		i)
			installdir=$OPTARG
			;;
		b)
			buildonly=1
			;;
		r)
			restart=1
			;;
		\?)
			echo "Invalid option: -$OPTARG" >&2
			exit 1
			;;
	esac
done

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
WORKDIR=$(mktemp -p $tmpbase -d xmonad-XXXXX)

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

if [[ $buildonly == 1 ]]; then
	exit 0
fi

echo "Installing to $installdir..."

rm -rf $HOME/.xmonad
mv $WORKDIR $installdir

if [[ $restart = 1 ]]; then
	echo "Restarting XMonad..."
	xmonad --restart
fi
