#!/bin/bash
BASEDIR="$(dirname $0)"
CACHEFILE="$BASEDIR/xmonad.state"
if [ -f $CACHEFILE ]; then
  echo "Deleting $CACHEFILE..."
  rm $CACHEFILE
else
  echo "No cache file to delete."
fi
