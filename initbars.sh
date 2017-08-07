#!/bin/sh
basedir=$(dirname $0)
pkill "xmobar"
while test ${#} -gt 0
do
  sid=$1
  shift
	focusPipe=$1
	shift
	workspacesPipe=$1
	shift
	xmobar -x $sid -C \
		"[Run PipeReader \"$sid:$focusPipe\" \"focusPipe\", Run PipeReader \"$sid:$workspacesPipe\" \"workspacesPipe\"]" \
		$basedir/xmobar.conf &
done
