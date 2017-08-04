#!/bin/sh
basedir=$(dirname $0)
pkill "xmobar"
while test ${#} -gt 0
do
  sid=$1
  shift
	pipe=$1
	shift
	xmobar -x $sid -C "[Run PipeReader \"$sid:$pipe\" \"pipe\"]" $basedir/xmobar.conf &
done
