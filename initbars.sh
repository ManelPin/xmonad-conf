#!/bin/sh
basedir=$(dirname $0)

main () {
  pkill "xmobar"

  while test ${#} -gt 0; do
    sid=$1 ; shift
    focusPipe=$1 ; shift
    workspacesPipe=$1 ; shift

    cmds=$(cat <<EOF
[ Run PipeReader "$focusPipe" "focusPipe"
, Run PipeReader "$sid:$workspacesPipe" "workspacesPipe"
]
EOF
)

    $basedir/xmobar.sh -x $sid --add-command="$cmds" $basedir/xmobar.conf &
  done
}

main $*
