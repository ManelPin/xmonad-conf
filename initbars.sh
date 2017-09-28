#!/bin/sh
basedir=$(dirname $0)
cacheused=0
globalrefresh=0

getxrandr () {
	cachepath="$XDG_RUNTIME_DIR/xmonad/xrandr.cache"
	refresh=$globalrefresh
	while getopts "r" opt; do
		case $opt in
			r)
				refresh=1
				shift;
				;;
			\?)
				echo "Invalid option: -$OPTARG" >&2
				exit 1
				;;
		esac
	done

	if [[ $refresh == 1 && -f $cachepath ]]; then
		echo "Removing cache file $cachepath (refresh=1)" >&2
		rm $cachepath
	fi

	if [[ -f $cachepath ]]; then
		echo "Using cached xrandr data from $cachepath" >&2
		cacheused=1
		cat $cachepath
	else
		echo "Using fresh xrandr data" >&2
		data="$(xrandr)"
		echo "Caching xrandr data to $cachepath" >&2
		mkdir -p $(dirname $cachepath)
		echo "$data" > $cachepath
		cacheused=0
		cat $cachepath
	fi
}

calcpos () {
	final=0
	offsetx=16
	offsety=16
	height=64
	while getopts "f" opt; do
		case $opt in
			f)
				final=1
				shift;
				;;
			# TODO: Add options for offsets & height
			\?)
				echo "Invalid option: -$OPTARG" >&2
				exit 1
				;;
		esac
	done

	sid=$1
	xrr="$(getxrandr)"
	parsedxrr=$(echo "$xrr" | grep "DP-$sid" | sed 's/DP-'$sid'[a-zA-Z ]*//' | sed 's/ .*//')

	if [[ $parsedxrr == "" ]]; then
		echo "No data for DP-$sid found in cached xrandr data" >&2
		if [[ $final == 1 ]]; then
			echo "Final attempt failed. Exiting..." >&2
			exit 1
		fi
		if [[ $cacheused == 1 ]]; then
			echo "Attempting to refresh xrandr data..." >&2
			getxrandr -r
			echo "Attempting to calculate position with fresh xrandr data..." >&2
			calcpos -f
			return
		fi
	fi
	dims=($(sed 's/+[0-9]*+[0-9]*//' <<< $parsedxrr | tr "x" "\n"))
	offsets=($(sed 's/[0-9]*x[0-9]*+//' <<< $parsedxrr | tr "+" "\n"))

	dx=${dims[0]}
	dy=${dims[1]}

	ox=${offsets[0]}
	oy=${offsets[1]}

	let xpos="$offsetx + $ox"
	let ypos="$offsety + $oy"
	let width="$dx - ($offsetx * 2)"

	printf "Static { xpos = %d, ypos = %d, width = %d, height = %d }" \
		$xpos $ypos $width $height
}

main () {
	while getopts "r" opt; do
		case $opt in
			r)
				globalrefresh=1
				shift;
				;;
			\?)
				echo "Invalid option: -$OPTARG" >&2
				exit 1
				;;
		esac
	done

	pkill "xmobar"
	while test ${#} -gt 0
	do
		sid=$1 ; shift
		focusPipe=$1 ; shift
		workspacesPipe=$1 ; shift

		cmds=$(cat <<EOF
[ Run PipeReader "$focusPipe" "focusPipe"
, Run PipeReader "$sid:$workspacesPipe" "workspacesPipe"
]
EOF
)
		# pos=$(calcpos $sid)
		# code=$?
		# if [[ $code != 0 ]]; then
		# 	echo "calcpos exited with non-zero code $code, aborting..." >&2
		# 	exit 1
		# fi
		# xmobar -x $sid --add-command="$cmds" --position="$pos" $basedir/xmobar.conf &
		xmobar -x $sid --add-command="$cmds" $basedir/xmobar.conf &
	done
}

main $*
