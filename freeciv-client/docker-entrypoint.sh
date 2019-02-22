#!/bin/bash

executable=/usr/games/freeciv-qt

if [ $# -lt 1 ] || [ $# -gt 3 ]; then
	echo "Usage: docker-entrypoint.sh [DISPLAY value] <HOST_IP> <INGAME_USER>"
	exit 1
else
	echo "$@"
fi

cd /home/guest
if [ -z "$2" ]; then
	DISPLAY="$1" $executable
else
	if [ -z "$3" ]; then
		DISPLAY="$1" $executable -s $2
	else
		DISPLAY="$1" $executable -s $2 -n $3
	fi
fi
