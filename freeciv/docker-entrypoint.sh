#!/bin/bash
if [ -z "$@" ]; then
/usr/games/freeciv-server --saves /freeciv --port 5556
else
/usr/games/freeciv-server --saves /freeciv --port 5556 -f /freeciv/"$@"
fi
