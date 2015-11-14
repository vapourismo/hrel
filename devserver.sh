#!/bin/sh

PIDFILE=".devserver.pid"
LAUNCHCMD="node --harmony server/server.js"

start_server() {
	($LAUNCHCMD; rm -rf $PIDFILE) &
	echo $! > $PIDFILE
}

restart_server() {
	kill $(< $PIDFILE)
	start_server
}

[[ -e $PIDFILE ]] && restart_server || start_server
