#!/bin/sh

pidfile=".devserver.pid"
script=$(readlink -f $0)
launchdm="node --harmony $(dirname $script)/server/server.js"

start_server() {
	$launchdm &
	echo $! > $pidfile
}

restart_server() {
	kill $(< $pidfile)
	start_server
}

[[ -e $pidfile ]] && restart_server || start_server
