#!/bin/sh

(
	echo "DELETE FROM torrents;"
	echo "DELETE FROM feed_contents;"
	echo "DELETE FROM releases;"
	echo "DELETE FROM feeds;"
) | psql hrel hrel
