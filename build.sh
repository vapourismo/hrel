#!/bin/bash

# This script shall be run from inside NPM
if [[ "$npm_lifecycle_event" != "build" ]]; then
	npm run build
	exit 0
fi

# Make sure 'dist' directory exists
[[ -e dist ]] || mkdir -p dist

# Build stuff
webpack -d
sass "src/index.scss":"dist/index.css"

# Versioning
commit="$(git log -1 --format=%h)"
sed "s/\$commit/$commit/" < src/index.html > dist/index.html
