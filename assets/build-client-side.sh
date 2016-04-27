#!/bin/sh

# Generate client-side files
webpack -p
node-sass client/source/index.scss client/static/index.css
