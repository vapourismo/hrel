#!/bin/sh

asset_dir=$(dirname $0)
cd "$asset_dir/../client"

webpack
sass source/index.scss:static/index.css
