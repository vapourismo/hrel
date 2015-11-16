#!/bin/sh

script=$(readlink -f "$0")

pushd $(dirname "$script")/../client
webpack
popd
