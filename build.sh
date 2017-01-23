#!/bin/bash

# Make sure 'dist' directory exists
[[ -e dist ]] || mkdir -p dist

# Build stylesheet
sass assets/stylesheet/index.scss:dist/index.css
