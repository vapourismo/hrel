#!/bin/sh

echo "DROP DATABASE hrel; CREATE DATABASE hrel;" | psql postgres hrel
psql hrel hrel < $(dirname $0)/schema.sql
