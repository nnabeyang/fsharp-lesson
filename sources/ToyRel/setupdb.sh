#!/bin/sh

rm -rf ./database
mkdir -p ./database/master
cp ../data/シラバス.csv database/master 
cp ../data/wikipedia/*.csv database/master
