#!/bin/sh

rm -rf ./database
mkdir -p ./database/master
cp ../data/シラバス.csv database/master
mkdir ./database/wikipedia
cp ../data/wikipedia/*.csv database/wikipedia
mkdir ./database/tandp
cp ../data/tandp/*.csv database/tandp
