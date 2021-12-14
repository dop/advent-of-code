#!/bin/sh

if [ -z "$1" ]; then
    clojure -M src/aoc/run.clj
else
    SCRIPT=`find src/aoc -name "day*$1.clj"`
    fswatch -0 -or ./src | xargs -0 -n 1 -I{} clojure -M $SCRIPT
fi
