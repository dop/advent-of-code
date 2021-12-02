#!/bin/sh

if [ -z "$1" ]; then
    find -s src -name 'day*.clj' -exec clojure -M {} \;
else
    fswatch -0 -r src | xargs -0 -n 1 -I {} clojure -M {}
fi
