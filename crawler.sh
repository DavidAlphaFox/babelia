#!/bin/bash

# set -xe

while IFS= read -r line; do  # read from stdin
    echo "$line";
    wget -A "*.html"  --random-wait --adjust-extension -q --mirror --no-parent "$line";
done
