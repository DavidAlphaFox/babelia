#!/bin/bash

# set -xe

while IFS= read -r file; do
    echo "$file";
    wget -q --mirror --no-parent "$file";
done
