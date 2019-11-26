#!/bin/bash

echo "* version"

guile --version

echo "* search: shepherd"

./babelia.scm benchmark bug-guix/ shepherd

echo "* search: shepherd reboot"

./babelia.scm benchmark bug-guix/ shepherd reboot

echo "* search: shepherd restart"

./babelia.scm benchmark bug-guix/ shepherd restart

echo "* search: guix"

./babelia.scm benchmark bug-guix/ guix
