#!/bin/bash

echo "* version"

guile --version

echo "* search: shepherd"

./babelia.scm search bug-guix/ shepherd

echo "* search: shepherd reboot"

./babelia.scm search bug-guix/ shepherd reboot

echo "* search: shepherd restart"

./babelia.scm search bug-guix/ shepherd restart

echo "* search: guix"

./babelia.scm search bug-guix/ guix
