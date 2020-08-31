#!/bin/bash

echo "* version"

guile --version

echo "* search: shepherd"

guile -L . babelia.scm bug-guix/ benchmark shepherd

echo "* search: shepherd reboot"

guile -L . babelia.scm bug-guix/ benchmark bug-guix/ shepherd reboot

echo "* search: shepherd restart"

guile -L . babelia.scm bug-guix/ benchmark bug-guix/ shepherd restart

echo "* search: guix"

guile -L . babelia.scm bug-guix/ benchmark bug- guix
