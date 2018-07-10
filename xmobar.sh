#!/bin/sh

# bash "strict mode"
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail
IFS=$'\n\t'

until xmobar "$@"; do
  sleep 0.1
done
