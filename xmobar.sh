#!/bin/sh
until xmobar "$@"; do
  sleep 0.1
done
