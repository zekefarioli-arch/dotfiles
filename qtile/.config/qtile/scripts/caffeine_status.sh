#!/bin/bash
if xset q | awk '/timeout:/ {exit ($2==0)}'; then
  echo ON
else
  echo OFF
fi
