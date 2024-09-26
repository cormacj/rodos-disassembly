#!/usr/bin/env bash
#
# This file checks the size of the file passed as a parameter and makes sure its exactly 16k.
#
destsize=`ls -l $1|awk '{print $5}'`
if [ "16384" != $destsize ]; then
  echo "Error! File size is not 16384 (16k) - may not validate as a ROM"
  echo "File size was $destsize"
  exit 255
else
  echo -e "\n$1 is a valid size for an Amstrad ROM!\n"
fi
