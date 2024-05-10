#!/usr/bin/env bash
destsize=`ls -l $1|awk '{print $5}'`
if [ "16384" != $destsize ]; then
  echo "Error! File size is not 16384 - may not validate as a ROM"
  echo -n "Size was :"
  echo $destsize
  exit 255
else
  echo "ROM file is a valid size!"
fi
