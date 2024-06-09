#!/usr/bin/env bash
destsize=`ls -l $1|awk '{print $5}'`
if [ "16384" != $destsize ]; then
  echo "Error! File size is not 16384 - may not validate as a ROM"
  echo "File size was $destsize"
  exit 255
else
  echo "File is valid!"
fi
