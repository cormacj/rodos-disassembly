#!/usr/bin/env bash

if [ "$1" == "" ]; then
  echo "Please supply a filename, eg RODOS213.ROM"
  echo "This is used to generate/annotate a blockfile for z80dasm to stop it decoding strings in the file."
  echo "Note, you'll have to do some manual cleanup on the output."
  exit 1
fi
#strings -td  RODOS213.ROM |awk '{printf("l%xh: equ 0x%x ;db \"%s\"\n",$1+0xc000,$1+0xc000, substr($0,9,9999))}'

#;STR_LOADING: start 0xd14c end 0xd155 type bytedata

strings -td  $1 |awk '{strdata=substr($0,9,9999); strlen=length(strdata); printf("l%xh: start 0x%x end 0x%x type bytedata ;db \"%s\"\n",$1+0xc000,$1+0xc000,$1+0xc000+strlen, substr($0,9,9999))}'
