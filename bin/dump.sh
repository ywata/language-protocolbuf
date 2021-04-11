#!/bin/sh

parser=`find dist-newstyle -name parse-proto -type f`
if [ -x ${parser} ]; then 
  # do nothing
  ${parser} plugin out.dat
else
  echo "no parser found"
  exit 1
fi


	 
