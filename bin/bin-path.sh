#!/bin/sh

parser=`find dist-newstyle -name parse-proto -type f`
dir=`dirname ${parser}`
if [ -x $parser ]; then 
  echo "PATH=\$PATH:${PWD}/${dir}:${PWD}/bin"
else
  exit 1
fi

	 
