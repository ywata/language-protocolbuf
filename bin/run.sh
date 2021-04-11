#!/bin/sh

if [ "$1" == "" ]; then
    echo "usage: ./run.sh dir"
    exit 1
fi

parser=`find dist-newstyle -name parse-proto -type f`
if [ -x $parser ]; then 
#dist-newstyle/build/x86_64-osx/ghc-8.10.4/language-protocolbuf-0.0.0/x/parse-proto/build/parse-proto/parse-proto
  echo "$parsor"
else
  echo "no parser found"
  exit 1
fi

protos=`find $1 -name "*.proto" -type f`

count=1
for p in $protos ; do
    echo "$count"
    count=$((count+1))
    ${parser} parse $p
    if [ $? -gt 0 ]; then
	echo "-------------$p"
    fi
done
	 
