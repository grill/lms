#!/bin/sh

# PUT HERE YOUR FAVORITE DIFF TOOL
#DIFF="diff -dur"
DIFF="opendiff"

cd `dirname $0`
for f in `ls -1 test-out/epfl/test$1*.check`; do
  b=`echo $f | sed 's/\.check$//g'`
  if [ -f "$b" ]; then
    echo "----------------- $b -----------------"
    $DIFF $f $b
    printf "Do you accept changes (yes/no) [n] ? "; read yn
    case $yn in
        [Yy]*) mv $b $f;;
    esac
  fi
done
