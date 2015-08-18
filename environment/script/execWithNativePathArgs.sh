#!/bin/bash

if [ $# -eq 0 ]
then
  echo "USAGE execWithNativePathArgs.sh command arguments"
fi
command=$1
shift
arguments=""
for i in $*
do
  a=$i
  if [ ${i:0:9} == "/cygdrive" ]
  then
    a=`native_path_name.py $i`
  fi
  arguments="${arguments} ${a}"
done
exec $command $arguments
