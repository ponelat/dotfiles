#!/usr/bin/env bash

if [[ $# -lt 1 ]]; then
    echo "$0: Pass URL to download as argument"
    exit 1
fi

url=$1
parts=${2:-8} # default to 8 parts
starting=1

name="$(expr $url : '.*/\(.*\)')"
size="$(curl --head --silent $url | grep Content-Length | sed 's/[^0-9]*//g')"
echo Size: $size
echo Filename: $name
echo Downloading in $parts parts

for i in $(seq $starting $parts)
do
  let from="$size*($i-1)/$parts"
    if [[ $c != $parts ]]; then
      let to="($size*$i/$parts)-1"
    else
      let to="$size*$i/$parts"
    fi

    out=temp.part$i

    if [[ -f $out ]] ; then
      echo "$out alredy exists... skipping"
      continue
    fi

    echo "curl -L --range $from-$to -o $out $url"
    curl -L --range $from-$to -o $out $url

done

echo Finished!
