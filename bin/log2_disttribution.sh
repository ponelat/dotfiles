#!/usr/bin/env bash

# Original line...
# find . -type f -print0 | xargs -0 ls -l | awk '{size[int(log($5)/log(2))]++}END{for (i in size) printf("%10d %3d\n", 2^i, size[i])}' | sort -n

# modified...
SEP=' '
echo 'Log2 Distribution:'
echo 'For each line, the first number will be counted, seperated by "'$SEP'"'
awk -F "$SEP" '{ 
size[ int(log($1) / log(2)) ]++
}
END { 
for (i in size) 
  printf("%10d %3d\n", 2^i, size[i])
}' | sort -n
