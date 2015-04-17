#!/usr/bin/env bash

# Original line...
# find . -type f -print0 | xargs -0 ls -l | awk '{size[int(log($5)/log(2))]++}END{for (i in size) printf("%10d %3d\n", 2^i, size[i])}' | sort -n

# modified...
#The argument to use for the file size
ARG=5
SEP=' '
FILE_SUFFIX=k
echo 'Log2 Distribution:'
echo "Argument separator: \"$SEP\", File sizes: \"$FILE_SUFFIX\""
awk -F "$SEP" '{ 
size[ int(log($'$ARG') / log(2)) ]++
}
END { 
for (i in size) 
  printf("%2d\t%10d'$FILE_SUFFIX' %3d files\n",i, 2^i, size[i])
}' | sort -n
