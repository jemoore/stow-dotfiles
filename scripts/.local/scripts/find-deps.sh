#!/usr/bin/bash

# ldd is a slight security risk as it loads the file and resolves dependencies
# and traces them.
# good to see VERSION section
# ldd -v $1

# readelf -d $1

objdump -x $1 | grep NEEDED
