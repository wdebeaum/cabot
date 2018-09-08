#!/bin/sh
#
# File: raw2aiff.sh
# Creator: George Ferguson
# Created: Mon Sep 17 09:40:25 2007
# Time-stamp: <Wed Sep  5 00:17:34 CDT 2018 lgalescu>
#

for f; do
    out="`basename $f .raw`".aiff
    sox -V -t raw -e signed-integer -b 16 -r 16000 -c 1 "$f" -t aiff "$out"
done
