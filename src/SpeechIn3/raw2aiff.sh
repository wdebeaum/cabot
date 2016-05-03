#!/bin/sh
#
# File: raw2aiff.sh
# Creator: George Ferguson
# Created: Mon Sep 17 09:40:25 2007
# Time-stamp: <Mon Sep 17 09:45:12 EDT 2007 ferguson>
#

for f; do
    out="`basename $f .raw`".aiff
    sox -V -t raw -s -w -r 16000 -c 1 "$f" -t aiff "$out"
done
