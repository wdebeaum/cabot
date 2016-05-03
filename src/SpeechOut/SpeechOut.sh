#!/bin/sh
#
# File: SpeechOut.sh
# Creator: George Ferguson
# Created: Mon Feb 19 14:11:42 2007
# Time-stamp: <Mon Feb 19 14:41:16 EST 2007 ferguson>
#

# TRIPS_BASE
TRIPS_BASE_DEFAULT=/usr/local/trips
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=$TRIPS_BASE_DEFAULT
fi

# Configuration default
SPEECH_OUT_COMPONENT_DEFAULT=SpeechOutNot

# Check environment
if test -z "$SPEECH_OUT_COMPONENT"; then
    SPEECH_OUT_COMPONENT="$SPEECH_OUT_COMPONENT_DEFAULT"
fi

# Check cmd-line
case "$1" in
    SpeechOutMac|SpeechOutFestival|SpeechOutNot)
        SPEECH_OUT_COMPONENT="$1"
        shift;;
esac

# Off we go
exec $TRIPS_BASE/bin/$SPEECH_OUT_COMPONENT "$@"
