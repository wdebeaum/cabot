#!/bin/bash
#
# File: trips-cabot.sh
# Creator: George Ferguson
# Created: Wed Jun 20 10:38:13 2012
# Time-stamp: <Tue Feb  5 22:38:49 CST 2019 lgalescu>
#
# trips-cabot: Run TRIPS/CABOT
#
# This script uses the following environment variables, if set:
#  TRIPS_BASE			Root of TRIPS directory tree
#  TRIPS_LOGS			Where to save the logs
#  TRIPS_VOICE			m|f|<name>
# Run with -help to see the usage message.
#

echo 'This is TRIPS/CABOT version 0'
TRIPS_BASE_DEFAULT=/usr/local/trips

# Set TRIPS_BASE unless set
if test ! -z "$TRIPS_BASE"; then
    echo "Using your TRIPS_BASE=\"$TRIPS_BASE\""
else
    TRIPS_BASE=$TRIPS_BASE_DEFAULT; export TRIPS_BASE
    echo "Using TRIPS_BASE=\"$TRIPS_BASE\""
fi

TRIPS_HOST_DEFAULT=localhost
TRIPS_PORT_DEFAULT=6200

TRIPS_SYSNAME=cabot
TRIPS_SYSNAME_ALLCAPS=`echo $TRIPS_SYSNAME | tr "[:lower:]" "[:upper:]"`

# good Mac voices:
# - female: Ava/US > Fiona/Scot Karen/AU > Samantha/US Susan/US Serena/UK
# - male: Daniel/UK > Lee/AU > Tom/US
TRIPS_VOICE_DEFAULT=Daniel

#############################################################################
#
# Command-line

usage="trips-$TRIPS_SYSNAME [-debug] [-port 6200] [-logdir DIR] [-nouser] [-nolisp] [-nocsm] [-nochat] [-showgen] [-showtraffic] [-display tty] [-apparatusip <ip address>] [-docker]"

debug=false
port=''
logdir=''
apparatusip=''
display=''
voice="${TRIPS_VOICE:-$TRIPS_VOICE_DEFAULT}"
who=User
channel=Desktop
mode=continuous
display="${TRIPS_DISPLAY}"
usettsdic=''
nospeech=''
nospeechin=''
nospeechout=''
nochat=''
nobeep=''
noapparatus=f
sriba=f
docker=f
# dev options
nouser=''
nolisp=''
nocsm=''
showgen=false
showtraffic=''
# this should be generally empty
testmode=''

while test ! -z "$1"; do
    case "$1" in
	-port)		port="$2";	shift;;
	-voice)		voice="$2" ;	shift;;
	-mode)		mode="$2" ;	shift;;
	-who)		who="$2";	shift;;
	-channel)	channel="$2";	shift;;
	-display)	display="$2";	shift;;
	-logdir)	logdir="$2";	shift;;
	-apparatusip)   apparatusip="$2";   shift;;
	-debug)		debug=t;;
	-nodebug)	debug='';;
	-nouser)	nouser=t;;
	-nolisp)	nolisp=t;;
	-nocsm)		nocsm=t;;
	-nochat)	nochat=t;;
	-usettsdic)	usettsdic=t;;
	-nospeech)	nospeech=t;;
	-nospeechin)	nospeechin=t;;
	-nospeechout)	nospeechout=t;;
	-nobeep)	nobeep=t;;
	-quiet)		nospeech=t; nobeep=t;;
	-noapparatus)	noapparatus=t;;
	-sriba)      sriba=t;;
	-docker)	docker=t;;
	-showgen)	showgen=t;;
	-showtraffic)	showtraffic=t;;
	-testmode)	testmode=t;;
	-help|-h|-\?)
	    echo "usage: $usage"
	    exit 0;;
	*)
	    echo "$0: unknown argument: $1" 1>&2
	    echo "usage: $usage" 1>&2;
	    exit 1;;
    esac
    shift
done

# beep?
if test \( -z "$nospeech" -a -z "$nospeechout" \) -o ! -z "$nobeep"; then
    beep_kbd=false
else
    beep_kbd=true
fi

# set port options
TRIPS_PORT=${port:-$TRIPS_PORT_DEFAULT}
TRIPS_SOCKET=${TRIPS_HOST_DEFAULT}:${TRIPS_PORT}
export TRIPS_SOCKET
port_opt="-connect $TRIPS_SOCKET"

# set default character encoding to UTF-8
export LC_ALL=en_US.UTF-8
export JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8

# Make sure log directory exists
if test -z "$logdir"; then
    logdir=${TRIPS_LOGDIR:-${TRIPS_LOGS:-`pwd`}/`date '+%Y%m%dT%H%M'`}
fi
if test -d "$logdir"; then
    echo "Using log directory $logdir"
else
    echo "Creating log directory $logdir"
    mkdir -p "$logdir" || exit 1
fi
original_cwd=`pwd`
cd "$logdir" || exit 1

# Clean up any child process when we die
# Note [LG, 2011/03/11]: We used to use pkill to kill subprocesses. Turns out 
# this was not working well on Macs (for reasons i won't get into). 
# The solution below uses just ps and awk, which should be available on all the
# platforms we currently use. However, there are different implementations of 
# ps and awk out there; this code was only tested on my Mac (10.6.6).
trap cleanup 0 1 2 3 15

cleanup () {
    rm -f /tmp/trips$$
    rkill $$
    # will never get here
}

rkill() {
    for cpid in $(ps -x -o pid,ppid | awk -v ppid=$1 '$2==ppid {print $1}')
    do
	rkill $cpid
    done
    #echo "killing: $(ps -o pid=,command= $1 )"
    kill -9 $1 > /dev/null 2>&1
}

#############################################################################
#
# Here we go...
#

# The following will be sent to the facilitator (via stdin) once it starts
cat - <<_EOF_ >/tmp/trips$$
(register :name init)
(tell :content (status ready))
_EOF_

if test "$display" != "tty" -a -z "$nouser" -a -z "$nochat"; then
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
           :name keyboard
           :class TRIPS.KeyboardManager.KeyboardManager
           :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.KeyboardManager.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
                          "$TRIPS_BASE/etc/java/TRIPS.util.jar")
           :argv (-geometry 60x6+0-0
                  -fontsize 16
                  -who $who
                  -channel $channel
                  -title "$TRIPS_SYSNAME_ALLCAPS: Chat"
                  -beep $beep_kbd
		  -showGenerate $showgen
		  $port_opt)))
_EOF_
fi

# SRI Wrapper
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
	   :name SRIWrapper
	   :class TRIPS.SRIWrapper.SRIWrapper
	   :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.SRIWrapper.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.util.jar"
			  "$TRIPS_BASE/etc/java/json-simple-1.1.1.jar"
			  "$TRIPS_BASE/etc/java/jblas-1.2.3.jar"
			  "$TRIPS_BASE/src/SRIWrapper/src")
	   :argv ($port_opt $noapparatus $sriba $docker $apparatusip)))
_EOF_

# CSM
if test -z "$nocsm"; then
cat - <<_EOF_ >>/tmp/trips$$
(request
  :receiver facilitator
  :content (start-module
	:name CSM
	:class TRIPS.CollaborativeStateManager.CollaborativeStateManager
	:urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.CollaborativeStateManager.jar"
		       "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
		       "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
		       "$TRIPS_BASE/etc/java/TRIPS.util.jar"
		       "$TRIPS_BASE/src/CollaborativeStateManager/src")
	:argv ($port_opt
	   -data "$TRIPS_BASE/etc/$TRIPS_SYSNAME")))
_EOF_
fi

# Modules not started by the Facilitator have to start *after* the
# Facilitator so they can connect, so we use the form (sleep 5; foo) &
# to start them

# Lisp
if test -z "$nolisp"; then
  (sleep 5; $TRIPS_BASE/bin/trips-$TRIPS_SYSNAME-lisp) 2>&1 | tee lisp.log &
fi

# SpeechIn
if test -z "$nouser" -a -z "$nospeech" -a -z "$nospeechin"; then
# Ditto to comment about lisp regarding external programs in JVM
(sleep 5; $TRIPS_BASE/bin/trips_exec -socket $TRIPS_SOCKET -register t -name speech-in $TRIPS_BASE/bin/speechin -who $who -channel $channel -scenario $TRIPS_SYSNAME -mode $mode -wait t) >speechin.err 2>&1 &
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
	   :name speechcontroller
           :class TRIPS.SpeechController.SpeechController
	   :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.SpeechController.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.util.jar")
	   :argv (-rows 4
		  -columns 16
		  -fontsize 24
		  -geometry -0-0
		  -iconic false
		  -who $who
		  -channel $channel
		  -mode $mode
		  -dynLMs false
		  $port_opt)
))
_EOF_
fi

# SpeechOut
# TODO: use $port_opt?
if test -z "$nouser" -a -z "$nospeech" -a -z "$nospeechout"; then
(sleep 5; $TRIPS_BASE/bin/trips_exec -socket $TRIPS_SOCKET -register t -name speech-out $TRIPS_BASE/bin/SpeechOut ${voice:+-voice $voice} $TTSDIC >SpeechOut.err 2>&1) &
else
    # We don't use speech-out at all (run SpeechOutNot for transcript)
cat - <<_EOF_ >>/tmp/trips$$
(request
 :receiver facilitator
 :content (start-module
	   :name speech-out
	   :class TRIPS.SpeechOutNot.SpeechOutNot
	   :urlclasspath ("$TRIPS_BASE/etc/java/TRIPS.SpeechOutNot.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.TripsModule.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.KQML.jar"
			  "$TRIPS_BASE/etc/java/TRIPS.util.jar")
	   :argv ($port_opt)
))
_EOF_
fi

# Start TextTagger
if test -z "$nouser" -a -z "$nospeech" -a -z "$nospeechout"; then
(sleep 5; \
 $TRIPS_BASE/bin/TextTagger \
     $port_opt \
     -process-input-utterances yes \
     -terms-file $TRIPS_BASE/etc/$TRIPS_SYSNAME/BlockNames.tsv \
     -init-taggers terms-from-file,word-net \
     -default-type '(or affixes words punctuation terms-input terms-from-file word-net)' \
 2>&1 | tee $logdir/TextTagger.err ) &
(sleep 9; \
 echo '(request :receiver TextTagger :content (load-input-terms :input-terms ((word :lex "<sil>"))))' \
     | $TRIPS_BASE/bin/trips_cat ) &
else
(sleep 5; \
 $TRIPS_BASE/bin/TextTagger \
     $port_opt \
     -process-input-utterances yes \
     -terms-file $TRIPS_BASE/etc/$TRIPS_SYSNAME/BlockNames.tsv \
     -init-taggers terms-from-file,misspellings,word-net \
     -default-type '(or affixes words punctuation terms-from-file misspellings word-net)' \
 2>&1 | tee $logdir/TextTagger.err) &
fi

# GraphMatcher
if test -n "$testmode" ; then
    (sleep 5; $TRIPS_BASE/bin/GraphMatcher >GraphMatcher.err 2>&1) &
fi

# set display option for facilitator
if test -n "$nouser"; then
    display='none'
fi
if test -n "$display"; then
    display_opt="-display $display"
else
    display_opt=''
fi

# set traffic option for facilitator
if test -z "$showtraffic"; then
    traffic_opt="-notraffic"
else
    traffic_opt=''
fi

# Launch facilitator and send initial messages via stdin
cat /tmp/trips$$ |\
 $TRIPS_BASE/bin/Facilitator -port $TRIPS_PORT -title $TRIPS_SYSNAME_ALLCAPS -geometry 260x600-0+0 $traffic_opt $display_opt >facilitator.err 2>&1 &

# Wait for Facilitator to die
wait $!

# Bye
exit 0
