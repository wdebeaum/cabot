#!/bin/sh
#
# speechin.sh: Wrapper for Sphinx in TRIPS
#
# George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
# $Id: speechin.sh,v 1.9 2009/04/13 14:43:39 lgalescu Exp $
#
# Lucian Galescu <lgalescu@ihmc.us> 2004/08/31
# Adapted for use with Sphinx3.
#

usage="speechin [-sex m|f] [-scenario calo|etc] [-task calo|etc]
         [-mode continuous|ptt] [-wait yes|no] [-channel Phone|Desktop] 
	 [-amdir DIR] [-lmdir DIR] [-help]"

TRIPS_BASE_DEFAULT=
if test -z "$TRIPS_BASE"; then
    TRIPS_BASE=$TRIPS_BASE_DEFAULT
elif test "$TRIPS_BASE" != "$TRIPS_BASE_DEFAULT"; then
    echo "$0: using your TRIPS_BASE=$TRIPS_BASE" 1>&2
fi

# Command line
sex='m'
scenario='calo'
mode='continuous'
who='Helen'
channel='Desktop'
amdir=''
lmdir=''
task=''
wait='no'
while test $# -gt 0; do
    case $1 in
	-help) 		echo "$usage" 1>&2; exit 0;;
	-sex)		sex="$2"; shift;;
	-scenario)	scenario="$2"; shift;;
	-mode)		mode="$2"; shift;;
	-wait)		wait="$2"; shift;;
	-who)		who="$2"; shift;;
	-channel)      	channel="$2"; shift;;
	-amdir)		amdir="$2"; shift;;
	-lmdir)		lmdir="$2"; shift;;
	-task)		task="$2"; shift;;
	*)		break;;
    esac
    shift
done

if test -z "$task"; then
    task=${scenario}
fi

if test -z "$TRIPS_BASE"; then
    # Running in build directory
    SPHINX=./sphinx
else
    SPHINX=$TRIPS_BASE/bin/sphinx
fi

if test -z "$TRIPS_BASE"; then
    # Running in build directory
    amdir=../Sphinx3/model/hmm/hub4_cd_continuous_8gau_1s_c_d_dd
elif test -z "$amdir"; then
    amdir=$TRIPS_BASE/etc/SpeechModels
fi

if test -z "$TRIPS_BASE"; then
    # Running in build directory
    lmdir=../SpeechLM/$scenario
elif test -z "$lmdir"; then
    lmdir=$TRIPS_BASE/etc/SpeechLM/$scenario
fi

DICT=$lmdir/$task.dic
LMFN=$lmdir/$task.lm.DMP

# write arguments to temporary file
cat - <<_EOF_>/tmp/trips$$
-mdef           $amdir/hub4opensrc.6000.mdef
-mean           $amdir/means
-var            $amdir/variances
-mixw           $amdir/mixture_weights 
-tmat           $amdir/transition_matrices
-subvq          $amdir/8gau.6000sen.quant
-fdict          $amdir/filler.dic
-feat           1s_c_d_dd
-upperf         6855.49756
-lowerf         133.33334
-nfilt          40
-nfft		512
-samprate       16000
-dict           $DICT
-lm             $LMFN
-agc            max
-varnorm        no
-cmn            current
-cmfn		$amdir/cm_16k_40_512
-cmsave         cep_means.new
-lw             11.7
-reportpron     0
-reportfill     0
_EOF_

exec $SPHINX \
    -mode $mode \
    -channel $channel \
    -wait $wait \
    -s3args /tmp/trips$$ \
2> sphinx.log

# other args for sphinx: 
# -basename "utt"
# -bufnum 2
