#!/usr/grads/bin/perl5
#
# tspeechout.pl: Run TrueTalk server and client
#
# George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
# Time-stamp: <Thu Apr 16 15:24:12 EDT 1998 ferguson>
#
# This script just forks two processes, one for the server and one for
# the client. It waits for either to exit, then kills the other and
# exits itself.
#
# Any arguments are passed to the client "ttc". A "-h" argument is also
# used to specify the host on which to run the TrueTalk server.
#

$TRIPS_BASE_DEFAULT = "/u/trains/97/3.2";

if (!($TRIPS_BASE = $ENV{'TRIPS_BASE'})) {
    $TRIPS_BASE = $TRIPS_BASE_DEFAULT;
} elsif ($TRIPS_BASE ne $TRIPS_BASE_DEFAULT) {
    print STDERR "$0: using your TRIPS_BASE=$TRIPS_BASE (default is $TRIPS_BASE_DEFAULT)\n";
}

#$TTS = "$TRIPS_BASE/bin/tts -i";
#$TTC = "$TRIPS_BASE/bin/ttc";
$TTS = "./tts.sh -i";
$TTC = "./ttc";

#
# If there's a -h argument, it specifies where to run the server
#
for ($i=0; $i <= $#ARGV; $i++) {
    if ($ARGV[$i] eq "-h") {
	$tts_host = $ARGV[$i+1];
    }
}

#
# Arrange to kill server if we're killed
#
$SIG{'INT'} = 'handler';
$SIG{'QUIT'} = 'handler';
$SIG{'TERM'} = 'handler';
$SIG{'CHLD'} = 'handler';
sub handler {
    local($sig) = @_;
    print STDERR "$0: caught SIG$sig; cleaning up\n";
    undef(%SIG);
    if ($serverpid) {
	#print STDERR "killing TT server (pid $serverpid)\n";
	kill "TERM", $serverpid;
    }
    if ($clientpid) {
	#print STDERR "killing TT client (pid $clientpid)\n";
	kill "TERM", $clientpid;
    }
    exit(-1);
}

#
# Start the server (in the background)
#
if (($serverpid = fork()) == 0) {
    # Child
    if ($tts_host) {
	print STDERR "$0: starting TrueTalk server on $tts_host\n";
	exec("/usr/ucb/rsh", "-n", $tts_host, $TTS) ||
	    die("$0: couldn't exec /usr/ucb/rsh: $!\n");
    } else {
	print STDERR "$0: starting TrueTalk server\n";
	exec($TTS) || die("$0: couldn't exec $TTS: $!\n");
    }
}

#
# Sleep to let it get started
#
sleep(8);

#
# Start the client
#
if (($clientpid = fork()) == 0) {
    # Child
    print STDERR "$0: starting TrueTalk client\n";
    exec("$TTC @ARGV") || die("$0: couldn't exec $TTC: $!\n");
}

#
# Wait for someone to exit
#
wait();

#
# Kill either process if still running
#
&handler('TERM');

exit(0);
