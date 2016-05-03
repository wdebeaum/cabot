/*
 * main.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Jan 1996
 * Time-stamp: <Tue Sep 18 10:15:50 EDT 2001 ferguson>
 */
#include <stdio.h>
#include <string.h>
#include "trlib/debugarg.h"
#include "util/error.h"
#include "util/debug.h"
#include "truetalk.h"
#include "send.h"
#include "input.h"
#include "audio.h"

/*
 * Functions defined here: 
 */
int main(int argc, char **argv);
void programExit(int status);
static void parseArgs(int *argcp, char ***argvp);

/*
 * Data defined here: 
 */
static char *tts_host = NULL;
static char *audio_server = NULL;
static float speech_rate = 0.0;
/* Changed by A. Stent 24/11/97 to not send messages to transcript - pls. excuse 
   use of global variable. */
int no_transcript = 0;
char *speech_voice = "f";

/* Default this to stderr to have debugging on by default */
FILE *debugfp = NULL;
char *progname;

/*	-	-	-	-	-	-	-	-	*/

int
main(int argc, char **argv)
{
    /* Print our timestamp to stderr for id purposes */
    extern char *_timestamp;
    fprintf(stderr, "%s: %s\n", argv[0], _timestamp);
    if ((progname = strrchr(argv[0], '/')) == NULL) {
	progname = argv[0];
    } else {
	progname += 1;
    }
    /* Handle -debug if given */
    trlibDebugArg(argc, argv);
    /* Parse command line */
    parseArgs(&argc, &argv);
    /* Open audio server */
    if (audioInit(audio_server) < 0) {
	exit(-1);
    }
    /* Setup connection to TrueTalk */
    if (truetalkInit(tts_host) < 0) {
	exit(-1);
    }
    /* Set speech rate (if given) */
    if (speech_rate != 0) {
	char buf[16];
	sprintf(buf, "\\!R%.2f\n", speech_rate);
	truetalkSend(buf, NULL);
    }
    /* Set speech voice (if known) */
    if (speech_voice != 0) {
	char buf[16];
	sprintf(buf, "\\!uS%s\n", speech_voice);
	truetalkSend(buf, NULL);
    }
    /* Announce that we're ready */
    sendReadyMsg();
    /* Read KQML from stdin and feed it to the TTS engine */
    while (1) {
	input(0);
    }
    /*NOTREACHED*/
}

void
programExit(int status)
{
    /* Close connection to server */
    truetalkClose();
    /* Close audio device */
    audioClose();
    /* Done */
    exit(status);
}

/*	-	-	-	-	-	-	-	-	*/

static void
parseArgs(int *argcp, char ***argvp)
{
    int argc = *argcp;
    char **argv = *argvp;

    while (--argc > 0) {
	argv += 1;
	if (strcmp(argv[0], "-h") == 0) {
	    if (argc < 2) {
		ERROR0("missing hostname for -h");
	    } else {
		tts_host = argv[1];
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-audio") == 0) {
	    if (argc < 2) {
		ERROR0("missing name for -audio");
	    } else {
		audio_server = argv[1];
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-speechrate") == 0) {
	    if (argc < 2) {
		ERROR0("missing value for -speechrate");
	    } else {
		if (sscanf(argv[1], "%f", &speech_rate) != 1) {
		    speech_rate = 0.0;
		}
		argc -= 1;
		argv += 1;
	    }
	    /* Changed by A. Stent 24/11/97 to not send messages to transcript */
	} else if (strcmp(argv[0], "-notranscript") == 0) {
	  argc -= 1;
	  argv += 1;
	  no_transcript = 1;
	} else if (strcmp(argv[0], "-voice") == 0) {
	    if (argc < 2) {
		ERROR0("missing value for -voice");
	    } else {
		speech_voice = argv[1];
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-debug") == 0) {
	    /* handled by trlibDebugArg() */
	    argc -= 1;
	    argv += 1;
	} else {
	    fprintf(stderr, "usage: ttc [-debug where] [-audio server]\n");
	    exit(1);
	}
    }
    *argcp = argc;
    *argvp = argv;
}


