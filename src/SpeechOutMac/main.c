/*
 * main.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Jan 1996
 * Time-stamp: <Thu Aug  2 14:27:58 CDT 2018 lgalescu>
 */
#include <stdio.h>
#include <string.h>
#include "trlib/debugarg.h"
#include "util/error.h"
#include "util/debug.h"
#include "send.h"
#include "input.h"
#include "tts.h"
#include "pronunciation.h"

/*
 * Functions defined here: 
 */
int main(int argc, char **argv);
void programExit(int status);
static void parseArgs(int *argcp, char ***argvp);

/*
 * Data defined here: 
 */
static float speech_rate = 0.0;
char *voice = NULL;
char *dictfile = NULL; /* pronunciation dictionary */

/* Default this to stderr to have debugging on by default */
FILE *debugfp = NULL;
int debuglevel = 0; /* currently used in pronunciation.c only */
char *progname;

/*	-	-	-	-	-	-	-	-	*/

int
main(int argc, char **argv)
{
    if ((progname = strrchr(argv[0], '/')) == NULL) {
	progname = argv[0];
    } else {
	progname += 1;
    }
    /* Handle -debug if given */
    trlibDebugArg(argc, argv);
    /* Parse command line */
    parseArgs(&argc, &argv);
    /* Initialize TTS */
    if (tts_init(voice, dictfile) < 0) {
	exit(-1);
    }
    /* Set speech rate (if given) */
    if (speech_rate != 0) {
	char buf[16];
	sprintf(buf, "\\!R%.2f\n", speech_rate);
	tts_send(buf, NULL);
    }
    /* Send subscriptions */
    sendSubscriptions();
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
    tts_close();
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
	if (strcmp(argv[0], "-speechrate") == 0) {
	    if (argc < 2) {
		ERROR0("missing value for -speechrate");
	    } else {
		if (sscanf(argv[1], "%f", &speech_rate) != 1) {
		    speech_rate = 0.0;
		}
		argc -= 1;
		argv += 1;
	    }
	} else if (strcmp(argv[0], "-voice") == 0) {
	    if (argc < 2) {
		ERROR0("missing value for -voice");
	    } else {
		voice = argv[1];
		argc -= 1;
		argv += 1;
		/* gf: 14 Dec 2004: Default female and male voices */
		if (strcmp(voice, "f") == 0) {
		    voice = "Victoria";
		} else if (strcmp(voice, "m") == 0) {
		    voice = "Bruce";
		}
	    }
	} else if (strcmp(argv[0], "-dic") == 0) {
	    if (argc < 2) {
		fprintf(stderr, "missing value for -dic");
	    } else {
		dictfile = argv[1];
		argc -= 1;
		argv += 1;
	    }	    
	} else if (strcmp(argv[0], "-debug") == 0) {
	    /* handled by trlibDebugArg() */
	    argc -= 1;
	    argv += 1;
	} else if (strcmp(argv[0], "-debuglevel") == 0) {
	    if (argc < 2) {
		fprintf(stderr, "missing value for -debuglevel");
	    } else {
		if (sscanf(argv[1], "%d", &debuglevel) != 1)
		    debuglevel = 0;
		argc -= 1;
		argv += 1;
	    }
	} else {
	    fprintf(stderr, "usage: speechout-mac [-voice m|f|<name>] [-speechrate F] [-debug WHAT] [-debuglevel D] [-dic <dic-file>]\n");
	    exit(1);
	}
    }
    *argcp = argc;
    *argvp = argv;
}


