/*
 * main.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: main.c,v 1.2 2008/06/03 20:04:18 lgalescu Exp $
 */
#include <stdio.h>
#include "wordbuf.h"
#include "send.h"
#include "input.h"
#include "sphinx.h"
#include "log.h"
#include "util/memory.h"
#include "util/error.h"
#include "util/debug.h"
#include "trlib/debugarg.h"

static void parseArgs(int argc, char **argv);

/* For debugging */
char *progname;
FILE *debugfp;

char *s3argsfile = NULL;
char *fileBasename = "utt";
char *channelName = "unknown";
int waitForStartConversation = 1;

int
main(int argc, char **argv)
{
    /* Initialize for debug */
    progname = argv[0];
#ifdef DEBUG
    debugfp = stderr;
#endif
    /* Parse our arguments */
    parseArgs(argc, argv);
    /* Initialize sphinx */
    if (initSphinx(s3argsfile) < 0) {
		exit(1);
    }
    /* Tell everyone we're ready */
    sendReadyMsg();
    /* Now, do we just start listening or do we wait? */
    if ((recognitionMode == SPEECHIN_MODE_CONTINUOUS) && !waitForStartConversation) {
		startContinuous();
    }
    /* Either we didn't start above, or we did and then we stopped,
		which must mean that we are now in PTT mode. */
    /* Wait for a message and dispatch it */
    while(1) {
		if (input(SPEECH_INPUT_BLOCK) <= 0) {
			break;
		}
    }
	shutdownSphinx();
    exit(0);
}

static void
parseArgs(int argc, char **argv)
{
    trlibDebugArg(argc, argv);
    while (--argc) {
		argv += 1;
		if (strcmp(argv[0], "-bufnum") == 0) {
			wordbufSetBufferLen(atoi(argv[1]));
		} else if (strcmp(argv[0], "-logdir") == 0) {
			logChdir(argv[1]);
		} else if (strcmp(argv[0], "-basename") == 0) {
			fileBasename = gnewstr(argv[1]);
		} else if (strcmp(argv[0], "-channel") == 0) {
			channelName = gnewstr(argv[1]);
		} else if (strcmp(argv[0], "-mode") == 0) {
			if (strcasecmp(argv[1], "ptt") == 0 ||
				strncasecmp(argv[1], "click", 5) == 0) {
				recognitionMode = SPEECHIN_MODE_PTT;
			} else if (strncasecmp(argv[1], "cont", 4) == 0) {
				recognitionMode = SPEECHIN_MODE_CONTINUOUS;
			} else {
				ERROR1("invalid -mode: %s", argv[1]);
			}
		} else if (strcmp(argv[0], "-wait") == 0) {
			if (strncasecmp(argv[1], "t", 1) == 0 ||
				strcasecmp(argv[1], "yes") == 0 ||
				strcasecmp(argv[1], "1") == 0) {
				waitForStartConversation = 1;
			} else {
				waitForStartConversation = 0;
			}
		} else if (strcmp(argv[0], "-s3args") == 0) {
			/* lgalescu <lgalescu@ihmc.us> -- for s3 all params should be in a file! */
			s3argsfile = gnewstr(argv[1]);
		} else {
			/* there shouldn't be anything else! */
		}	
		argc--;
		argv++;
    }
}
