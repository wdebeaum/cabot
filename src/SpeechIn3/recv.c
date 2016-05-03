/*
 * recv.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: recv.c,v 1.10 2009/04/18 16:30:31 lgalescu Exp $
 */
/* History
 * lgalescu 2007/03/26 - added traffic light messaging 
 * lgalescu ????/??/?? - added dynamic LM updates
 */

#include <stdio.h>
#include <stdlib.h>
#include "trlib/parse.h"
#include "trlib/error.h"
#include "util/memory.h"
#include "util/error.h"
#include "util/debug.h"
#include "recv.h"
#include "send.h"
#include "sphinx.h"
#include "utt.h"
#include "log.h"
#include "main.h"

#include "trafficlight.h"

/*
 * Functions defined here:
 */
void receiveMsg(KQMLPerformative *perf);
static void receiveRequestStart(KQMLPerformative *perf, char **contents);
static void receiveRequestStop(KQMLPerformative *perf, char **contents);
static void receiveRequestChdir(KQMLPerformative *perf, char **contents);
static void receiveRequestExit(KQMLPerformative *perf, char **contents);
static void receiveTellStartConv(KQMLPerformative *perf, char **contents);
static void receiveRequestSetMode(KQMLPerformative *perf, char **contents);
static void receiveRequestSetChannelInfo(KQMLPerformative *perf, char **contents);
static void receiveRequestSetNewLMs(KQMLPerformative *perf, char **contents);
static void receiveRequestInterrupt(KQMLPerformative *perf, char **contents);
static void receiveRequestResume(KQMLPerformative *perf, char **contents);

/*
 * Data defined here:
 */
static TrlibParseDef defs[] = {
    { "request",	"start-listening",	receiveRequestStart },
    { "request",	"stop-listening",	receiveRequestStop },
    { "request",	"interrupt-listening",	receiveRequestInterrupt },
    { "request",	"resume-listening",	receiveRequestResume },
    { "request",	"set-mode",		receiveRequestSetMode },
    { "request",	"chdir",		receiveRequestChdir },
    { "request",	"set-channel-info",	receiveRequestSetChannelInfo },
    { "request",	"set-new-lms",		receiveRequestSetNewLMs },
    { "request",	"exit",			receiveRequestExit },
    { "request",	"hide-window",		NULL },
    { "request",	"show-window",		NULL },
    { "tell",		"start-conversation",	receiveTellStartConv },
    { "tell",		"end-conversation",	NULL },
    { "tell",		"new-scenario",		NULL },
    { "tell",		"modify-scenario",	NULL },
    { NULL,		NULL,			NULL }
};

static int seenStartConversation = 0;

//  ID for last set-new-lms request
char* lastSetNewLMsRequest = NULL;

/*	-	-	-	-	-	-	-	-	*/

static char *
findKeywordArg(char **contents, char *key)
{
    int i;
    for (i=0; contents[i] != NULL; i++) {
	if (strcasecmp(contents[i], key) == 0) {
	    return contents[i+1];
	} 
    }
    return NULL;
}

static void
setChannelInfo(char **contents)
{
    char *channel = findKeywordArg(contents, ":channel");
    if (channel != NULL) {
	channelName = gnewstr(channel);
    }
}

static void
setNewLMs(char **contents)
{
    char *dicfile = findKeywordArg(contents, ":dictionary");
    char *lmfile = findKeywordArg(contents, ":lm");
    DEBUG2("Setting new LMs: lm=%s, dic=%s", lmfile, dicfile);
    setDynLMs(gnewstr(lmfile), gnewstr(dicfile));
}

/*	-	-	-	-	-	-	-	-	*/

void
receiveMsg(KQMLPerformative *perf)
{
    DEBUG1("verb=%s", KQML_VERB(perf));
    trlibParsePerformative(perf, defs);
    DEBUG0("done");
}

static void
receiveRequestStart(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    if (recognitionMode == SPEECHIN_MODE_PTT) {
	setChannelInfo(contents);
	startPTT();
    } else {
	if (seenStartConversation) {
	    resumeContinuousMode();
	}
    }
    DEBUG0("done");
}

static void
receiveRequestStop(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    if (recognitionMode == SPEECHIN_MODE_PTT) {
	setChannelInfo(contents);
	stopPTT();
    } else {
	if (seenStartConversation) {
	    pauseContinuousMode();
	}
    }
    DEBUG0("done");
}

static void
receiveTellStartConv(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    /* uttReset(); */
    if (recognitionMode == SPEECHIN_MODE_CONTINUOUS && !doneSomeRecognition) {
	seenStartConversation = 1;
	startContinuous();
    }
    DEBUG0("done");
}

static void
receiveRequestSetMode(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    if (contents[1] != NULL) {
	if (strncasecmp(contents[1], "cont", 4) == 0) {
	    switchToContinuous();
	} else if (strcasecmp(contents[1], "ptt") == 0) {
	    switchToPTT();
	} else {
	    trlibErrorReply(perf, ERR_BAD_ARGUMENT, contents[1]);
	}
    }
    DEBUG0("done");
}

static void
receiveRequestSetChannelInfo(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    setChannelInfo(contents);
    DEBUG0("done");
}

static void
receiveRequestSetNewLMs(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    char *replyWith = KQMLGetParameter(perf, ":reply-with");
    if (dynLM) {
      sendSorryMsg(lastSetNewLMsRequest, "dropped");
    }
    setNewLMs(contents);
    lastSetNewLMsRequest = gnewstr(replyWith);
    DEBUG1("Registered request: %s", lastSetNewLMsRequest);
    DEBUG0("done");
}

static void
receiveRequestChdir(KQMLPerformative *perf, char **contents)
{
    char *dir;

    DEBUG0("");
    if (contents[1] == NULL) {
	trlibErrorReply(perf, ERR_MISSING_ARGUMENT, "chdir");
    } else {
	dir = KQMLParseThing(contents[1]);
	logChdir(dir);
	gfree(dir);
    }
    DEBUG0("done");
}

static void
receiveRequestExit(KQMLPerformative *perf, char **contents)
{
    int status = 0;

    DEBUG0("");
    if (contents[1] != NULL) {
	status = atoi(contents[1]);
    }
    exit(status);
}

static void
receiveRequestInterrupt(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    if (recognitionMode == SPEECHIN_MODE_PTT) {
      // nop
    } else {
	if (seenStartConversation) {
	    pauseContinuousMode();
	}
    }
    DEBUG0("done");
}

static void
receiveRequestResume(KQMLPerformative *perf, char **contents)
{
    DEBUG0("");
    if (recognitionMode == SPEECHIN_MODE_PTT) {
      // nop
    } else {
	if (seenStartConversation) {
	    resumeContinuousMode();
	}
    }
    DEBUG0("done");

}
