/*
 * recv.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * Time-stamp: <Fri Apr 17 15:47:55 EDT 1998 ferguson>
 */
#include <stdio.h>
#include <string.h>
#include "trlib/parse.h"
#include "trlib/error.h"
#include "util/debug.h"
#include "recv.h"
#include "truetalk.h"
#include "main.h"

/*
 * Functions defined here:
 */
void receiveMsg(KQMLPerformative *perf);
static void receiveRequestSay(KQMLPerformative *perf, char **contents);
static void receiveRequestExit(KQMLPerformative *perf, char **contents);

/*
 * Data defined here:
 */
static TrlibParseDef defs[] = {
    { "request",	"say",			receiveRequestSay },
    { "request",	"exit",			receiveRequestExit },
    { "request",	"hide-window",		NULL },
    { "request",	"show-window",		NULL },
    { "request",	"chdir",		NULL },
    { "tell",		"start-conversation",	NULL },
    { "tell",		"end-conversation",	NULL },
    { "tell",		"new-scenario",		NULL },
    { "tell",		"modify-scenario",	NULL },
    { NULL,		NULL,			NULL }
};

/* Changed by A. Stent 24/11/97 to not send messages to transcript.  no_transcript is
   declared in main.c */
extern int no_transcript;

/*	-	-	-	-	-	-	-	-	*/

void
receiveMsg(KQMLPerformative *perf)
{
    DEBUG1("verb=%s", KQML_VERB(perf));
    trlibParsePerformative(perf, defs);
    DEBUG0("done");
}

static void
receiveRequestSay(KQMLPerformative *perf, char **contents)
{
    char *str;

    DEBUG0("");
    if (contents[1] == NULL) {
	trlibErrorReply(perf, ERR_MISSING_ARGUMENT, contents[0]);
	return;
    }
    /* Convert from KQML to normal string */
    if ((str = KQMLParseString(contents[1])) == NULL) {
	trlibErrorReply(perf, ERR_BAD_VALUE, contents[1]);
	return;
    }
    /* Send it to TrueTalk */
    DEBUG1("sending to truetalk: \"%s\"", str);
    truetalkSend(str, perf);
#ifdef undef
    /* Also send to the transcript that something was said */
    sendLogMsg(str);
#endif
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
    programExit(status);
}
