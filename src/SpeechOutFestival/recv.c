/*
 * recv.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * $Id: recv.c,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
 */
#include <stdio.h>
#include <string.h>
#include "trlib/parse.h"
#include "trlib/error.h"
#include "util/debug.h"
#include "recv.h"
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
    /* Send to transcript we can read it while we're listening - HACK! */
    sendTranscriptMsg(str);
    /* Send it to the synthesizer */
    DEBUG1("sending to Festival: \"%s\"", str);
    sendToSynth(str);
    /* Send done reply */
    DEBUG0("replying");
    sendDoneReply(perf);
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
