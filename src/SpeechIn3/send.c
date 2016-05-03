/*
 * send.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: send.c,v 1.4 2009/03/23 14:25:01 lgalescu Exp $
 *
 */
/* History
 * lgalescu 2006/10/26 - added traffic light messaging 
 * lgalescu 2007/09/12 - added "sorry" & "done"
 */

#include <stdio.h>
#include "KQML/KQML.h"
#include "trlib/send.h"
#include "util/debug.h"
#include "send.h"

/*
 * Functions defined here:
 */
void sendReadyMsg(void);
void sendLogMessage(int uttnum, char *result);
void sendTrafficLightMsg(tLight color);
void sendSorryMsg(char* inReplyTo, char* comment);
void sendDoneMsg(char* inReplyTo);

/*	-	-	-	-	-	-	-	-	*/

void
sendReadyMsg(void)
{
    KQMLPerformative *perf;

    DEBUG0("");
    if ((perf = KQMLNewPerformative("tell")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":content", "(module-status ready)");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendLogMessage(int uttnum, char *result)
{
    KQMLPerformative *perf;
    char content[1024];

    DEBUG0("");
    if ((perf = KQMLNewPerformative("request")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":receiver", "transcript");
    sprintf(content, "(log \"usr %03d %s\")", uttnum, result);
    KQMLSetParameter(perf, ":content", content);
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendTrafficLightMsg(tLight color)
{
    KQMLPerformative *perf;
    char msg[64];

    DEBUG0("");
    if ((perf = KQMLNewPerformative("request")) == NULL) {
	return;
    }
    sprintf(msg, "(trafficlight %s)", toString(color));
    KQMLSetParameter(perf, ":content", msg);
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendSorryMsg(char* inReplyTo, char* comment)
{
    KQMLPerformative *perf;

    DEBUG0("");
    if ((perf = KQMLNewPerformative("sorry")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":in-reply-to", inReplyTo ? inReplyTo : "nil");
    KQMLSetParameter(perf, ":comment", comment ? comment : "nil");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendDoneMsg(char* inReplyTo)
{
    KQMLPerformative *perf;

    DEBUG0("");
    if ((perf = KQMLNewPerformative("reply")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":in-reply-to", inReplyTo ? inReplyTo : "nil");
    KQMLSetParameter(perf, ":content", "(done)");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

