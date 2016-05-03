/*
 * send.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * Time-stamp: <Tue Sep 18 10:25:51 EDT 2001 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include "trlib/send.h"
#include "util/error.h"
#include "util/debug.h"
#include "send.h"

/*
 * Functions defined here:
 */
void sendReadyMsg(void);
void sendIndexReply(KQMLPerformative *perf, int value);
void sendDoneReply(KQMLPerformative *perf);
void sendStartedSpeakingMsg(void);
void sendStoppedSpeakingMsg(void);

/*	-	-	-	-	-	-	-	-	*/

void
sendReadyMsg(void)
{
    KQMLPerformative *perf;

    DEBUG0("send ready to facilitator");
    if ((perf = KQMLNewPerformative("tell")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":content", "(module-status ready)");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendIndexReply(KQMLPerformative *perf, int value)
{
    KQMLPerformative *reply;
    char content[32];

    DEBUG0("sending index reply");
    if ((reply = KQMLNewPerformative("reply")) == NULL) {
	return;
    }
    KQMLSetParameter(reply, ":receiver", KQMLGetParameter(perf, ":sender"));
    KQMLSetParameter(reply, ":in-reply-to",
		     KQMLGetParameter(perf,":reply-with"));
    sprintf(content, "(done %d)", value);
    KQMLSetParameter(reply, ":content", content);
    trlibSendPerformative(stdout, reply);
    KQMLFreePerformative(reply);
    DEBUG0("done");
}

void
sendDoneReply(KQMLPerformative *perf)
{
    static KQMLPerformative *reply;

    DEBUG0("sending done reply");
    if (reply == NULL) {
	if ((reply = KQMLNewPerformative("reply")) == NULL) {
	    return;
	}
    }
    KQMLSetParameter(reply, ":receiver", KQMLGetParameter(perf, ":sender"));
    KQMLSetParameter(reply, ":in-reply-to",
		     KQMLGetParameter(perf,":reply-with"));
    KQMLSetParameter(reply, ":content", "(done)");
    trlibSendPerformative(stdout, reply);
    DEBUG0("done");
}

#ifdef undef
void
sendLogMsg(char *str)
{
    static KQMLPerformative *msg;
    static char content[256];

    DEBUG0("sending log msg");
    if (msg == NULL) {
	if ((msg = KQMLNewPerformative("request")) == NULL) {
	    return;
	}
	KQMLSetParameter(msg, ":receiver", "transcript");
    }
    sprintf(content, "(log \"SYS say %.240s\")", str);
    KQMLSetParameter(msg, ":content", content);
    trlibSendPerformative(stdout, msg);
    DEBUG0("done");
}
#endif

void
sendStartedSpeakingMsg(void)
{
    KQMLPerformative *perf;

    DEBUG0("send started-speaking");
    if ((perf = KQMLNewPerformative("tell")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":content", "(started-speaking :who sys)");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendStoppedSpeakingMsg(void)
{
    KQMLPerformative *perf;

    DEBUG0("send stopped-speaking");
    if ((perf = KQMLNewPerformative("tell")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":content", "(stopped-speaking :who sys)");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

void
sendSpokenMsg(char *text)
{
    KQMLPerformative *perf;
    char content[1024];

    DEBUG0("send spoken");
    if ((perf = KQMLNewPerformative("tell")) == NULL) {
	return;
    }
    sprintf(content, "(spoken :who sys :what \"%s\")", text);
    KQMLSetParameter(perf, ":content", content);
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
    DEBUG0("done");
}

