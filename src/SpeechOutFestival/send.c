/*
 * send.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * $Id: send.c,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
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

    DEBUG0("send subscribe to facilitator");
    if ((perf = KQMLNewPerformative("subscribe")) == NULL) {
	return;
    }
    KQMLSetParameter(perf, ":content", "(request &key :content (say . *))");
    trlibSendPerformative(stdout, perf);
    KQMLFreePerformative(perf);
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

/*
 * gf: 9/16/2002:
 *  This grotty code has come back to life with the AT&T synthesizer.
 *  Should really make the KQML data structures complete, as in
 *  KQMLList and the like. But that is more than I want to do to
 *  support crappy C and C++. So instead, I use the sprintf below.
 *  Barfola.
 */
void
sendDoneReply(KQMLPerformative *perf)
{
    static KQMLPerformative *reply;
    static char buf[1024];

    DEBUG0("sending done reply");
    if (reply == NULL) {
	if ((reply = KQMLNewPerformative("reply")) == NULL) {
	    return;
	}
    }
    KQMLSetParameter(reply, ":receiver", KQMLGetParameter(perf, ":sender"));
    KQMLSetParameter(reply, ":in-reply-to",
		     KQMLGetParameter(perf,":reply-with"));
    sprintf(buf, "(done %.1000s)", KQMLGetParameter(perf, ":content"));
    KQMLSetParameter(reply, ":content", buf);
    trlibSendPerformative(stdout, reply);
    DEBUG0("done");
}

void
sendTranscriptMsg(char *str)
{
    static KQMLPerformative *msg;
    static char content[1024];

    DEBUG0("sending transcript msg");
    if (msg == NULL) {
	if ((msg = KQMLNewPerformative("tell")) == NULL) {
	    return;
	}
    }
    sprintf(content, "(spoken :who sys :what \"%.1000s\")", str);
    KQMLSetParameter(msg, ":content", content);
    trlibSendPerformative(stdout, msg);
    DEBUG0("done");
}

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

