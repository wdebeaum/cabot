/*
 * send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * Time-stamp: <Fri Feb 27 11:39:45 EST 1998 ferguson>
 */

#ifndef _send_h_gf
#define _send_h_gf

#include "KQML/KQML.h"

extern void sendMonitorMsg(void);
extern void sendOpenMsg(void);
extern void sendReadyMsg(void);
extern void sendDrainMsg(KQMLPerformative *perf);
extern void sendIndexReply(KQMLPerformative *perf, int value);
extern void sendDoneReply(KQMLPerformative *perf);
#ifdef undef
extern void sendLogMsg(char *str);
#endif

#endif
