/*
 * send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * Time-stamp: <Thu Aug  2 10:51:07 CDT 2018 lgalescu>
 */

#ifndef _send_h_gf
#define _send_h_gf

#include "KQML/KQML.h"

extern void sendSubscriptions(void);
extern void sendMonitorMsg(void);
extern void sendOpenMsg(void);
extern void sendReadyMsg(void);
extern void sendDrainMsg(KQMLPerformative *perf);
extern void sendIndexReply(KQMLPerformative *perf, int value);
extern void sendDoneReply(KQMLPerformative *perf);
extern void sendSpokenMsg(KQMLPerformative *perf, char *str);
#ifdef undef
extern void sendLogMsg(char *str);
#endif

#endif
