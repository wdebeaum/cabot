/*
 * send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * Time-stamp: <Tue Jun 12 11:18:26 EDT 2001 ferguson>
 */

#ifndef _send_h_gf
#define _send_h_gf

#include "KQML/KQML.h"

extern void sendReadyMsg(void);
extern void sendIndexReply(KQMLPerformative *perf, int value);
extern void sendDoneReply(KQMLPerformative *perf);
#ifdef undef
extern void sendLogMsg(char *str);
#endif

#endif
