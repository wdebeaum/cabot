/*
 * send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * $Id: send.h,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
 */

#ifndef _send_h_gf
#define _send_h_gf

#ifdef __cplusplus
extern "C" {
#endif

#include "KQML/KQML.h"


extern void sendReadyMsg(void);
extern void sendIndexReply(KQMLPerformative *perf, int value);
extern void sendDoneReply(KQMLPerformative *perf);
extern void sendTranscriptMsg(char *str);

#ifdef __cplusplus
}
#endif


#endif
