/*
 * recv.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: recv.h,v 1.2 2009/03/23 14:25:01 lgalescu Exp $
 */

#ifndef _recv_h_gf
#define _recv_h_gf

#include "KQML/KQML.h"

extern char* lastSetNewLMsRequest;

extern void receiveMsg(KQMLPerformative *perf);

#endif
