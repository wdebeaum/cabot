/*
 * truetalk.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Jan 1996
 * Time-stamp: <96/03/12 13:42:22 ferguson>
 */

#ifndef _truetalk_h_gf
#define _truetalk_h_gf

#include "KQML/KQML.h"

extern int initTrueTalk(char *host);
extern void sendTrueTalk(char *buf, KQMLPerformative *perf);
extern void closeTrueTalk(void);

#endif
