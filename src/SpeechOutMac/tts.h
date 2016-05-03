/*
 * tts.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 23 Sep 2002
 * Time-stamp: <Mon Sep 23 23:15:01 EDT 2002 ferguson>
 */

#ifndef _tts_h_gf
#define _tts_h_gf

#include "KQML/KQML.h"

extern int tts_init();
extern void tts_send(char *txt, KQMLPerformative *perf);
extern void tts_close(void);

#endif
