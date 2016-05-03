/*
 * wordbuf.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: wordbuf.h,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */

#ifndef _wordbuf_h_gf
#define _wordbuf_h_gf

#include "live.h"

extern void wordbufOutput(int final);
extern void wordbufSetBufferLen(int len);
extern void wordbufReset(void);

#endif
