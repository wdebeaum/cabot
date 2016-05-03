/*
 * audio.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  5 Aug 1996
 * Time-stamp: <Wed May 14 13:43:56 EDT 1997 ferguson>
 */

#ifndef _audio_h_gf
#define _audio_h_gf

extern int audioInit(char *name);
extern void audioClose(void);
extern void audioWrite(char *bytes, int num_bytes);
extern void audioSync(void);

#endif
