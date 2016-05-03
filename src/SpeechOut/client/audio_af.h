/*
 * audio_af.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  5 Aug 1996
 * Time-stamp: <Wed May 14 13:54:00 EDT 1997 ferguson>
 */

#ifndef _audio_af_h_gf
#define _audio_af_h_gf

extern int audioInit_AF(char *name);
extern void audioClose_AF(void);
extern void audioWrite_AF(char *bytes, int num_bytes);
extern void audioSync_AF(void);

#endif
