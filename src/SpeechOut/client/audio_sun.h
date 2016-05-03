/*
 * audio_sun.h
 *
 * George Ferguson, ferguson@cs.rochester.edu,  5 Aug 1996
 * Time-stamp: <Wed May 14 13:54:27 EDT 1997 ferguson>
 */

#ifndef _audio_sun_h_gf
#define _audio_sun_h_gf

extern int audioInit_SUN(char *name);
extern void audioClose_SUN(void);
extern void audioWrite_SUN(char *bytes, int num_bytes);
extern void audioSync_SUN(void);

#endif
