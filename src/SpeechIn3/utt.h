/*
 * utt.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 31 Jan 1996
 * $Id: utt.h,v 1.2 2009/04/18 16:30:31 lgalescu Exp $
 */

#ifndef _utt_h_gf
#define _utt_h_gf

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define MAX_UTT_LEN 1024

/*
 * Data declared here.
 */
extern int uttnum;
extern char uttid[64];
extern char uttresult[MAX_UTT_LEN];
extern int uttInterrupted;

extern void uttId(void);
extern void uttSetInterrupted(int isInterrupted);
extern void uttStart(void);
extern void uttStop(void);
extern void uttResult(void);
extern void uttUpdate(void);
extern void uttDone(void);
extern void uttReset(void);
extern void uttChdir(char *dir);

#endif
