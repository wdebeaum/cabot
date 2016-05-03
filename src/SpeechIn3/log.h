/*
 * log.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: log.h,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */

#ifndef _log_h_gf
#define _log_h_gf

extern FILE *logfp;
extern char *logdir;
extern void logChdir(char *dir);

#endif
