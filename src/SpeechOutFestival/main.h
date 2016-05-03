/*
 * main.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Jan 1996
 * $Id: main.h,v 1.1.1.1 2005/01/14 19:48:14 ferguson Exp $
 */

#ifndef _main_h_gf
#define _main_h_gf

extern int main(int argc, char **argv);
extern void handleParameters(char **argv);
extern void programExit(int status);
extern void sendToSynth(const char *str);

#endif
