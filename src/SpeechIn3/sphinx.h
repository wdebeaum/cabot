/*
 * sphinx.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: sphinx.h,v 1.5 2009/04/18 16:30:31 lgalescu Exp $
 */

#ifndef _sphinx_h_gf
#define _sphinx_h_gf

typedef enum {
    SPEECHIN_MODE_PTT,
    SPEECHIN_MODE_CONTINUOUS
} RecognitionMode;

extern RecognitionMode recognitionMode;
extern int doneSomeRecognition;
extern int continuousModePaused;

extern char* dynLM;
extern char* dynDic;

extern int initSphinx(char *argsfile);
extern int shutdownSphinx(void);
extern void pauseContinuousMode(void);
extern void resumeContinuousMode(void);

extern void startPTT(void);
extern void stopPTT(void);
extern void startContinuous(void);
extern void switchToPTT(void);
extern void switchToContinuous(void);

#endif
