/*
 * input.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: input.h,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */

#ifndef _input_h_gf
#define _input_h_gf

typedef enum {
    SPEECH_INPUT_BLOCK,
    SPEECH_INPUT_NO_HANG
} SpeechInputFlag;

extern int input(SpeechInputFlag flag);

#endif
