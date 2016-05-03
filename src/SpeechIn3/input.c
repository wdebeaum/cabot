/*
 * input.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: input.c,v 1.1.1.1 2005/01/14 19:48:13 ferguson Exp $
 */
#include <stdio.h>
#include "trlib/input.h"
#include "util/debug.h"
#include "input.h"
#include "recv.h"

/*
 * Functions defined here:
 */
int input(SpeechInputFlag flag);

/*	-	-	-	-	-	-	-	-	*/
/*
 * Returns: < 0 on error
 *          = 0 if nothing was read (can mean EOF, esp. if flag==BLOCK)
 *          > 0 otherwise, ie. something was read or non-fatal error
 */
int
input(SpeechInputFlag flag)
{
    int ret;

    DEBUG1("flag=%s", (flag == SPEECH_INPUT_BLOCK ? "BLOCK" : "NO_HANG"));
    if (flag == SPEECH_INPUT_BLOCK) {
	ret = trlibInput(0, TRLIB_BLOCK, receiveMsg);
    } else {
	ret = trlibInput(0, TRLIB_DONTBLOCK, receiveMsg);
    }
    DEBUG1("done, ret=%d", ret);
    return ret;
}

