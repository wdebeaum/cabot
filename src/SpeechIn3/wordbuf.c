/*
 * wordbuf.c : Incremental word output from the typed input
 *
 * George Ferguson, ferguson@cs.rochester.edu,  9 Feb 1995
 * $Id: wordbuf.c,v 1.4 2008/06/17 14:55:32 lgalescu Exp $
 *
 * This code incrementally outputs words from the current hypothesis.
 * Each time its called, it compares what it has previously sent, backs
 * up if necessary, and sends new words. It does not send the last `bufnum'
 * words recognized so far, on the grounds that they are likely to change.
 * Of course, since that word has to get out sometime, the FINAL flag
 * is provided to force it out.
 *
 * NOTE: The parser considers words with an apostrophe or hyphen to be
 * two words. We have to remember this when generating :index tags for the
 * output.
 * NOTE: Sphinx generates words (lexemes) with underscores in them, which
 * we need to convert to spaces before processing.
 *
 * History:
 *   95 Feb 09  ferguson - Created.
 *   96 Mar 07  ringger -  For each word, get start frame, end frame,
 *                         and normalized acoustic score from Sphinx-II.
 *   96 Aug 13  ringger  - Using frames instead of word indexes for backto.
 *   97 May 14  ringger  - Diff'ing start frames and end frames in addition
 *                         to words.  i.e., words don't suffice.
 *   02 Sep 26  ferguson - Updated for sphinx2-0.4 hyp structure
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
/* Sphinx includes */
#include "live.h"
/* TRIPS includes */
#include "util/error.h"
#include "util/debug.h"
#include "wordbuf.h"
#include "bcast.h"
#include "main.h"
#include "utt.h"
/*
 * Functions defined here:
 */
void wordbufOutput(int isFinal);
void wordbufSetBufferLen(int len);
void wordbufReset(void);
static int countSpecials(char *s);

/*
 * Data defined here:
 */
static int bufnum = 2;

#define MAX_WORDS	128
#define MAX_WORD_LEN	32
static char oldWords[MAX_WORDS][MAX_WORD_LEN];
static int  oldStartFrames[MAX_WORDS];
static int  oldEndFrames[MAX_WORDS];
static char newWords[MAX_WORDS][MAX_WORD_LEN];
static int numOld, numNew;

/*	-	-	-	-	-	-	-	-	*/

void
wordbufOutput(int isFinal)
{
  static char content[256];		/* static for speed, maybe */
  int index, diffpos, i, n;
  int frame;
	
  DEBUG1("new result=\"%s\"", uttresult);
  DEBUG2("uttnum=\"%d\", isFinal=%d", uttnum, isFinal);
  //#ifdef undef
  {
    char *s;
    /* Convert underscores to spaces (don't sweat modifying the string...) */
    for (s=uttresult; *s; s++) {
      if (*s == '_') {
	*s = ' ';
      }
    }
  }
  //#endif
  /* Copy new hyp into newWords[] */
  numNew = parthyplen;
  for (i = 0; i < numNew; i++) {
    strcpy(newWords[i], parthyp[i].word);
  }

  /* Parser indices start at 1 and count apostrophe as splitting a word */
  index = 1;
  /* Compare new words to old to find where they differ */
  for (diffpos=0; diffpos < numNew && diffpos < numOld; diffpos++) {
    DEBUG4("%d: new=\"%s\", old=\"%s\", index=%d",
	   diffpos, newWords[diffpos], oldWords[diffpos], index);
    if (strcmp(newWords[diffpos], oldWords[diffpos]) != 0) {
      /* Words differ */
      break;
    } else if ((parthyp[diffpos].sf != oldStartFrames[diffpos])
	       || (parthyp[diffpos].ef != oldEndFrames[diffpos])) {
      /* Start or End frames differ */
      break;
    } else {
      /* Words are the same, just adjust index */
      index += 1 + countSpecials(newWords[diffpos]);
    }
  }
  DEBUG4("numOld = %d, numNew=%d, diffpos=%d, index=%d",
	 numOld, numNew, diffpos, index);
  /* Maybe some old words need to be backed up */
  if (diffpos < numOld) {
    if (diffpos < 0) {
      ERROR0("trying to convert neg. index to frame!\n");
    }
    frame = parthyp[diffpos].sf;
    DEBUG1("backto %d", frame);
    sprintf(content, "(backto :frame %d :uttnum %d)", frame, uttnum);
    broadcast(content);
		
    /* Send gap/SIL if previous word ef less then this sf */
    /* gf: No valid score for partial hypotheses (apparently) */
    if(parthyp[diffpos].sf > oldStartFrames[diffpos]) {
      sprintf(content, 
	      "(word \"<SIL>\" :index %d :frame (%d %d) :uttnum %d :channel %s :direction input)",
	      index++, (parthyp[diffpos-1].ef + 1), (parthyp[diffpos].sf - 1),
	      uttnum, channelName);
      broadcast(content);
    };
    numOld = diffpos;
  } else {
    DEBUG0("don't need to backup");
  }
  /* Unless this is a "final" output, throw away the buffered words */
  if (!isFinal) {
    numNew -= bufnum;
    if (numNew <= 0) {
      DEBUG0("done (nothing to output) --------");
      return;
    }
    DEBUG2("bufnum=%d, numNew now=%d", bufnum, numNew);
  }
  /* Maybe some new words need to be added */
  if (diffpos < numNew) {
    DEBUG0("outputting new words...");
    for (i=diffpos; i < numNew; i++) {
      /* pmc: Adjust start frame to be imediately after previous end frame */
      if( (i > 0) && (parthyp[i].sf > (parthyp[i-1].ef + 1))) { 
	sprintf(content, 
		"(word \"<SIL>\" :index %d :frame (%d %d) :uttnum %d :channel %s :direction input :peak %d)",
		index++, parthyp[i-1].ef + 1, parthyp[i].sf - 1, uttnum, channelName, peak_amplitude);
	broadcast(content);
	content[0]='\0';
      };
      /* Parser's indices start from 1 */
      if ((n = countSpecials(newWords[i])) == 0) {
	sprintf(content,
		"(word \"%s\" :index %d :frame (%d %d) :uttnum %d :channel %s :direction input :peak %d)",
		newWords[i], index, parthyp[i].sf, parthyp[i].ef, uttnum, channelName, peak_amplitude);
      } else {
	sprintf(content, 
		"(word \"%s\" :index (%d %d) :frame (%d %d) :uttnum %d :channel %s :direction input :peak %d)",
		newWords[i], index, index+n+1, parthyp[i].sf, parthyp[i].ef, uttnum, channelName, peak_amplitude);
      }
      broadcast(content);
      index += 1 + n;
      /* And save the new word for next time */
      strcpy(oldWords[i], newWords[i]);
      /* Save the new frames for next time */
      oldStartFrames[i] = parthyp[i].sf;
      oldEndFrames[i] = parthyp[i].ef;
    }
    /* And save the number of new words for next time */
    numOld = numNew;
    for (i = 0; i < numOld; i++) {
      DEBUG2("oldWords[%d] = \"%s\"", i, oldWords[i]);
    }
  } else {
    DEBUG0("don't need to add words");
  }
  DEBUG0("done ----------------------------");
}

void
wordbufSetBufferLen(int len)
{
  bufnum = len;
}

void
wordbufReset(void)
{
  numOld = 0;
}

static int
countSpecials(char *s)
{
  int n = 0;
	
  /* Special tokens start with an angle bracket */
  if (*s == '<') {
    return 0;
  }
  /* Otherwise we have to count */
  while (*s) {
    if (ispunct(*s) || *s == '_') {
      n += 1;
    }
    s += 1;
  }
  return n;
}
