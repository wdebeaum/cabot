/*
 * utt.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 31 Jan 1996
 * $Id: utt.c,v 1.3 2009/04/18 16:30:31 lgalescu Exp $
 */
#include <stdio.h>
#include <sys/param.h>
/* Sphinx includes */
#include "live.h"
#include "cmd_ln_args.h"
/* TRIPS includes */
#include "util/memory.h"
#include "util/error.h"
#include "util/debug.h"
#include "utt.h"
#include "bcast.h"
#include "wordbuf.h"
#include "send.h"
#include "analyze.h"
#include "main.h"
#include "log.h"

/*
 * Functions defined here:
 */
void uttId(void);
void uttSetInterrupted(int);
void uttStart(void);
void uttStop(void);
void uttUpdate(void);
void uttDone(void);
void uttReset(void);
void uttChdir(char *dir);

/*
 * Data defined here:
 */
int uttnum = 0;
char uttid[64];
char uttresult[MAX_UTT_LEN];
int uttInterrupted;

/*	-	-	-	-	-	-	-	-	*/

void
uttId()
{
  sprintf(uttid, "%s%05d", fileBasename, uttnum);
  live_utt_set_uttid(uttid);
}

void
uttSetInterrupted(int isInterrupted) 
{
  uttInterrupted = isInterrupted;
}

void
uttStart(void)
{
  char content[256];
	
  DEBUG0("");
  uttnum += 1;
  DEBUG1("uttnum=%d", uttnum);
  sprintf(content, "(started-speaking :channel %s :direction input :uttnum %d)", channelName, uttnum);
  broadcast(content);
  uttId();
  wordbufReset();
  uttInterrupted = FALSE;
  DEBUG0("done");
}


void
uttStop(void)
{
  char content[256];
	
  DEBUG0("");
  sprintf(content, "(stopped-speaking :channel %s :direction input :uttnum %d)", channelName, uttnum);
  broadcast(content);
  DEBUG0("done");
}

/* lgalescu <lgalescu@ihmc.us> at IHMC
 * Adapted from Sphinx2:search.c::search_hyp_to_str()
 *
 * Note: This function is called before any update is sent out.
 */
void 
uttResult(void)
{
  int i, k, l;
	
  uttresult[0] = '\0';
	
  /* check if last "word" is "</s>" -- this is the only marker S3 sends out;
   * if so, remove it -- we only care about true words, not markers */
  if (strcmp(parthyp[parthyplen-1].word, "</s>") == 0) {
    parthyplen--;
  }

  k = 0;
  DEBUG1("parthyplen=%d",parthyplen);
  for (i = 0; i < parthyplen; i++) {
    DEBUG2("parthyp[%d]=%s",i,parthyp[i].word);
    l = strlen(parthyp[i].word);
    DEBUG3("k=%d; l=%d; MAX_UTT_LEN=%d",k,l,MAX_UTT_LEN);
    if (k+l+6 > MAX_UTT_LEN) {
      ERROR2("%s(%d): **ERROR** Increase uttresult[] size\n", __FILE__, __LINE__);
    }
    strcpy (uttresult+k, parthyp[i].word);
    k += l;
    uttresult[k] = ' ';
    k++;
    uttresult[k] = '\0';
  }
	
  DEBUG1("result=\"%s\"", uttresult);
}

void
uttUpdate(void)
{
  uttResult();
  DEBUG1("result=\"%s\"", uttresult);
  wordbufOutput(FALSE);
  E_INFO("update; segment peak %d\n",peak_amplitude);
  peak_amplitude = 0; /* LG 2008/06/13 */
  DEBUG0("done");
}

void
uttDone(void)
{
  char str[MAXPATHLEN];
  int problem;
  char *problem_str;

  uttResult();
  DEBUG1("result=\"%s\"", uttresult);
	
  /* Send final words for this utt */
  wordbufOutput(TRUE);
	
  /* Ok, say we're done with the utt */
  /* gf: 26 May 1998: Include final hypothesis in END message */
  /* gf: 13 Jul 1998: Use analyze.c routine to check signal levels */
  if (logdir != NULL) {
    sprintf(str, "%s/%s.raw", logdir, uttid);
  } else {
    sprintf(str, "%s/%s.raw", cmd_ln_str("-outrawdir"), uttid);
  }
  problem = analyze(str);
  if (problem > 0) {
    problem_str = " :signal too-loud";
  } else if (problem < 0) {
    problem_str = " :signal too-soft";
  } else {
    problem_str = "";
  }
  sprintf(str, "(utterance :channel %s :direction input :mode speech :uttnum %d :text \"%s\"%s%s)",
	  channelName, uttnum, uttresult ? uttresult : "<EMPTY>", problem_str, uttInterrupted ? " :interrupted" : "");
  broadcast(str);
  DEBUG0("done");
}

void
uttReset(void)
{
  DEBUG0("resetting uttnum to 0");
  uttnum = 0;
  DEBUG0("done");
}
