/* ====================================================================
 * Copyright (c) 1999-2001 Carnegie Mellon University.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer. 
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * This work was supported in part by funding from the Defense Advanced 
 * Research Projects Agency and the National Science Foundation of the 
 * United States of America, and the CMU Sphinx Speech Consortium.
 *
 * THIS SOFTWARE IS PROVIDED BY CARNEGIE MELLON UNIVERSITY ``AS IS'' AND 
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, 
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL CARNEGIE MELLON UNIVERSITY
 * NOR ITS EMPLOYEES BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ====================================================================
 *
 */
/********************************************************************
 * Example program to show usage of the live mode routines
 * The decoder is initialized with live_initialize_decoder()
 * Blocks of samples are decoded by live_utt_decode_block()
 * To compile an excutable compile using
 * $(CC) -I. -Isrc -Llibutil/linux -Lsrc/linux main_live_example.c -lutil -ldecoder -lm
 * from the current directory 
 * Note the include directories (-I*) and the library directories (-L*)
 *
 ********************************************************************/
/*************************************************
 * History
 *************************************************
 *
 * 30-Aug-2004	Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Added update_result(). Cleaned up.
 *
 * 27-Aug-2004  Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *		Added sleeping a little when recording and no A/D read to avoid
 *		potential latency issues.
 * 
 * 19-Aug-2004  Lucian Galescu <lgalescu@ihmc.us> at IHMC
 *              First version apparently working; based on main_live_example.c 
 *              and Sphinx2's tty-ptt.c.
 */
#include <libutil/libutil.h>
#include <stdio.h>
#include <string.h>

#ifdef WIN32
#include <time.h>
#else
#include <sys/types.h>
#include <sys/time.h>
#endif

#include "live.h"
#include "cmd_ln_args.h"
#include "ad.h"

#define LISTENTIME	3.0
#define MAX_RECORD      (LISTENTIME*DEFAULT_SAMPLES_PER_SEC)
#define RESULT_UPDATE_PERIOD	4000	/* in samples; that's 1/4th of a sec */

static ad_rec_t *ad;

/* Determine if the user has indicated end of utterance (keyboard hit at end of utt) */
static int32 speaking (int32 ns)
{
  /*#ifdef WIN32*/
#if 0
    return (ns > MAX_RECORD) ? 0 : 1;
#else
    /* ------------------- Unix ------------------ */
    /* Check for a keyboard hit, BUT NON-BLOCKING */
    fd_set readfds;
    struct timeval zero_tmo;
    int32 status;
    char line[1024];
    
    FD_ZERO(&readfds);
    FD_SET(0 /* stdin */, &readfds);
    zero_tmo.tv_sec = 0;
    zero_tmo.tv_usec = 0;
    
    status = (select(1, &readfds, NULL, NULL, &zero_tmo) <= 0);
    if (! status) { 
        /* Assume user typed something at the terminal to stop speaking */
        fgets (line, sizeof(line), stdin);

	printf ("%s [%d]\n", status ? "speaking..." : "done speaking.", ns);
    }
    return (status);
#endif
}

static void ui_ready ( void )
{
#ifdef WIN32
    printf ("\nSystem will listen for ~ %.1f sec of speech\n", LISTENTIME);
    printf ("Hit <cr> before speaking: ");
#else
    printf ("\nHit <cr> BEFORE and AFTER speaking: ");
#endif
    fflush (stdout);
}

/* Sleep for specified msec */
static void sleep_msec (int32 ms)
{
#ifdef WIN32
    Sleep(ms);
#else
#ifdef undef /* gf */
    /* ------------------- Unix ------------------ */
    struct timeval tmo;
    
    tmo.tv_sec = 0;
    tmo.tv_usec = ms*1000;
    
    select(0, NULL, NULL, NULL, &tmo);
#else
    /* Modern! */
    usleep(ms*1000);
#endif
#endif
}

/* Function for reporting partial recognition result */
static void
update_result()
{
  int32 j;

  for (j=0; j < parthyplen; j++) 
    printf(" %s",parthyp[j].word);
  printf("\n");
}

/* Main utterance processing loop: decode each utt */
static void utterance_loop()
{
  char line[1024];
  int16 adbuf[4096];
  int32 k;
  int32 ns;	/* #Samples read from audio in this utterance */
  int32 hwm;	/* High Water Mark: to know when to report partial result */
  int32 recording;

  int32 uttno = 0;
  char uttid[64];

  for (;;)
    {
      ui_ready ();

      fgets (line, sizeof(line), stdin);
      if ((line[0] == 'q') || (line[0] == 'Q'))
	return;

      E_INFO("ready...");
      uttno++;
      sprintf(uttid, "utt%05d", uttno);
      live_utt_set_uttid(uttid);

      ad_start_rec(ad);	/* Start A/D recording for this utterance */
      recording = 1;

      ns = 0;
      hwm = RESULT_UPDATE_PERIOD; /* Next partial result reported after this many samples */
	
      /* Send audio data to decoder until end of utterance */
      for (;;) {
        /*
         * Read audio data (NON-BLOCKING).  Use your favourite substitute here.
         * NOTE: In our implementation, ad_read returns -1 upon end of utterance.
         */
        if ((k = ad_read (ad, adbuf, 4096)) < 0) {
          break;
	}
        E_INFO("ad read: %d\n", k);

        if (k == 0) {
            /* just in case A/D latency is a factor */
            sleep_msec(10);
        } else {
	  /* Send whatever data was read above to decoder */
	  live_utt_decode_block(adbuf, k, 0, &parthyp);
	  ns += k;

	  /* Time to report partial result? */
	  if (ns > hwm) {
	    E_INFO("PARTIAL HYP [%d]:", ns);
	    update_result();
	    hwm = ns + RESULT_UPDATE_PERIOD;
	  }
	}
	/*
	 * Check for end of utterance indication from user.
	 * NOTE: Our way of finishing an utterance is to stop the A/D, but
	 * continue to read A/D data in order to empty system audio buffers.
	 * The ad_read function returns -1 when audio recording has been stopped
	 * and no more data is available, exiting this for-loop (see above).
	 * Other implementations can adopt a different approach; eg, exit the
	 * loop right here if (! speaking()).
	 */
	if (recording && (! speaking(ns))) {
	  ad_stop_rec(ad);
	  E_INFO("A/D Stopped\n");
	  recording = 0;
	}
      }

      printf("%d total samples read\n", ns);

      live_utt_decode_block(adbuf, (k > 0) ? k : 0, 1, &parthyp);
      E_INFO("\n\nFINAL HYP:");
      update_result();
      live_utt_summary();
    }
}


int main (int argc, char *argv[])
{
    char   *argsfile;

    if (argc != 2) {
      argsfile = NULL;
	/* lgalescu: what's with this???
	parse_args_file(argsfile);
	*/
      E_FATAL("\nUSAGE: %s <argsfile>\n", argv[0]);
    }
    argsfile = argv[1];
    live_initialize_decoder(argsfile);

    /*ARCHAN*/
    {
      int samprate = DEFAULT_SAMPLES_PER_SEC;

      samprate = cmd_ln_int32 ("-samprate");
      if ((ad = ad_open_sps(samprate)) == NULL)
	E_FATAL("ad_open_sps failed\n");

      utterance_loop();
    }

    ad_close(ad);
    live_free_memory();
    
    exit(0);
}

