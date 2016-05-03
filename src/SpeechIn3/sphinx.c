/*
 * sphinx.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 26 Sep 2002
 * $Id: sphinx.c,v 1.7 2009/04/18 16:30:31 lgalescu Exp $
 *
 * Lucian Galescu, lgalescu@ihmc.us, 24 Aug 2004 -- modified for Sphinx3
 */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
/* Sphinx includes */
#include "prim_type.h"
#include "ad.h"
#include "cont_ad.h"
#include "cmd_ln.h"
/* TRIPS includes */
#include "live.h"
#include "sphinx.h"
#include "input.h"
#include "bcast.h"
#include "send.h"
#include "recv.h"
#include "utt.h"
#include "main.h"
#include "util/debug.h"
#include "util/error.h"

#define RESULT_UPDATE_PERIOD	4000	/* in samples; that's 1/4th of a sec */

/* Some globals used elsewhere */
RecognitionMode recognitionMode;
int doneSomeRecognition = FALSE;
int continuousModePaused = FALSE;

char* dynLM = NULL;
char* dynDic = NULL;

/* Audio device */
static ad_rec_t *ad;

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

/*      -       -       -       -       -       -       -       -       */

int
initSphinx(char *argsfile)
{
  live_initialize_decoder(argsfile);
    
  if ((ad = ad_open_sps(cmd_ln_int32("-samprate"))) == NULL) {
    ERROR0("failed to open audio device");
    return -1;
  }
    
  return 0;
}

int
shutdownSphinx(void)
{
  ad_close(ad);
  live_free_memory();
  return 0;
}

void pauseContinuousMode(void)	{ 
  if (!continuousModePaused) {
    continuousModePaused = TRUE; 
    broadcast("(stopped-listening :who sys)");
  }
}
void resumeContinuousMode(void) { 
  if (continuousModePaused) {
    continuousModePaused = FALSE; 
    broadcast("(started-listening :who sys)");
  }
}

void
setDynLMs(char* lmfile, char* dicfile)
{
  dynLM = lmfile;
  dynDic = dicfile;
}

void
resetDynLMs()
{
  dynLM = dynDic = NULL;
}

void
updateDynLMsMaybe()
{
  if (dynLM && dynDic) {
    sendTrafficLightMsg(RED);
    DEBUG2("dynLM=%s, dynDic=%s", dynLM, dynDic);
    live_update_decoder(dynDic, dynLM);
    resetDynLMs();
    sendDoneMsg(lastSetNewLMsRequest);
    sendTrafficLightMsg(GREEN);
  }

}

static void
dispatchMessages(int period)
{
  //#   define UI_SPEAKING_INTERVAL 5
  static int count = 0;
    
  /* Every so often, check the input for a message (maybe "stop") */
  //if (++count % UI_SPEAKING_INTERVAL == 0) {
  if (++count % period == 0) {
    if (input(SPEECH_INPUT_NO_HANG) < 0) {
      /* Error processing KQML input */
      exit(-2);
    }
    /* Otherwise maybe a message got dispatched... */
  }
}

/*      -       -       -       -       -       -       -       -       */
/*
 * PTT mode
 */
/* Are we still speaking (or really, should we stop processing?) */
static int speaking;
/*
 * startPTT() will run until someone calls stopPTT() to clear the speaking flag
 */
void
startPTT(void)
{
  int16 adbuf[4096];
  int32 k;
  int32 ns = 0;	/* #Samples read from audio in this utterance */
  int32 hwm = RESULT_UPDATE_PERIOD;	/* High Water Mark: to know when to report partial result */
  int32 recording;

  /* Begin utterance */
  recognitionMode = SPEECHIN_MODE_PTT;
  doneSomeRecognition = TRUE;
  uttStart();
  DEBUG1("started utt: %s", uttid);
  /* Start A/D recording for this utterance */
  if (ad_start_rec(ad) < 0) {
    ERROR0("ad_start_rec failed");
    exit(1);
  }
  recording = 1;
  speaking = 1;
    
  /* Send audio data to decoder until end of utterance */
  for (;;) {
    /*
     * Read audio data (NON-BLOCKING).  Use your favourite substitute here.
     * NOTE: In our implementation, ad_read returns -1 upon end of utterance.
     */
    DEBUG0("calling ad_read"); 
    if ((k = ad_read(ad, adbuf, 4096)) < 0)
      break;
    DEBUG1("ad_read returned %d samples", k);
        
    if (k == 0) {
      /* just in case A/D latency is a factor */
      sleep_msec(10);
    } else {
      /* Send whatever data was read above to decoder */
      live_utt_decode_block(adbuf, k, 0, &parthyp);
      ns += k;
            
      /* Time to report partial result? */
      if (ns > hwm) {
	uttUpdate();
	hwm = ns + RESULT_UPDATE_PERIOD;
      }
    }

    DEBUG0("checking for messages"); 
    dispatchMessages(10);
        
    /*
     * Check for end of utterance indication from user.
     * NOTE: Our way of finishing an utterance is to stop the A/D, but
     * continue to read A/D data in order to empty system audio buffers.
     * The ad_read function returns -1 when audio recording has been stopped
     * and no more data is available, exiting this for-loop (see above).
     * Other implementations can adopt a different approach; eg, exit the
     * loop right here if (! speaking()).
     */
    if (recording && !speaking) {
      DEBUG0("stopping ad");
      ad_stop_rec(ad);
      recording = 0;
    }
  }

  /* Utterance ended */
  uttStop();

  /* Finalize results */
  DEBUG0("finalizing results");
  live_utt_decode_block(adbuf, (k > 0) ? k : 0, 1, &parthyp);

  /* obtain and print result */
  uttDone();

  /* make LM updates if requested */
  updateDynLMsMaybe();

}

void
stopPTT(void)
{
  DEBUG0("setting speaking=0");
  speaking = 0;
  DEBUG0("done");
}

/*      -       -       -       -       -       -       -       -       */
/*
 * Continuous mode
 */
/*
 * startContinuous() will run until someone calls switchToPTT().
 */
void
startContinuous(void)
{
  int16 adbuf[4096];
  int32 k, ts;
  int32 ns = 0;	/* #Samples read from audio in this utterance */
  int32 hwm = RESULT_UPDATE_PERIOD;	/* High Water Mark: to know when to report partial result */
  cont_ad_t *cont;
    
  recognitionMode = SPEECHIN_MODE_CONTINUOUS;
  doneSomeRecognition = TRUE;
  /* Initialize continuous listening module */
  if ((cont = cont_ad_init(ad, ad_read)) == NULL) {
    ERROR0("cont_ad_init failed");
    exit(1);
  }
  if (ad_start_rec(ad) < 0) {
    ERROR0("ad_start_rec failed");
    exit(1);
  }
  if (cont_ad_calib(cont) < 0) {
    ERROR0("cont_ad_calib failed");
    exit(1);
  }
  broadcast("(started-listening :who sys)");
  /* Continue unless the mode is changed by a message handler */
  while (recognitionMode == SPEECHIN_MODE_CONTINUOUS) {
    DEBUG1("calling cont_ad_read, adbuf=0x%lx", adbuf);
    /* Await data for next utterance */
    while (recognitionMode == SPEECHIN_MODE_CONTINUOUS &&
	   (continuousModePaused ||
	    (k = cont_ad_read(cont, adbuf, 4096)) == 0)) {
      /* wait a little */
      if (continuousModePaused) {
	DEBUG0("paused...");
	/* take advantage to clear msg queue while we're waiting to resume */
	while (continuousModePaused && 
	       recognitionMode == SPEECHIN_MODE_CONTINUOUS) {
	  DEBUG0("checking for messages"); 
	  dispatchMessages(1);
	  sleep_msec(10); 
	}
	/*
	 * Flush any accumulated, unprocessed A/D data 
	 */
	ad_stop_rec(ad);
	while (ad_read(ad, adbuf, 4096) >= 0);
	cont_ad_reset(cont);
	/* Resume A/D recording for next utterance */
	if (ad_start_rec(ad) < 0) {
	  ERROR0("ad_start_rec failed");
	}
      }
      else {
	DEBUG0("silence...");
	sleep_msec(40); 
      }

      /* Might switch modes during silence */
      DEBUG0("checking for messages"); 
      dispatchMessages(1);

      /* make LM updates if requested */
      updateDynLMsMaybe();
    }

    /* If loop broke because of mode switch, don't start a new utt */
    if (recognitionMode != SPEECHIN_MODE_CONTINUOUS) {
      DEBUG0("mode switched (during silence)");
      ad_stop_rec(ad); 
      break;
    }

    /* input error handling: just die */
    if (k < 0) {
      ERROR0("cont_ad_read failed");
      exit(1);
    }
    DEBUG1("cont_ad_read returned %d samples", k);
        
    /*
     * Non-zero amount of data received; start recognition of new utterance.
     * automatic generation of utterance-id.
     */
    uttStart();
    DEBUG1("started utt: %s", uttid);

    /* send data to decoder */
    live_utt_decode_block(adbuf, k, 0, &parthyp);
    ns += k;

    /* Time to report partial result? */
    if (ns > hwm) {
      uttUpdate();
      hwm = ns + RESULT_UPDATE_PERIOD;
    }
        
    /* Note timestamp for this first block of data */
    ts = cont->read_ts;
        
    /* Decode utterance until end of speech (marked by a "long" silence, >1sec) 
       or interruption */
    while ((recognitionMode == SPEECHIN_MODE_CONTINUOUS) &&
	   ! continuousModePaused ) {
      /* Read non-silence audio data, if any, from continuous listening module */
      if ((k = cont_ad_read(cont, adbuf, 4096)) < 0) {
	ERROR0("cont_ad_read failed");
      }
      if (k == 0) {
	/*
	 * No speech data available; check current timestamp with most recent
	 * speech to see if more than 1 sec elapsed.  If so, end of utterance.
	 */
	if ((cont->read_ts - ts) > DEFAULT_SAMPLES_PER_SEC) {
	  break;
	}
	/* If no work to be done, sleep a bit */
	sleep_msec(20);
      } else {
	/* New speech data received; note current timestamp */
	ts = cont->read_ts;
                
	/* send data to decoder */
	live_utt_decode_block(adbuf, k, 0, &parthyp);
	ns += k;

	/* Time to report partial result? */
	if (ns > hwm) {
	  uttUpdate();
	  hwm = ns + RESULT_UPDATE_PERIOD;
	}
      }
            
      /* Check for mode switch or other msgs (tested at top of loop) */
      DEBUG0("checking for messages"); 
      dispatchMessages(5);
    }
        
    /* Utterance ended */
    uttStop();

    /*
     * Flush any accumulated, unprocessed A/D data and stop
     * listening until current utterance completely decoded
     */
    ad_stop_rec(ad);
    while (ad_read(ad, adbuf, 4096) >= 0);
    cont_ad_reset(cont);
        
    /* If we broke out of previous loop, then abandon this utt */
    if (recognitionMode != SPEECHIN_MODE_CONTINUOUS) {
      DEBUG0("mode switched (during utt)");
      break;
    }

    /* note if interrupted, since there may be garbage at the end of this hyp */
    uttSetInterrupted(continuousModePaused);

    /* Finish decoding */
    live_utt_decode_block(adbuf, k, 1, &parthyp);

    /* obtain and print result */
    uttDone();

    /* make LM updates if requested */
    updateDynLMsMaybe();

    /* Resume A/D recording for next utterance */
    if (ad_start_rec(ad) < 0) {
      ERROR0("ad_start_rec failed");
    }
  }
 
  cont_ad_close(cont);
  broadcast("(stopped-listening :who sys)");
}

/*      -       -       -       -       -       -       -       -       */
/*
 * Mode-switching
 */
void switchToPTT(void) {
  DEBUG1("mode=%d", recognitionMode);
  if (recognitionMode == SPEECHIN_MODE_CONTINUOUS) {
    recognitionMode = SPEECHIN_MODE_PTT;
    DEBUG1("new mode=%d", recognitionMode);
    /* Nothing else to do: wait for START message */
  }
  DEBUG0("done");
}

void switchToContinuous(void) {
  DEBUG1("mode=%d", recognitionMode);
  if (recognitionMode == SPEECHIN_MODE_PTT) {
    recognitionMode = SPEECHIN_MODE_CONTINUOUS;
    DEBUG1("new mode=%d", recognitionMode);
    if (doneSomeRecognition || !waitForStartConversation) {
      startContinuous();
    }
  }
}


