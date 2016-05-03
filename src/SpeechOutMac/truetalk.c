/*
 * truetalk.c: Routines that interface with the TrueTalk API
 *
 * George Ferguson, ferguson@cs.rochester.edu, 10 Jan 1996
 * Time-stamp: <Wed May 13 13:44:07 EDT 1998 ferguson>
 *
 * Note that we use the 8000 Hz sample rate here, since using the 12k
 * one results in nasty artifacts when it gets rate-converted in the audio
 * server. Sigh.
 *
 * gf: 13 May 1998: Updated for TrueTalk r2.0 API.
 *
 */
#include <stdio.h>
#include <TT_funcs.h>
#include "util/error.h"
#include "util/debug.h"
#include "truetalk.h"
#include "send.h"
#include "audio.h"

/*
 * Functions defined here:
 */
int truetalkInit(char *host);
void truetalkSend(char *buf, KQMLPerformative *perf);
void truetalkClose(void);
static int ttCB(char *buf, int count, int rec_size, struct TtsIndex *index);

/*
 * Data defined here: (more below)
 */
static TTclient myclient;
static TTserver server;
static KQMLPerformative *currentPerf;

/*	-	-	-	-	-	-	-	-	*/

int
truetalkInit(char *host)
{
    static unsigned int ports[2] = {0, 0};
    int who, where, encoding, rate;
    int retries;

    DEBUG0("initializing TrueTalk");
    /* Initialize the TrueTalk API */
    EinitTtbase(NULL);
    /* Find and connect to the server */
    if ((server = TT_BindServer(host, ports)) == NULL) {
	ERROR1("couldn't bind to TT server on %s",
	       (host ? host : "<local host>"));
	return -1;
    }
    /* Try repeatedly to connect to TrueTalk server */
    retries = 100;
    while (retries) {
	/* Try to connect */
	if ((myclient = TT_Open(server)) != NULL) {
	    /* Yes! Carry on */
	    break;
	}
	/* Really didn't connect? */
	if (--retries <= 0) {
	    ERROR2("couldn't connect to TT server on %s: %s",
		   (host ? host : "<local host>"), TT_DecodeLastError());
	    return -1;
	}
	/* Maybe it's still coming up, let's wait and see */
	DEBUG2("couldn't connect to TT server on %s: %s (will retry)",
	       (host ? host : "<local host>"), TT_DecodeLastError());
	sleep(1);
    }
    /* Tell the server to exit after we're done */
    TT_ExitOnClose(myclient);
    /* Tell TrueTalk we want to handle the output ourselves */
    who = OUTPUT_CLIENT;
    where = OUTPUT_FUNC;
    encoding = ENCODING_PCM;
    rate = 8000;
    DEBUG0("setting TrueTalk output parameters");
    if (TT_SetOutput(myclient, who, where, encoding, rate, ttCB) == 0) {
	ERROR1("couldn't set TT output: %s\n", TT_DecodeLastError());
	truetalkClose();
	return -1;
    }
    /* Done */
    DEBUG0("done");
    return 0;
}

void
truetalkSend(char *buf, KQMLPerformative *perf)
{
    unsigned long marker;

    DEBUG1("sending \"%s\"", buf);
    /* Save this performative for use by callbacks */
    currentPerf = perf;
    /* Send the line to TrueTalk (will callback ttCB() with output) */
    marker = TT_SendText(myclient, buf, strlen(buf));
    if (!marker) {
	ERROR1("TT_SendText failed: %s\n", TT_DecodeLastError());
    }
    /* Let server finish with this text before sending new stuff */
    DEBUG0("waiting for TrueTalk to finish");
    if (!TT_Sync(myclient, marker)) {
	ERROR1("TT_Sync failed: %s\n", TT_DecodeLastError());
    }
    /* Sync with server */
    DEBUG0("syncing server");
    audioSync();
    /* Send done reply */
    if (perf != NULL) {
	DEBUG0("replying");
	sendDoneReply(perf);
    }
    /* Done */
    DEBUG0("done");
}

void
truetalkClose(void)
{
    DEBUG0("closing connection to TrueTalk");
    TT_Close(myclient, server);
    DEBUG0("done");
}

/*	-	-	-	-	-	-	-	-	*/
#ifdef no_rate_convert
/*
 * This function is called back whenever there is some audio generated.
 * The `buf' is an array of signed shorts (16 bit PCM) coming at 8 or 12k
 * samp/sec.
 */
static int
ttCB(char *buf, int count, int rec_size, struct TtsIndex *index)
{
    DEBUG1("recvd %d bytes", count);
    audioWrite(buf, count);
    /* Return 1 for success (to TrueTalk) */
    DEBUG0("done");
    return 1;
}

#else /* rate_convert */

/*
 * This function is called back whenever there is some audio generated.
 * We read the `buf' as an array of signed shorts (16 bit PCM) and
 * interpolate to generate 16k output from the 8k input. These output
 * samples are sent to the audio device as the `audiobuf' buffer fills.
 * NOTE: The size of this buffer reflects a tradeoff between smooth
 *	 playback and the latency before and between chunks.
 */
#define AUDIOBUFSIZE 8192		/* words (x2 for bytes) */
static short audiobuf[AUDIOBUFSIZE];
static short lastout = 0;
static int nout = 0;

static int
ttCB(char *buf, int count, int rec_size, struct TtsIndex *index)
{
    short *samples = (short*)buf;
    int nsamples = count / 2;		/* Assumed to be even... */

    DEBUG1("recvd %d bytes", count);
    /* For each sample (signed 16-bit word)... */
    while (nsamples-- > 0) {
	/* Interpolate linearly between last sample and this one */
	audiobuf[nout++] = (short)(((long)lastout + (long)*samples) / 2);
	/* Output the original input sample and save it as the last sample */
	audiobuf[nout++] = lastout = *samples++;
	/* If the output buffer is full, send it to the audio device */
	if (nout >= AUDIOBUFSIZE-1) {
	    DEBUG1("writing %d bytes", nout*sizeof(short));
	    audioWrite((char*)audiobuf, nout*sizeof(short));
	    /* And reset output buffer */
	    nout = 0;
	}
    }
    /* If there's something left, send it now. This works when the audio
     * routines are looking after buffering. Otherwise we might want to
     * wait and send one final chunk before calling audioSync().
     */
    if (nout > 0) {
	DEBUG1("writing %d more bytes", nout*sizeof(short));
	audioWrite((char*)audiobuf, nout*sizeof(short));
	/* And reset output buffer */
	nout = 0;
    }
    /* Return 1 for success (to TrueTalk) */
    DEBUG0("done");
    return 1;
}
#endif /* rate_convert */
