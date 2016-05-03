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

/*
 * ad.c -- Wraps a "sphinx-II standard" audio interface around the basic audio
 * 		utilities.
 *
 * HISTORY
 * 
 * 30-Aug-04	L Galescu (lgalescu@ihmc.us) at IHMC.
 *		Fixed omission: setting of handle->bps. 
 *		Added more debugging msgs.
 *
 * 20-Jul-04	L Galescu (lgalescu@ihmc.us) at IHMC.
 *              Adapted to Sphinx3. Cleaned up a bit.
 * 
 * 12-Nov-03    L Galescu (lgalescu@ihmc.us) at IHMC.
 *              Modified for MAC OSX using PABLIO (on top of PortAudio).
 *
 * 11-Jun-96	M K Ravishankar (rkm@cs.cmu.edu) at Carnegie Mellon University.
 * 		Modified to conform to new A/D API.
 * 
 * 12-May-96	M K Ravishankar (rkm@cs.cmu.edu) at Carnegie Mellon University.
 * 		Dummy template created.
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "s3types.h"
#include "ad.h"

#include "pablio.h"
/*#include "pa_trace.h"*/

#define NUM_CHANNELS    (1)
#define SAMPLE_TYPE	paInt16

ad_rec_t *ad_open_sps (int32 samples_per_sec)
{
  PaError err;

  ad_rec_t *handle;

  if ((handle = (ad_rec_t *) calloc (1, sizeof(ad_rec_t))) == NULL) {
      fprintf(stderr, "calloc(%ld) failed\n", sizeof(ad_rec_t));
      abort();
  }

  handle->sps = samples_per_sec;
  handle->bps = AD_SAMPLE_SIZE;

  /* Open simplified blocking I/O layer on top of PortAudio. */
  err = OpenAudioStream( &(handle->stream), handle->sps, SAMPLE_TYPE,
			 (PABLIO_READ | PABLIO_MONO) );

  if( err != paNoError ) {
    free(handle);
    return NULL;
  }

  handle->recording = 0;

  /*
   * Initialize recording; else there'll be a bunch of 0 frames at the
   * begining that will throw off the spectral analysis
   * hmm... it looks like this can happen any time, not just at the beginning...
   ad_start_rec(handle);
   sleep(1);
   ad_stop_rec(handle);
   */

  return handle;
}

ad_rec_t *ad_open ( void )
{
  return ad_open_sps(DEFAULT_SAMPLES_PER_SEC);
}

int32 ad_start_rec (ad_rec_t *handle)
{
  if (handle->recording)
    return AD_ERR_GEN;

  handle->recording = 1;

  if (StartAudioStream( handle->stream ) < 0)
    return AD_ERR_GEN;

#ifdef AD_PA_DEBUG
  fprintf(stderr, "A/D Recognition started\n"); 
#endif

  return 0;
}

int32 ad_stop_rec (ad_rec_t *handle)
{
  if (! handle->recording)
    return AD_ERR_GEN;

  if (StopAudioStream( handle->stream ) < 0)
    return AD_ERR_GEN;

  handle->recording = 0;

#ifdef AD_PA_DEBUG
  fprintf(stderr, "A/D Recognition stopped\n");
  fprintf(stderr, ">>>ad_stop_rec>>>ring buffer: %d[r:%d;w:%d]\n",
	  handle->stream->inFIFO.bufferSize/handle->bps,
	  handle->stream->inFIFO.readIndex/handle->bps,
	  handle->stream->inFIFO.writeIndex/handle->bps);
#endif

  return 0;
}

int32 ad_read (ad_rec_t *handle, int16 *buf, int32 max)
{
  int32 length;

  /* 2004/08/29 L Galescu <lgalescu@ihmc.us> 
   * ReadAudioStream() returns number of samples (frames in pa parlance) read! 
   */
  length = ReadAudioStream(handle->stream, buf, max);

#ifdef AD_PA_DEBUG
  fprintf(stderr, "recording: %d; read: %d (requested: %d)\n", 
	  handle->recording, length, max); 
  fprintf(stderr, ">>>ad_read>>>ring buffer: %d[r:%d;w:%d]\n",
	  handle->stream->inFIFO.bufferSize/handle->bps,
	  handle->stream->inFIFO.readIndex/handle->bps,
	  handle->stream->inFIFO.writeIndex/handle->bps);
#endif

  if ((length == 0) && (! handle->recording))
    return AD_EOF;

#ifdef UNDEF_LG
  {
    int32 offset;

    /* hack! get rid of those pesky nulls at the beginning! */
    for (offset = 0; (offset < length) && (buf[offset] == 0); offset++);

    if (offset > 0) {
	fprintf(stderr, "ad_read(): %d nulls deleted (out of %d samples)\n", offset, length);
      bcopy(buf+offset, buf, length-offset);
      length -= offset;
    }
  }

  fprintf(stderr, "A/D Read: %d\n", length);
#endif

  return length;
}

int32 ad_close (ad_rec_t *handle)
{
  if (handle->recording)
    if (ad_stop_rec(handle) < 0)
	return AD_ERR_GEN;

  if (CloseAudioStream( handle->stream ) < 0)
    return AD_ERR_GEN;

  free(handle);

#ifdef AD_PA_DEBUG
  fprintf(stderr, "A/D Closed\n"); 
#endif

  return 0;
}
