/*
 * $Id: pablio.c,v 1.5 2011/09/30 18:21:52 lgalescu Exp $
 * pablio.c
 * Portable Audio Blocking Input/Output utility.
 *
 * Author: Phil Burk, http://www.softsynth.com
 *
 * This program uses the PortAudio Portable Audio Library.
 * For more information see: http://www.portaudio.com
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "portaudio.h"
#include "pa_ringbuffer.h"
#include "pablio.h"
#include <string.h>

/************************************************************************/
/******** Constants *****************************************************/
/************************************************************************/

#define FRAMES_PER_BUFFER    (256)

/************************************************************************/
/******** Prototypes ****************************************************/
/************************************************************************/

static int blockingIOCallback( const void *inputBuffer, void *outputBuffer,
                               unsigned long framesPerBuffer,
			       const PaStreamCallbackTimeInfo* timeInfo,
			       PaStreamCallbackFlags statusFlags,
			       void *userData );
static PaError PABLIO_InitFIFO( PaUtilRingBuffer *rbuf, long numFrames, long bytesPerFrame );
static PaError PABLIO_TermFIFO( PaUtilRingBuffer *rbuf );

/************************************************************************/
/******** Functions *****************************************************/
/************************************************************************/

/* Called from PortAudio.
 * Read and write data only if there is room in FIFOs.
 */
static int blockingIOCallback( const void *inputBuffer, void *outputBuffer,
                               unsigned long framesPerBuffer,
                               const PaStreamCallbackTimeInfo* timeInfo,
			       PaStreamCallbackFlags statusFlags, 
			       void *userData )
{
    PABLIO_Stream *data = (PABLIO_Stream*)userData;
    /* LGalescu 20110929 -- not needed anymore; see pablio.h
    long numBytes = data->bytesPerFrame * framesPerBuffer;
    */

    /* This may get called with NULL inputBuffer during initial setup. */
    if( inputBuffer != NULL )
    {
        PaUtil_WriteRingBuffer( &data->inFIFO, inputBuffer, framesPerBuffer);
    }
    if( outputBuffer != NULL )
    {
        int i;
        int numRead = PaUtil_ReadRingBuffer( &data->outFIFO, outputBuffer, framesPerBuffer );
        /* Zero out remainder of buffer if we run out of data. */
        for( i=numRead; i<framesPerBuffer; i++ )
        {
	  /* LGalescu 20110929 -- updated */
	  int size = data->outFIFO.elementSizeBytes;
	  memset(((char *)outputBuffer) + i*size, 0, size);
        }
    }

    return 0;
}

/* Allocate buffer. */
static PaError PABLIO_InitFIFO( PaUtilRingBuffer *rbuf, long numFrames, long bytesPerFrame )
{
    long numBytes = numFrames * bytesPerFrame;
    char *buffer = (char *) malloc( numBytes );
    if( buffer == NULL ) return paInsufficientMemory;
    memset( buffer, 0, numBytes );
    return (PaError) PaUtil_InitializeRingBuffer( rbuf, bytesPerFrame, numFrames, buffer );
}

/* Free buffer. */
static PaError PABLIO_TermFIFO( PaUtilRingBuffer *rbuf )
{
    if( rbuf->buffer ) free( rbuf->buffer );
    rbuf->buffer = NULL;
    return paNoError;
}

/************************************************************
 * Write data to ring buffer.
 * Will not return until all the data has been written.
 * LGalescu 20110929 -- updated this to conform to new ringbuffer 
 * implementation, which is in terms of elements (aka frames) rather than 
 * bytes. 
 */
long WriteAudioStream( PABLIO_Stream *aStream, void *data, long numFrames )
{
    long framesToWrite = numFrames;
    char *p = (char *) data;
    while( numFrames > 0 )
    {
        long framesWritten;
        framesWritten = PaUtil_WriteRingBuffer( &aStream->outFIFO, p, numFrames );
        framesToWrite -= framesWritten;
        p += (framesWritten * aStream->outFIFO.elementSizeBytes);
        if( framesToWrite > 0) Pa_Sleep(10);
    }
    return numFrames;
}

/************************************************************
 * Read data from ring buffer.
 * Will not return until all the data has been read.
 * LGalescu 20110929 -- updated this to conform to new ringbuffer 
 * implementation, which is in terms of elements (aka frames, aka samples in 
 * Sphinx) rather than bytes. 
 * NB: i think i will eventually get rid of pablio altogether and use the 
 * portaudio APIs directly. so this should be seen as a temporary hack.
 */
long ReadAudioStream( PABLIO_Stream *aStream, void *data, long numFrames )
{
    long framesToRead = numFrames;
    char *p = (char *) data;
    while( framesToRead > 0 )
    {
        long framesRead;
        framesRead = PaUtil_ReadRingBuffer( &aStream->inFIFO, p, framesToRead );

        /* <lgalescu> what if we don't have anything else coming? */
        if (framesRead <= 0)
          return (numFrames - framesToRead);

        framesToRead -= framesRead;
        p += (framesRead * aStream->inFIFO.elementSizeBytes);
        if( framesToRead > 0) Pa_Sleep(10);
    }
    return numFrames;
}

/************************************************************
 * Return the number of frames that could be written to the stream without
 * having to wait.
 */

/* LGalescu 20110929 -- looks like this function is not used anywhere
long GetAudioStreamWriteable( PABLIO_Stream *aStream )
{


    int bytesEmpty = PaUtil_GetRingBufferWriteAvailable( &aStream->outFIFO );
    return bytesEmpty / aStream->bytesPerFrame;
}
*/
/************************************************************
 * Return the number of frames that are available to be read from the
 * stream without having to wait.
 */
/* LGalescu 20110929 -- looks like this function is not used anywhere
long GetAudioStreamReadable( PABLIO_Stream *aStream )
{
    int bytesFull = PaUtil_GetRingBufferReadAvailable( &aStream->inFIFO );
    return bytesFull / aStream->bytesPerFrame;
}
*/
/************************************************************/
static unsigned long RoundUpToNextPowerOf2( unsigned long n )
{
    long numBits = 0;
    if( ((n-1) & n) == 0) return n; /* Already Power of two. */
    while( n > 0 )
    {
        n= n>>1;
        numBits++;
    }
    return (1<<numBits);
}

/************************************************************
 * Opens a PortAudio stream with default characteristics.
 * Allocates PABLIO_Stream structure.
 *
 * flags parameter can be an ORed combination of:
 *    PABLIO_READ, PABLIO_WRITE, or PABLIO_READ_WRITE,
 *    and either PABLIO_MONO or PABLIO_STEREO
 */
PaError OpenAudioStream( PABLIO_Stream **rwblPtr, double sampleRate,
                         PaSampleFormat format, long flags )
{
    long   bytesPerSample;
    long   samplesPerFrame;
    long   bytesPerFrame;
    long   doRead = 0;
    long   doWrite = 0;
    PaError err;
    PABLIO_Stream *aStream;
    /* long   minNumBuffers; */
    long   numFrames;

    /* Allocate PABLIO_Stream structure for caller. */
    aStream = (PABLIO_Stream *) malloc( sizeof(PABLIO_Stream) );
    if( aStream == NULL ) return paInsufficientMemory;
    memset( aStream, 0, sizeof(PABLIO_Stream) );

    /* Determine size of a sample. */
    bytesPerSample = Pa_GetSampleSize( format );
    if( bytesPerSample < 0 )
    {
        err = (PaError) bytesPerSample;
        goto error;
    }
    /* LGalescu 20110929 -- see pablio.h */
    samplesPerFrame = ((flags&PABLIO_MONO) != 0) ? 1 : 2;
    bytesPerFrame = bytesPerSample * samplesPerFrame;

    /* Initialize PortAudio  */
    err = Pa_Initialize();
    if( err != paNoError ) goto error;

    /* Warning: numFrames must be larger than amount of data processed per interrupt
     *    inside PA to prevent glitches. Just to be safe, adjust size upwards.
     */
    /* L Galescu 20070404 - modified to hold 10' of speech */
    numFrames = bytesPerSample * sampleRate * 10;
    numFrames = RoundUpToNextPowerOf2( numFrames );

    /* Initialize Ring Buffers */
    doRead = ((flags & PABLIO_READ) != 0);
    doWrite = ((flags & PABLIO_WRITE) != 0);
    if(doRead)
    {
        err = PABLIO_InitFIFO( &aStream->inFIFO, numFrames, bytesPerFrame );
        if( err != paNoError ) goto error;
    }
    if(doWrite)
    {
        err = PABLIO_InitFIFO( &aStream->outFIFO, numFrames, bytesPerFrame );
        if( err != paNoError ) goto error;

        /* Make Write FIFO appear full initially. */
        long numFramesAvailable;
        numFramesAvailable = PaUtil_GetRingBufferWriteAvailable( &aStream->outFIFO );
        PaUtil_AdvanceRingBufferWriteIndex( &aStream->outFIFO, numFramesAvailable );
    }

    /* Open a PortAudio stream that we will use to communicate with the underlying
     * audio drivers. */
    err = Pa_OpenDefaultStream(
              &aStream->stream,
              (doRead ? 1 : 0),
              (doWrite ? 1 : 0 ),
              format,
              sampleRate,
              FRAMES_PER_BUFFER,
              blockingIOCallback,
              aStream );
    if( err != paNoError ) goto error;

    /* L Galescu 2007/04/05 we don't start the stream here */
    /*
    err = Pa_StartStream( aStream->stream );
    if( err != paNoError ) goto error;
    */

    *rwblPtr = aStream;
    return paNoError;

error:
    CloseAudioStream( aStream );
    *rwblPtr = NULL;
    return err;
}

/************************************************************/
PaError CloseAudioStream( PABLIO_Stream *aStream )
{
    PaError err;
    int bytesEmpty;
    int byteSize = aStream->outFIFO.bufferSize;

    /* If we are writing data, make sure we play everything written. */
    if( byteSize > 0 )
    {
        bytesEmpty = PaUtil_GetRingBufferWriteAvailable( &aStream->outFIFO );
        while( bytesEmpty < byteSize )
        {
            Pa_Sleep( 10 );
            bytesEmpty = PaUtil_GetRingBufferWriteAvailable( &aStream->outFIFO );
        }
    }

    err = Pa_StopStream( aStream->stream );
    if( err != paNoError ) goto error;
    err = Pa_CloseStream( aStream->stream );
    if( err != paNoError ) goto error;
    Pa_Terminate();

error:
    PABLIO_TermFIFO( &aStream->inFIFO );
    PABLIO_TermFIFO( &aStream->outFIFO );
    free( aStream );
    return err;
}

/************************************************************/
PaError StartAudioStream( PABLIO_Stream *aStream )
{
    PaError err = paNoError;

    PaUtil_FlushRingBuffer( &aStream->inFIFO );
    PaUtil_FlushRingBuffer( &aStream->outFIFO );
    err = Pa_StartStream( aStream->stream );
    if( err != paNoError ) {
        fprintf(stderr, "Error starting audio stream!\n");
        CloseAudioStream( aStream );
    }

    return err;
}

/************************************************************/
PaError StopAudioStream( PABLIO_Stream *aStream )
{
    PaError err = paNoError;

    err = Pa_StopStream( aStream->stream );
    if( err != paNoError ) {
        fprintf(stderr, "Error stopping audio stream!\n");
      CloseAudioStream( aStream );
    }

    return err;
}
