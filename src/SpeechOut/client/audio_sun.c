/*
 * audio_sun.c: Sun /dev/audio version of the audio driver
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Aug 1996
 * Time-stamp: <Wed May 14 13:53:01 EDT 1997 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/audioio.h>
#include "util/error.h"
#include "util/debug.h"

/*
 * Functions defined here:
 */
int audioInit_SUN(char *name);
void audioClose_SUN(void);
void audioWrite_SUN(char *bytes, int num_bytes);
void audioSync_SUN(void);

/*
 * Data defined here:
 */
static int audioFd;

/*	-	-	-	-	-	-	-	-	*/

int
audioInit_SUN(char *name)
{
    audio_info_t info;
 
    DEBUG1("opening audio device %s", name);
    if ((audioFd = open(name, O_WRONLY | O_NDELAY)) < 0) {
	SYSERR1("couldn't open audio device %s", name);
	return -1;
    }
    DEBUG0("configuring audio device");
    AUDIO_INITINFO(&info);
    info.play.sample_rate = 16000;
    info.play.precision = 16;
    info.play.encoding = AUDIO_ENCODING_LINEAR;
    if (ioctl(audioFd, AUDIO_SETINFO, &info) < 0) {
	SYSERR0("couldn't configure audio device (continuing anyway)");
    }
    DEBUG0("done");
    return 0;
}

void
audioClose_SUN(void)
{
    DEBUG0("closing audio device");
    close(audioFd);
    DEBUG0("done");
}

/*
 * This adds data to the buffer waiting to go to the audio server.
 */
void
audioWrite_SUN(char *bytes, int num_bytes)
{
    DEBUG1("sending %d bytes to audio device", num_bytes);
    write(audioFd, bytes, num_bytes);
    DEBUG0("done");
}

void
audioSync_SUN(void)
{
    DEBUG0("draining audio device");
    ioctl(audioFd, AUDIO_DRAIN);
    DEBUG0("done");
}
