/*
 * audio.c: Generic interface to the audio driver(s)
 *
 * George Ferguson, ferguson@cs.rochester.edu, 20 Aug 1996
 * Time-stamp: <Wed May 14 14:02:51 EDT 1997 ferguson>
 */
#include <stdio.h>
#include <stdlib.h>
#include "util/debug.h"
#include "audio.h"

/*
 * Functions defined here:
 */
int audioInit(char *name);
void audioClose(void);
void audioWrite(char *bytes, int num_bytes);
void audioSync(void);

/*
 * Data defined here:
 */
static enum { AUDIO_AUDIOFILE, AUDIO_SUNAUDIO } audioType;

/*	-	-	-	-	-	-	-	-	*/

int
audioInit(char *name)
{
    int ret;

    DEBUG1("initializing audio: \"%s\"", name);
    if (name != NULL && *name == '/') {
	audioType = AUDIO_SUNAUDIO;
    } else {
	audioType = AUDIO_AUDIOFILE;
    }
    switch (audioType) {
      case AUDIO_AUDIOFILE: 
	  ret = audioInit_AF(name);
	  break;
      case AUDIO_SUNAUDIO: 
	  ret = audioInit_SUN(name);
	  break;
    }
    DEBUG0("done");
    return ret;
}

void
audioClose(void)
{
    switch (audioType) {
      case AUDIO_AUDIOFILE: 
	  audioClose_AF();
	  break;
      case AUDIO_SUNAUDIO: 
	  audioClose_SUN();
	  break;
    }
}

void
audioWrite(char *bytes, int num_bytes)
{
    switch (audioType) {
      case AUDIO_AUDIOFILE: 
	  audioWrite_AF(bytes, num_bytes);
	  break;
      case AUDIO_SUNAUDIO: 
	  audioWrite_SUN(bytes, num_bytes);
	  break;
    }
}

void
audioSync(void)
{
    switch (audioType) {
      case AUDIO_AUDIOFILE: 
	  audioSync_AF();
	  break;
      case AUDIO_SUNAUDIO: 
	  audioSync_SUN();
	  break;
    }
}
