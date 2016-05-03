/*
 * tts.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 23 Sep 2002
 * Time-stamp: <Wed Apr  8 11:16:00 CDT 2009 lgalescu>
 */

#include <stdio.h>
#include <semaphore.h>
#include <ApplicationServices/ApplicationServices.h>
#include "util/debug.h"
#include "util/error.h"
#include "tts.h"
#include "pronunciation.h"

static SpeechChannel channel;
static sem_t *sem;

static void
errorMsg(char *msg, int theErr) {
    fprintf(stderr, "speechout-mac: %s: error %d\n", theErr);
}

/*
 * Called by speech channel when an error occurs during processing of text
 * to speak.
 */
static void
OurErrorCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
		     OSErr inError, long inBytePos)
{
    ERROR2("Error %d at pos=%ld\n", inError, inBytePos);
}

/*
 * Called by speech channel when all speech has been generated.
 */
void
OurSpeechDoneCallBackProc(SpeechChannel inSpeechChannel, long inRefCon)
{
    DEBUG0("posting semaphore");
    if (sem_post(sem) < 0) {
	SYSERR0("sem_post failed");
    }
    DEBUG0("done");
}

/*
 * Case-insensitive match on NAME.
 */
static int
getVoiceSpec(char *name, VoiceSpec *vspec)
{
    OSErr theErr = noErr;
    short nvoices;
    long vindex;
    VoiceDescription vdesc;

    DEBUG1("looking for voice %s", name);
    theErr = CountVoices(&nvoices);
    if (theErr != noErr) {
	errorMsg("CountVoices failed", theErr);
    } else {
	for (vindex = 1; vindex <= nvoices; vindex++) {
	    theErr = GetIndVoice(vindex, vspec);
	    if (theErr != noErr) {
	        errorMsg("GetIndVoice failed", theErr);
	    } else {
		theErr = GetVoiceDescription(vspec, &vdesc, sizeof(VoiceDescription));
		if (theErr != noErr) {
		    errorMsg("GetVoiceDescription failed", theErr);
		} if (strncasecmp(name, vdesc.name+1, vdesc.name[0]) == 0) {
		    DEBUG3("found voice %s: creator=%d, id=%d",
			    name, vspec->creator, vspec->id);
		    return 0;
		}
	    }
	}
    }
    return -1;
}

/*
 * Allocate speech channel and setup callbacks
 */
static SpeechChannel
createNewSpeechChannel(VoiceSpec *voiceSpec)
{
    SpeechChannel channel = NULL;
    OSErr theErr = noErr;

    theErr = NewSpeechChannel(voiceSpec, &channel);
    if (theErr != noErr) {
	errorMsg("NewSpeechChannel failed", theErr);
    } else {    
	theErr = SetSpeechInfo(channel, soErrorCallBack, OurErrorCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soErrorCallBack) failed", theErr);
	}
	theErr = SetSpeechInfo(channel, soSpeechDoneCallBack, OurSpeechDoneCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soSpeechDoneCallBack) failed", theErr);
	}
    }
    return channel;
}

int
tts_init(char *voice, char *dictfile)
{
    VoiceSpec vspec;
    int useDefaultVoice = 1;

    DEBUG0("creating semaphore");
    if ((sem = sem_open("speechout-mac.lock", O_CREAT, 0700, 0)) == (sem_t*)SEM_FAILED) {
	SYSERR0("sem_open failed");
	return -1;
    }
    if (voice != NULL) {
	DEBUG0("getting voice spec");
	if (getVoiceSpec(voice, &vspec) != 0) {
	    ERROR1("no voice named \"%s\"", voice);
	    return(-1);
	}
	useDefaultVoice = 0;
    }
    DEBUG0("creating speech channel");
    channel = createNewSpeechChannel(useDefaultVoice ? NULL : &vspec);
    if (channel == NULL) {
	return(-1);
    }
    if (dictfile) {
	DEBUG0("installing pronunciation dictionary");
	installPronunciationDictionary(channel, dictfile);
    }
    DEBUG0("done");
    return 0;
}


void
tts_send(char *txt, KQMLPerformative *perf)
{
    int theErr = noErr;

    /* Notify that we started speaking */
    DEBUG0("notifying started speaking");
    sendStartedSpeaking();
    DEBUG0("done");
    /* wait until speech-in got the message
       note: it's impractical to do this sync-ing via messages, so we'll just
       wait a reasonable amount of time, say 20ms */
    usleep(20000);
    /* Send the text */
    DEBUG1("saying \"%s\"", txt);
    theErr = SpeakText(channel, txt, strlen(txt) );
    if (theErr != noErr) {
	errorMsg("SpeakText failed", theErr);
    }
    /* Wait for speech to finish */
    DEBUG0("waiting");
    if (sem_wait(sem) < 0) {
	SYSERR0("sem_wait failed");
    }
    /* Notify that we stopped speaking */
    DEBUG0("notifying stopped speaking");
    sendStoppedSpeaking();
    DEBUG0("done");
    /* Send reply */
    DEBUG0("replying");
    sendDoneReply(perf);
    DEBUG0("done");
}

void tts_close()
{
    sem_close(sem);
    DisposeSpeechChannel(channel);
}
