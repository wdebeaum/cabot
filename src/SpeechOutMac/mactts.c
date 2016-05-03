/*
 * mactts.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 13 Dec 2001
 * Time-stamp: <Wed Dec 15 13:48:18 EST 2004 ferguson>
 *
 * To compile:

    cc -framework ApplicationServices -o mactts mactts.c

 *
 * Usage: mactts [-printvoices] [-voice NAME] ["TEXT TO SPEAK"]
 *
 * For API details, see:
 *  file://localhost/Developer/Documentation/Carbon/multimedia/SpeechSynthesisManager/Speech_Synthesis_Manager/index.html
 *
 * Several fields in a VoiceDescription are ``Pascal strings'' (C type
 * Str63 or Str255). These are a pain in the ass, since they have their
 * length as the first byte (and obviously have a maximum length).
 * In printf()-like functions, you need to use
 *    printf("%.*s", str[0], str+1)
 * and to do comparisons you need to use:
 *    strncmp(x, str+1, str[0]);
 * Other C string functions will work even worse. Gack!
 *
 * Note that we register all available callbacks (I think). This is
 * more for illustration than because they are actually used. All we
 * need for TRIPS SpeechOut is the SpeechDone callback, but the others
 * might be useful in the future.
 *
 */

#include <stdio.h>
#include <ApplicationServices/ApplicationServices.h>

void
errorMsg(char *msg, int theErr) {
    fprintf(stderr, "mactts: %s: error %d\n", theErr);
}

void
dumpVoices()
{
    OSErr theErr = noErr;
    short numOfVoices;
    long voiceIndex;
    VoiceSpec	theVoiceSpec;

    theErr = CountVoices(&numOfVoices);
    if (theErr != noErr) {
	errorMsg("CountVoices failed", theErr);
    } else {
	for ( voiceIndex = 1; voiceIndex <= numOfVoices; voiceIndex++) {
	    VoiceDescription theVoiceDesc;
	    theErr = GetIndVoice(voiceIndex, &theVoiceSpec);
	    if (theErr != noErr) {
	        errorMsg("GetIndVoice failed", theErr);
	    } else {
		theErr = GetVoiceDescription( &theVoiceSpec, &theVoiceDesc, sizeof( theVoiceDesc ) );
		if (theErr != noErr) {
		    errorMsg("GetVoiceDescription failed", theErr);
		} else {
		    printf("voice %d: \n", voiceIndex);
		    printf("  name=\"%.*s\"\n",
			   theVoiceDesc.name[0], theVoiceDesc.name+1);
		    printf("  comment=\"%.*s\"\n",
			   theVoiceDesc.comment[0], theVoiceDesc.comment+1);
		    printf("  gender=%s\n",
			   theVoiceDesc.gender == kNeuter ? "neuter" :
			   (theVoiceDesc.gender == kMale ? "male" :
			    (theVoiceDesc.gender == kFemale ? "female" :
			     "unknown")));
		    printf("  age=%d\n", theVoiceDesc.age);
		    /*
		    printf("  script=%d\n", theVoiceDesc.script);
		    printf("  language=%d\n", theVoiceDesc.language);
		    printf("  region=%d\n", theVoiceDesc.region);
		    */
		}
	    }
	}
    }
}

/*
 * Case-insensitive match on NAME.
 */
int
getVoiceSpec(char *name, VoiceSpec *vspec)
{
    OSErr theErr = noErr;
    short nvoices;
    long vindex;
    VoiceDescription vdesc;

    fprintf(stderr, "looking for voice %s\n", name);
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
		    fprintf(stderr, "found voice %s: creator=%d, id=%d\n",
			    name, vspec->creator, vspec->id);
		    return 0;
		}
	    }
	}
    }
    return -1;
}

/*
 * Callbacks
 */

/*
 * Called by speech channel when an error occurs during processing of text
 * to speak.
 */
void
OurErrorCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
		     OSErr inError, long inBytePos)
{
    printf("Error %d at pos=%ld\n", inError, inBytePos);
}

/*
 * Called by speech channel when all text has been processed.
 * Additional text can be passed back to continue processing.
 */
void
OurTextDoneCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
			const void **nextBuf, unsigned long *byteLen,
			long *controlFlags)
{
    *nextBuf = NULL;
    printf("TextDone\n");
}

/*
 * Called by speech channel when all speech has been generated.
 */
void
OurSpeechDoneCallBackProc(SpeechChannel inSpeechChannel, long inRefCon)
{
    printf("SpeechDone\n");
}

/*
 * Called by speech channel when it encouters a synchronization command
 * within an embedded speech comand in text being processed.
 */
void
OurSyncCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
		    OSType inSyncMessage)
{
    printf("Sync: msg=0x%lx\n", inSyncMessage);
}

/*
 * Called by speech channel every time a phoneme is about to be generated.
 * You might use this to animate a speaking character.
 */
void
OurPhonemeCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
		       short inPhonemeOpcode)
{
    printf("Phoneme: %d\n", inPhonemeOpcode);
}

/*
 * Called by speech channel every time a word is about to be generated.
 */
void
OurWordCallBackProc(SpeechChannel inSpeechChannel, long inRefCon,
		    long inWordPos, short inWordLen)
{
    printf("Word: pos=%ld, len=%d\n", inWordPos, inWordLen);
}

/*
 * Allocate speech channel and setup callbacks
 */
SpeechChannel
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
	theErr = SetSpeechInfo(channel, soPhonemeCallBack, OurPhonemeCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soPhonemeCallBack) failed", theErr);
	}
	theErr = SetSpeechInfo(channel, soSpeechDoneCallBack, OurSpeechDoneCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soSpeechDoneCallBack) failed", theErr);
	}
	theErr = SetSpeechInfo(channel, soSyncCallBack, OurSyncCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soSyncCallBack) failed", theErr);
	}
	theErr = SetSpeechInfo(channel, soTextDoneCallBack, OurTextDoneCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soTextDoneCallBack) failed", theErr);
	}
	theErr = SetSpeechInfo(channel, soWordCallBack, OurWordCallBackProc);
	if (theErr != noErr) {
	    errorMsg("SetSpeechInfo(soWordCallBack) failed", theErr);
	}
    }
    return channel;
}

int
main(int argc, char **argv)
{
    VoiceSpec vspec;
    SpeechChannel channel;
    int useDefaultVoice = 1;
    char *theTextToSpeak = "Hello world.";
    int theErr = noErr;

    argc -= 1;
    argv += 1;
    while (argc > 0) {
	if (strcmp(argv[0], "-printvoices") == 0) {
	    dumpVoices();
	    exit(0);
	} else if (strcmp(argv[0], "-voice") == 0) {
	    if (getVoiceSpec(argv[1], &vspec) != 0) {
		fprintf(stderr, "no voice named \"%s\"\n", argv[1]);
		exit(1);
	    }
	    useDefaultVoice = 0;
	    argv += 2;
	} else {
	    theTextToSpeak = argv[0];
	    break;
	}
    }
    printf("creating speech channel\n");
    channel = createNewSpeechChannel(useDefaultVoice ? NULL : &vspec);
    if (channel == NULL) {
	exit(1);
    }
    printf("saying \"%s\"\n", theTextToSpeak);
    theErr = SpeakText(channel, theTextToSpeak, strlen(theTextToSpeak) );
    if (theErr != noErr) {
	errorMsg("SpeakText failed", theErr);
    }
    printf("sleeping...\n");
    /* Should do something like wait for the SpeechDone callback here */
    sleep(5);
    printf("done\n");
}
