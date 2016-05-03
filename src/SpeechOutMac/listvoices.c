/*
 * listvoices.c
 *
 * George Ferguson, ferguson@cs.rochester.edu, 25 Sep 2002
 * Time-stamp: <Tue Dec 14 15:12:23 EST 2004 ferguson>
 *
 * This code is a small uncommented part of "mactts.c".
 *
 * To compile:

    cc -framework ApplicationServices -o listvoices listvoices.c

 */
#include <stdio.h>
#include <ApplicationServices/ApplicationServices.h>

void
errorMsg(char *msg, int theErr) {
    fprintf(stderr, "listvoices: %s: error %d\n", theErr);
}

int
main(int argc, char **argv)
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
		    printf("%-10.*s",
			   theVoiceDesc.name[0], theVoiceDesc.name+1);
		    printf(" %s",
			   theVoiceDesc.gender == kNeuter ? "n" :
			   (theVoiceDesc.gender == kMale ? "m" :
			    (theVoiceDesc.gender == kFemale ? "f" :
			     "unknown")));
		    printf(" %4d", theVoiceDesc.age);
		    printf(" %.*s",
			   theVoiceDesc.comment[0], theVoiceDesc.comment+1);
		    printf("\n");
		    /* Always 0 as of MacOSX 10.1.5
		    printf("  script=%d\n", theVoiceDesc.script);
		    printf("  language=%d\n", theVoiceDesc.language);
		    printf("  region=%d\n", theVoiceDesc.region);
		    */
		}
	    }
	}
    }
}
