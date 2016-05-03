/*
 * File: listvoices.m
 * Creator: George Ferguson
 * Created: Mon Jan  4 11:28:18 2010
 * Time-stamp: <Mon Jan  4 13:27:46 EST 2010 ferguson>
 *
 * Reimplementation of the "listvoices.c" code using NSSpeechSynthesizer.
 * Pass -v to see all the attributes of the voices as well as their
 * identifiers.
 *
 * To compile:

    cc -framework Foundation -framework AppKit -o listvoices listvoices.m DebugLog.m

 * References:

http://developer.apple.com/mac/library/documentation/Cocoa/Reference/ApplicationKit/Classes/NSSpeechSynthesizer_Class/Reference/Reference.html
http://developer.apple.com/mac/library/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/Reference/NSString.html

 */

#import <stdlib.h>
#import <stdio.h>
#import <Foundation/NSArray.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSDictionary.h>
#import <Foundation/NSString.h>
#import <AppKit/NSSpeechSynthesizer.h>
#import "DebugLog.h"

NSSpeechSynthesizer *synth;

int
main(int argc, char *argv[]) {

    BOOL verbose = (argc > 1 && strcmp(argv[1],"-v") == 0);

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    NSArray *voiceIDs = [NSSpeechSynthesizer availableVoices];
    for (NSString *voiceID in voiceIDs) {
        DebugLog(@"%@", voiceID);
	if (verbose) {
            NSDictionary *attrs = [NSSpeechSynthesizer attributesForVoice:voiceID];
	    for (id key in attrs) {
	        DebugLog(@"  %@: %@\n", key, [attrs objectForKey:key]);
	    }
	}
    }

    [pool drain];
}
