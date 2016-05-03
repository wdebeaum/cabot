/*
 * File: mactts.m
 * Creator: George Ferguson
 * Created: Mon Jan  4 10:46:49 2010
 * Time-stamp: <Mon Jan  4 11:29:29 EST 2010 ferguson>
 *
 * A reimplementation of the very simplest speech synthesis app using
 * the NSSpeechSynthesizer class.
 *
 * To compile:

    cc -framework Foundation -framework AppKit -o mactts mactts.m

 * References:

http://developer.apple.com/mac/library/documentation/Cocoa/Reference/ApplicationKit/Classes/NSSpeechSynthesizer_Class/Reference/Reference.html
http://developer.apple.com/mac/library/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/Reference/NSString.html

 */

#import <stdlib.h>
#import <stdio.h>
#import <Foundation/NSAutoreleasePool.h>
#import <Foundation/NSString.h>
#import <AppKit/NSSpeechSynthesizer.h>

NSSpeechSynthesizer *synth;

int
main(int argc, char *argv[]) {

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    //printf("argv[1]=\"%s\"\n", argv[1]);
    NSString *text = [NSString stringWithCString:argv[1] encoding:[NSString defaultCStringEncoding]];
    NSLog(@"text=\"%@\"", text);

    synth = [[NSSpeechSynthesizer alloc] init];
    if (!synth) {
	exit(1);
    }
    NSLog(@"synth=\"%@\"", synth);

    if (![synth startSpeakingString:text]) {
        exit(1);
    }

    /* Should do something like implementing the didFinishSpeaking callback here */
    NSLog(@"sleeping...");
    sleep(5);

    NSLog(@"done");
    [pool drain];
}
