/*
 * File: mactts2.m
 * Creator: George Ferguson
 * Created: Mon Jan  4 12:51:33 2010
 * Time-stamp: <Mon Jan  4 14:22:51 EST 2010 ferguson>
 *
 * Build on the basic mactts.m to illustrate a few additional features.
 * This uses an objective-c class, MySpeechSynthesizer.[ch], in order
 * to use callbacks.
 *
 *** This version  fails to call the delegate methods, probably because there
 *** is no event dispatching loop like onewould expect in a Cocoa app
 *** (part of NSApplicationMain(), usually).
 *
 * To compile:

    cc -framework Foundation -framework AppKit -o mactts2 mactts2.m MySpeechSynthesizer.m DebugLog.m

 * References:

http://developer.apple.com/mac/library/documentation/Cocoa/Reference/ApplicationKit/Classes/NSSpeechSynthesizer_Class/Reference/Reference.html
http://developer.apple.com/mac/library/documentation/Cocoa/Reference/Foundation/Classes/NSString_Class/Reference/NSString.html

 */

#import <stdlib.h>
#import <stdio.h>
#import <Foundation/NSAutoreleasePool.h>
#import "MySpeechSynthesizer.h"
#import "DebugLog.h"

MySpeechSynthesizer *synth;

int
main(int argc, char *argv[]) {

    NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

    NSString *text = [NSString stringWithCString:argv[1] encoding:[NSString defaultCStringEncoding]];
    DebugLog(@"text=\"%@\"", text);

    synth = [[MySpeechSynthesizer alloc] init];
    DebugLog(@"synth=%@", synth);

    [synth setExitWhenDone:YES];

    DebugLog(@"speaking...");
    [synth speak:text];

    //DebugLog(@"sleeping...");
    //sleep(999);
}
