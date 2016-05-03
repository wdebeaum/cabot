/*
 * File: MySpeechSynthesizer.h
 * Creator: George Ferguson
 * Created: Mon Jan  4 12:54:59 2010
 * Time-stamp: <Mon Jan  4 13:44:52 EST 2010 ferguson>
 *
 * Specification of a simple Objective-C class used by mactts2.
 * details and references.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import <AppKit/NSSpeechSynthesizer.h>

@interface MySpeechSynthesizer: NSObject <NSSpeechSynthesizerDelegate> {
    NSSpeechSynthesizer *synth;
    BOOL exitWhenDone;
}

-(id) init;
-(void) setExitWhenDone:(BOOL)b;
-(void) setVoice:(NSString*)voiceID;
-(void) speak:(NSString*)text;

@end
