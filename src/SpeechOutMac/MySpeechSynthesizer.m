/*
 * File: MySpeechSynthesizer.m
 * Creator: George Ferguson
 * Created: Mon Jan  4 12:54:05 2010
 * Time-stamp: <Mon Jan  4 13:59:06 EST 2010 ferguson>
 *
 * Implementation of a simple Objective-C class used by mactts2.
 * See mactts2.m for more details and references.
 */

#import <stdlib.h>
#import "MySpeechSynthesizer.h"
#import "DebugLog.h"

@implementation MySpeechSynthesizer

- (id)init {
    self = [super init];
    if (self) {
        synth = [[NSSpeechSynthesizer alloc] init];
        [synth setDelegate:self];
    }
    return self;
}
 
- (void)setExitWhenDone:(BOOL)b {
    exitWhenDone = b;
}

- (void)setVoice:(NSString*)voiceID {
    [synth setVoice:voiceID];
}

- (void)speak:(NSString*)text {
  DebugLog(@"self=%@, delegate=%@", self, [synth delegate]);
    [synth startSpeakingString:text];
    while ([synth isSpeaking]) {
    }
}

/*
 * NSSpeechSynthesizerDelegate protocol methods (callbacks).
 */

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender didFinishSpeaking:(BOOL)success {
    DebugLog(@"didFinishSpeaking: %@", success);
    if (exitWhenDone) {
        exit(0);
    }
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender didEncounterErrorAtIndex:(NSUInteger)characterIndex ofString:(NSString *)string message:(NSString *)message {
  DebugLog(@"didEncounterErrorAtIndex: %@ ofString: %@; message: %@", characterIndex, string, message);
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender didEncounterSyncMessage:(NSString *)message {
    DebugLog(@"didEncounterSyncMessage: %@", message);
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender willSpeakPhoneme:(short)phonemeOpcode {
    DebugLog(@"willSpeakPhoneme: %d", phonemeOpcode);
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender willSpeakWord:(NSRange)wordToSpeak ofString:(NSString *)text {
  DebugLog(@"willSpeakWord: [%d,%d] of: %@", wordToSpeak.location, wordToSpeak.length, text);
}

@end
