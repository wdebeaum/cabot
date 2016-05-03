//
//  SpeechOutMacAppDelegate.h
//  SpeechOutMac
//
//  Created by George Ferguson on 1/5/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "VoicesPopUpButton.h"
#import "MySpeechSynthesizer.h"
#import "TRIPSAgent.h"

@interface SpeechOutMacAppDelegate : TRIPSAgent <NSSpeechSynthesizerDelegate> {

    IBOutlet NSWindow *window;
	IBOutlet NSTextField *textField;
	IBOutlet NSButton *speakButton;
	
	IBOutlet NSWindow *prefsWindow;
	IBOutlet VoicesPopUpButton *voicesPopUp;
	
	MySpeechSynthesizer *synth;
	BOOL isSpeaking;
	
}

- (IBAction)handleSpeakButton:(id)sender;
- (void)speak:(NSString*)text;
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender didFinishSpeaking:(BOOL)finishedSpeaking;

- (IBAction)openPrefsPane:(id)sender;
- (IBAction)didSelectVoice:(id)sender;
- (void)updateVoiceParams;
- (void)setVoiceFromPopUp;
- (void)setUserDefault:(NSString*)key value:(id)value;
- (id)getUserDefault:(NSString*)key;

- (IBAction)resetVoice:(id)sender;

@end
