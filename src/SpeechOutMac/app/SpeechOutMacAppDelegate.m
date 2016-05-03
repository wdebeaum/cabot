//
//  SpeechOutMacAppDelegate.m
//  SpeechOutMac
//
//  Created by George Ferguson on 1/5/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import "SpeechOutMacAppDelegate.h"

@implementation SpeechOutMacAppDelegate

/*
 * Initialization
 */
- (void)applicationDidFinishLaunching:(NSNotification *)aNotification {
	// Create synthesizer
	synth = [[MySpeechSynthesizer alloc] init];
	[synth setDelegate: self];
	isSpeaking = NO;
	// Select all text in the NSTextField so it's typed over by default
	[textField selectText:self];
	// Can't wait to update params from voice until prefs pane is opened
	// since we might hit Speak without opening the prefs!
	[self updateVoiceParams];
	// Tell TRIPS we're ready
	[self registerName:@"speech-out"];
	[self ready];
}

/*
 * handleSpeakButton: called when the Speak/Stop button is pressed.
 */
- (void) handleSpeakButton:(id)sender {
	if (!isSpeaking) {
		isSpeaking = YES;
		[speakButton setTitle: @"Stop"];
		[self speak: [textField stringValue]];
	} else {
		isSpeaking = NO;
		[synth stopSpeaking];
	}
}

/*
 * Start speaking the given text.
 * Sets the synth voice properties before speaking.
 */
- (void)speak:(NSString*)text {
	[self setVoiceFromPopUp];
	[synth setRate: [self getUserDefault:@"speechRate"]];
	[synth setPitchBase: [self getUserDefault:@"speechPitchBase"]];
	[synth setPitchMod: [self getUserDefault:@"speechPitchMod"]];
	[synth setVolume: [self getUserDefault:@"speechVolume"]];
	[synth startSpeakingString: text];
}

/*
 * NSSpeechSynthesizer delegate method called when speech output stops
 * (for any reason).
 */
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender didFinishSpeaking:(BOOL)finishedSpeaking {
	[speakButton setTitle: @"Speak"];
	isSpeaking = NO;
}

- (IBAction)openPrefsPane:(id)sender {
	NSLog(@"openPrefsPane");
	// FIXME: This hack avoids having an empty item selected in the voicesPopUp
	// in the case where there is no voice specified in the defaults (i.e.,
	// when there is no preferences file).
	NSArray *items = [[voicesPopUp menu] itemArray];
	NSMenuItem *item = [items objectAtIndex:([items count]-1)];
	if ([[item title] isEqual:@""]) {
		NSLog(@"openPrefsPane: removing empty menu item");
		[[voicesPopUp menu] removeItem:item];
		[voicesPopUp selectItemAtIndex:0];
	}	
	// Put up the preferences window (if we didn't need the above, we
	// could just bind this to the "Preferences" menu item directly in
	// IB)
	[prefsWindow makeKeyAndOrderFront:sender];
}
/*
 * Action invoked when a voice is selected in the voicesPopUp.
 */
- (IBAction)didSelectVoice:(id)sender {
	//NSLog(@"didSelectVoice: %@", [voicesPopUp titleOfSelectedItem]);
	[self updateVoiceParams];
}

/*
 * Set the voice for our synthesizer based on the voice selected in the popup,
 * then update the voice attributes (displayed in the preferences window)
 * from the current settings of the synth.
 * This is called after a voice is selected (didSelectVoice) or after
 * a voice is reset (resetVoice).
 */
- (void)updateVoiceParams {
	[self setVoiceFromPopUp];
	[self setUserDefault:@"speechRate" value:[synth getRate]];
	[self setUserDefault:@"speechPitchBase" value:[synth getPitchBase]];
	[self setUserDefault:@"speechPitchMod" value:[synth getPitchMod]];
	[self setUserDefault:@"speechVolume" value:[synth getVolume]];
}

/*
 * Set the voice of our synthesizer based on the selection in the voicesPopUp.
 * This is used in the didSelectCallback so that we can display the current
 * properties of the synth, and also in the speak method before speaking.
 */
- (void)setVoiceFromPopUp {
	NSString *voiceID = [voicesPopUp getSelectedVoiceID];
	if (voiceID != nil) {
		[synth setVoice:voiceID];
	} else {
		[synth setVoice:[NSSpeechSynthesizer defaultVoice]];
	}
}

/*
 * Tell the shared UserDefaultsController a new value for the given key.
 * This will update preference pane controls which are bound to the value
 * (as well as saving it in the user defaults aka. preferences file).
 * http://developer.apple.com/mac/library/documentation/cocoa/conceptual/CocoaBindings/Concepts/NSUserDefaultsController.html
 * "Programmatically Accessing NSUserDefaultsController Values"
 */
- (void)setUserDefault:(NSString*)key value:(id)value {
	[[[NSUserDefaultsController sharedUserDefaultsController] values] setValue: value forKey: key];
}

/*
 * Return the object from the shared UserDefaultsController for the given
 * key.
 */
- (id)getUserDefault:(NSString*)key {
	return [[[NSUserDefaultsController sharedUserDefaultsController] values] valueForKey: key];
}

/*
 * Reset synth to defaults and update views.
 */
- (IBAction)resetVoice:(id)sender {
	NSLog(@"resetVoice");
	[synth reset];
	[self updateVoiceParams];
}

/*
 * KQMLStreamDelegate method
 *
 * Very simple handling of this (for now). KQMLPerformative is not very object-oriented
 * in the C library version (nor in our Objective-C wrapper). But you know, it's almost
 * harder to write this stuff once than to do it right under the hood an then code this
 * specific component's handler...
 */
- (void)KQMLStream:(KQMLStream*)stream receivedMessage:(KQMLPerformative*)msg {
	int done = 0;
	char *verb = KQML_VERB(msg);
	NSLog(@"SpeechOutMac: receivedMessage: %s", verb);
	if (strcasecmp(verb, "request") == 0) {
		NSLog(@"we have a request");
		char *content_str = KQMLGetParameter(msg, ":content");
		if (content_str != NULL) {
			NSLog(@"we have content");
			char **content = KQMLParseThingList(content_str);
			if (content != NULL) {
				NSLog(@"content parsed ok");
				char *content0 = content[0];
				if (strcasecmp(content0, "exit") == 0) {
					NSLog(@"exiting");
					[NSApp terminate: self];
					done = 1;
				} else if (strcasecmp(content0, "say") == 0) {
					const char *text = content[1];
					if (text != NULL) {
						NSString *text_str = [NSString stringWithCString: text encoding: NSASCIIStringEncoding];
						[textField setStringValue: text_str];
						[self speak: text_str];
						done = 1;
					}
				}
			}
			// Free memory allocated by KQMLParseThingList
			while (*content) {
				free(*content);
				content += 1;
			}
		}
	}
	// If we didn't handle it, let the supperclass at it
	if (!done) {
		[super KQMLStream:stream receivedMessage:msg];
	}
}

@end
