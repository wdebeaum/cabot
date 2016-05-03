//
//  VoicesPopUpButton.m
//  SpeechOutMac
//
//  Created by George Ferguson on 1/5/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import "VoicesPopUpButton.h"

@implementation VoicesPopUpButton

- (id)initWithCoder:(NSCoder *)decoder {
	//NSLog(@"VoicesPopUpButton.initWithCoder");
	self = [super initWithCoder:decoder];
	if (self) {
		isShowingMoreVoices = NO;
		[self initializeMenuItems];
	}
	return self;
}

- (void)initializeMenuItems {
	// We look after this ourselves
	[self setAutoenablesItems:NO];
	// Allocate temporary arrays
	NSMutableArray *maleVoices = [NSMutableArray arrayWithCapacity:0];
	NSMutableArray *femaleVoices = [NSMutableArray arrayWithCapacity:0];
	NSMutableArray *neuterVoices = [NSMutableArray arrayWithCapacity:0];
	// Iterate through available voices and save in appropriate array
	NSArray *voiceIDs = [NSSpeechSynthesizer availableVoices];
    for (NSString *voiceID in voiceIDs) {
		NSDictionary *attrs = [NSSpeechSynthesizer attributesForVoice:voiceID];
		NSString *gender = (NSString*)[attrs objectForKey:NSVoiceGender];
		// Undocumented property of system voices
		BOOL showInFullListOnly = ([attrs objectForKey:@"VoiceShowInFullListOnly"] != nil);
		if (isShowingMoreVoices || !showInFullListOnly) {
			if ([gender isEqual:NSVoiceGenderMale]) {
				[maleVoices addObject:voiceID];
			} else if ([gender isEqual:NSVoiceGenderFemale]) {
				[femaleVoices addObject:voiceID];
			} else {
				[neuterVoices addObject:voiceID];
			}
		}
	}
	// Sort arrays for presentation
	[maleVoices sortUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
	[femaleVoices sortUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
	[neuterVoices sortUsingSelector:@selector(localizedCaseInsensitiveCompare:)];
	// Prepare to setup popup menu (and mapping from menu indexes to voice IDs)
	[self removeAllItems];
	// First entry is "use default", with voice ID NIL
	[self addMenuItem:@"Use System Default" object:nil];
	// Male voices
	if ([maleVoices count] > 0) {
		[self addMenuItems:maleVoices label:@"Male"];
	}
	// Female voices
	if ([femaleVoices count] > 0) {
		[self addMenuItems:femaleVoices label:@"Female"];
	}
	// Neuter voices
	if ([neuterVoices count] > 0) {
		[self addMenuItems:neuterVoices label:@"Neuter"];
	}
	// Show full list toggle
	[self addSeparator];
	[self addShowMoreToggleButton];
}

- (NSMenuItem*)addMenuItem:(NSString *)voiceName object:(NSString*)voiceID {
	//NSLog(@"addMenuItem: \"%@\" = \"%@\"", voiceName, voiceID);
	NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:voiceName action:NULL keyEquivalent:@""];
	[item setRepresentedObject:voiceID];
	[[self menu] addItem:item];
	//[self synchronizeTitleAndSelectedItem];
	return item;
}

- (void)addSeparator {
	[[self menu] addItem:[NSMenuItem separatorItem]];
}

- (void)addLabel:(NSString*)title {
	NSMenuItem *item = [self addMenuItem:title object:nil];
	[item setEnabled:NO];
}

- (void)addMenuItems:(NSArray*)items label:(NSString*)label {
	[self addSeparator];
	[self addLabel:label];
	for (NSString* voiceID in items) {
		NSDictionary *attrs = [NSSpeechSynthesizer attributesForVoice:voiceID];
		NSString *voiceName = (NSString*)[attrs objectForKey:NSVoiceName];
		[self addMenuItem:voiceName object:voiceID];
	}
}

- (void)addShowMoreToggleButton {
	NSString *title = (isShowingMoreVoices ? @"Show Fewer Voices" : @"Show More Voices");
	NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:title action:@selector(toggleShowMoreVoices) keyEquivalent:@""];
	[item setTarget:self];
	[[self menu] addItem:item];
}

- (void)toggleShowMoreVoices {
	//NSLog(@"VoicesPopUpButton.toggleShowMoreVoices");
	isShowingMoreVoices = !isShowingMoreVoices;
	[self initializeMenuItems];
}

- (NSString*)getSelectedVoiceID {
	int index = [self indexOfSelectedItem];
	if (index == 0) {
		// Use system default
		return nil;
	} else {
		return (NSString*)[[self itemAtIndex:index] representedObject];
	}
}

@end
