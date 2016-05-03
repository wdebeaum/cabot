//
//  VoicesPopUpButton.h
//  SpeechOutMac
//
//  Created by George Ferguson on 1/5/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface VoicesPopUpButton : NSPopUpButton {
	IBOutlet BOOL isShowingMoreVoices;
}

- (void)initializeMenuItems;
- (NSMenuItem*)addMenuItem:(NSString *)voiceName object:(NSString*)voiceID;
- (void)addSeparator;
- (void)addLabel:(NSString*)title;
- (void)addMenuItems:(NSArray*)items label:(NSString*)label;
- (void)addShowMoreToggleButton;
- (void)toggleShowMoreVoices;
- (NSString*)getSelectedVoiceID;

@end
