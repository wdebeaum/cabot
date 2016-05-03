//
//  MySpeechSynthesizer.h
//  SpeechOutMac
//
//  Created by George Ferguson on 1/6/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface MySpeechSynthesizer : NSSpeechSynthesizer {

}

- (id)getProperty:(NSString*)property;
- (void)setProperty:(NSString*)property value:(id)value;
- (NSNumber*)getRate;
- (NSNumber*)getPitchBase;
- (NSNumber*)getPitchMod;
- (NSNumber*)getVolume;
- (void)setRate:(NSNumber*)value;
- (void)setPitchBase:(NSNumber*)value;
- (void)setPitchMod:(NSNumber*)value;
- (void)setVolume:(NSNumber*)value;
- (void)reset;

@end
