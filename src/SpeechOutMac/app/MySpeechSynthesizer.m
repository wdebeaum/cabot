//
//  MySpeechSynthesizer.m
//  SpeechOutMac
//
//  Created by George Ferguson on 1/6/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//
/**
 * This class simply wraps NSSpeechSynthesizer so we don't have to deal with properties.
 */

#import "MySpeechSynthesizer.h"


@implementation MySpeechSynthesizer

- (id)getProperty:(NSString*)property {			
	NSError *err;
	id value = [self objectForProperty: property error: &err];
	if ([err code] != 0) {
		NSLog(@"MySpeechSynthesizer.getProperty: %@: %@", property, err);
		return nil;
	} else {
		return value;
	}
}

- (void)setProperty:(NSString*)property value:(id)value {			
	NSError *err;
	[self setObject: value forProperty: property error: &err];
	if ([err code] != 0) {
		NSLog(@"MySpeechSynthesizer.setProperty: %@: %@: %@", property, value, err);
	}
}

- (NSNumber*)getRate {
	return (NSNumber*)[self getProperty: NSSpeechRateProperty];
}

- (NSNumber*)getPitchBase {
	return (NSNumber*)[self getProperty: NSSpeechPitchBaseProperty];
}

- (NSNumber*)getPitchMod {
	return (NSNumber*)[self getProperty: NSSpeechPitchModProperty];
}

- (NSNumber*)getVolume {
	return (NSNumber*)[self getProperty: NSSpeechVolumeProperty];
}

- (void)setRate:(NSNumber*)value {
	[self setProperty: NSSpeechRateProperty value: value];
}

- (void)setPitchBase:(NSNumber*)value {
	[self setProperty: NSSpeechPitchBaseProperty value: value];
}

- (void)setPitchMod:(NSNumber*)value {
	[self setProperty: NSSpeechPitchModProperty value: value];
}

- (void)setVolume:(NSNumber*)value {
	[self setProperty: NSSpeechVolumeProperty value: value];
}

- (void)reset {
	[self setProperty: NSSpeechResetProperty value: NULL];
}

@end
