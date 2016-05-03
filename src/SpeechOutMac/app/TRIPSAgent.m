//
//  TRIPSAgent.m
//  SpeechOutMac
//
//  Created by George Ferguson on 1/8/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import "TRIPSAgent.h"

@implementation TRIPSAgent

- (id)init {
	self = [super init];
	if (self) {
		// Setup connection to TRIPS facilitator
		kqmlStream = [KQMLStream connectedToHost:@"localhost" port: 6200];
		[kqmlStream setDelegate: self];
	}
	return self;
}

- (void)registerName:(NSString*)name {
	const char *namestr = [name cStringUsingEncoding:NSASCIIStringEncoding]; // Apparently auto-released
	KQMLPerformative *perf = KQMLNewPerformative("register");
	KQMLSetParameter(perf, ":name", (char*)namestr);
	[kqmlStream sendMessage:perf];
	KQMLFreePerformative(perf);
}

- (void)ready {
	KQMLPerformative *perf = KQMLNewPerformative("tell");
	KQMLSetParameter(perf, ":content", "(module-status ready)");
	[kqmlStream sendMessage:perf];
	KQMLFreePerformative(perf);
}

- (void)sendMessage:(KQMLPerformative*)msg {
	[kqmlStream sendMessage:msg];
}

/*
 * KQMLStreamDelegate methods
 */
- (void)KQMLStream:(KQMLStream*)stream receivedMessage:(KQMLPerformative*)msg {
	KQMLPerformative *perf = KQMLNewPerformative("error");
	NSString *msg_str = [KQMLStream performativeToString:msg];
	NSString *content_str = [NSString stringWithFormat:@"(message-not-handled %@)", msg_str];
	const char *content = [content_str cStringUsingEncoding:NSASCIIStringEncoding]; // Auto-released
	KQMLSetParameter(perf, ":content", (char*)content);
	[kqmlStream sendMessage:perf];
	[msg_str release];
	KQMLFreePerformative(perf);
}

- (void)eofOnKQMLStream:(KQMLStream*)stream {
	[NSApp terminate: self];
}

@end
