//
//  TRIPSAgent.h
//  SpeechOutMac
//
//  Created by George Ferguson on 1/8/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import <Foundation/NSString.h>
#import "KQML/KQMLStream.h"

@interface TRIPSAgent : NSObject <KQMLStreamDelegate> {
	KQMLStream *kqmlStream;
}

-(id)init;

-(void)registerName:(NSString*)name;
-(void)ready;
-(void)sendMessage:(KQMLPerformative*)msg;

- (void)KQMLStream:(KQMLStream*)stream receivedMessage:(KQMLPerformative*)msg;
- (void)eofOnKQMLStream:(KQMLStream*)stream;

@end
