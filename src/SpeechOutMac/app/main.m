//
//  main.m
//  SpeechOutMac
//
//  Created by George Ferguson on 1/5/10.
//  Copyright 2010 University of Rochester. All rights reserved.
//

#import <Cocoa/Cocoa.h>

// TRIPS C library linkage
char *progname;

int main(int argc, char *argv[])
{
	progname = argv[0];
	return NSApplicationMain(argc,  (const char **) argv);
}
