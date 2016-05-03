/*
 * File: DebugLog.m
 * Creator: George Ferguson
 * Created: Mon Jan  4 13:23:32 2010
 * Time-stamp: <Mon Jan  4 13:26:50 EST 2010 ferguson>
 *
 * Less verbose Objective-C printing (than NSLog) from:
 *   http://www.karlkraft.com/index.php/2009/03/23/114/
 */

#import <stdio.h>
#import <stdarg.h>
#import "DebugLog.h"

void DebugLog(NSString *format,...) {
    va_list ap;
    va_start (ap, format);
    if (![format hasSuffix: @"\n"]) {
        format = [format stringByAppendingString: @"\n"];
    }
    NSString *body =  [[NSString alloc] initWithFormat: format arguments: ap];
    va_end (ap);
    fprintf(stderr,"%s",[body UTF8String]);
    [body release];
}
