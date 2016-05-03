/*
 * send.h
 *
 * George Ferguson, ferguson@cs.rochester.edu, 19 Jan 1996
 * $Id: send.h,v 1.4 2009/03/23 14:25:01 lgalescu Exp $
 */

#ifndef _send_h_gf
#define _send_h_gf

#include "trafficlight.h"

extern void sendReadyMsg(void);
extern void sendLogMessage(int uttnum, char *result);

/* lgalescu 2006/10/24 - added traffic light messaging */
extern void sendTrafficLightMsg(tLight color);

/* lgalescu 2007/09/12 - more additions */
extern void sendSorryMsg(char* inReplyTo, char* comment);
extern void sendDoneMsg(char* inReplyTo);

#endif
