/*
 * trafficlight.h
 *
 * Time-stamp: <Mon Mar 26 13:19:56 CDT 2007 lgalescu>
 *
 * Author: Lucian Galescu <lgalescu@ihmc.us>
 * $Id: trafficlight.h,v 1.2 2007/03/26 18:15:10 lgalescu Exp $
 *
 */

#ifndef _trafficlight_h_
#define _trafficlight_h_

typedef enum {
  GREEN,
  YELLOW,
  RED
} tLight;

#define toString(c)	(((c)==GREEN) ? "green" : (((c)==YELLOW) ? "yellow" : "red"))

#endif
