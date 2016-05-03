#
# Makefile for Java Speech Controller
#
# George Ferguson, ferguson@cs.rochester.edu, 23 Feb 1999
# $Id: Makefile,v 1.3 2008/06/17 14:55:32 lgalescu Exp $
#

MAIN    = SpeechController
PACKAGE = TRIPS.SpeechController
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util

SRCS = SpeechController.java \
	SpeechToken.java \
	JLevelMeter.java

CLASSES = $(SRCS:.java=.class) \
	JLevelMeter$$MeterLayout.class

CONFIGDIR=../config
include $(CONFIGDIR)/java/prog.mk
