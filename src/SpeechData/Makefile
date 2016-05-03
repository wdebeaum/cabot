#
# Makefile for TRIPS SpeechData
#
# George Ferguson, ferguson@cs.rochester.edu,  2 Dec 1997
# Time-stamp: <Tue Oct 23 16:55:46 EDT 2001 ferguson>
#
# Not much to do here, except make sure that the symlink to the real
# Sphinx SpeechData directory is installed. We don't copy this stuff around to
# avoid filling the filesystem with copies of the (unchanging) acoustic
# models.

CONFIGDIR=../config
include $(CONFIGDIR)/trips.mk

all default clean install.man:
	@echo 'No need to make $@ in SpeechData.'

install:
	test -d $(TRIPS_BASE)/etc || mkdir $(TRIPS_BASE)/etc
	-rm $(TRIPS_BASE)/etc/SpeechData
	ln -s /u/trains/SpeechData2 $(TRIPS_BASE)/etc/SpeechData

