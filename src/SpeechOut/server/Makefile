#
# Makefile for TRIPS TrueTalk client
#
# George Ferguson, ferguson@cs.rochester.edu, 8 Jan 1996
# Time-stamp: <Thu Aug  3 18:25:18 EDT 2000 ferguson>
#

CONFIGDIR = ../../config
include $(CONFIGDIR)/trips.mk
include $(CONFIGDIR)/defs/$(UNAME).mk

MODULE = SpeechOut

tts default all: tts.sh
	sed -e 's@TT_BASE_DEFAULT=.*@TT_BASE_DEFAULT=$(TT_BASE)@' <tts.sh >tts
	chmod a+x tts

install: tts
	test -d $(TRIPS_BASE)/bin || mkdir $(TRIPS_BASE)/bin
	$(CP) tts $(TRIPS_BASE)/bin

install.man:
	test -d $(TRIPS_BASE)/man || mkdir $(TRIPS_BASE)/man
	test -d $(TRIPS_BASE)/man/man1 || mkdir $(TRIPS_BASE)/man/man1
	$(CP) tts.man $(TRIPS_BASE)/man/man1/tts.1

clean:
	$(RM) tts
