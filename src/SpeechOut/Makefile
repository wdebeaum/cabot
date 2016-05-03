#
# File: SpeechOut/Makefile						      
# Creator: George Ferguson						      
# Created: 8 Jan 1996
# Time-stamp: <Wed Feb 21 16:03:03 EST 2007 ferguson>			   
#									       

PROG = SpeechOut

CONFIGDIR=../config
include $(CONFIGDIR)/version.mk
include $(CONFIGDIR)/defs.mk
include $(CONFIGDIR)/speech-out/defs.mk

default all $(PROG): $(PROG).sh
	sed -e 's@TRIPS_BASE_DEFAULT=.*$$@TRIPS_BASE_DEFAULT=$(prefix)@' \
	    -e 's@SPEECH_OUT_COMPONENT_DEFAULT=.*$$@SPEECH_OUT_COMPONENT_DEFAULT=$(SPEECH_OUT_COMPONENT)@' $(PROG).sh >$(PROG)
	chmod a+x $(PROG)

install: $(PROG)
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(PROG) $(bindir)

clean:
	rm -f $(PROG)
