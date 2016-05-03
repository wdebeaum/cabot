#
# Makefile for TRIPS SpeechLM code
#
# George Ferguson, ferguson@cs.rochester.edu,  2 Dec 1997
# $Id: Makefile,v 1.10 2015/10/20 21:57:23 lgalescu Exp $
#

CONFIGDIR = ../config
include $(CONFIGDIR)/defs.mk

DESTDIR = $(etcdir)/SpeechLM

SUBDIRS = salle calo boudreaux fruitcarts lou ralf plow cardiac savant plot obtw gps cabot

all default:
	@echo 'No need to make in SpeechLM.'

clean:
	@echo 'No need to clean in SpeechLM.'

install:
	$(MKINSTALLDIRS) $(DESTDIR)
	for d in $(SUBDIRS); do \
	  if test -d "$$d"; then \
	    $(MKINSTALLDIRS) $(DESTDIR)/$$d; \
	    $(INSTALL_DATA) $$d/*.lm $(DESTDIR)/$$d; \
	    $(INSTALL_DATA) $$d/*.lm.DMP $(DESTDIR)/$$d; \
	    $(INSTALL_DATA) $$d/*.dic $(DESTDIR)/$$d; \
	  fi; \
	  if test -f "$$d/$$d.wdmap"; then \
	    $(INSTALL_DATA) $$d/$$d"gen.lm" $(DESTDIR)/$$d; \
	  fi; \
	  if test -f "$$d/$$d.wdmap"; then \
            $(INSTALL_DATA) $$d/$$d.wdmap $(DESTDIR)/$$d; \
	  fi; \
	done

install.man::
	@echo 'No manpages for SpeechLM.'
