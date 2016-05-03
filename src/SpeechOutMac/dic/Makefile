#
# Makefile
#
# Time-stamp: <Tue May 10 12:14:18 CDT 2005 lgalescu>
# Author: Lucian Galescu <lgalescu@ihmc.us>, 10 May 2005
#

SUBDIRS = plow

CONFIGDIR = ../../config
include $(CONFIGDIR)/defs.mk

DICDIR = $(etcdir)/PronDic

install::
	$(MKINSTALLDIRS) $(DICDIR)
	for d in $(SUBDIRS); do \
	  if test -d "$$d"; then \
	    $(MKINSTALLDIRS) $(DICDIR)/$$d; \
	    $(INSTALL_DATA) $$d/$$d.dic $(DICDIR)/$$d; \
	  fi; \
	done
