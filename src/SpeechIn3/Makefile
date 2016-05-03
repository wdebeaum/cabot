#
# Makefile for SpeechIn
#
# George Ferguson, ferguson@cs.rochester.edu, 25 Sep 2002
# $Id: Makefile,v 1.4 2007/12/07 18:20:21 ferguson Exp $
#

MODULE = SpeechIn

WRAPPER = speechin
PROG = sphinx
SRCS = analyze.c \
	bcast.c \
	input.c \
	log.c \
	main.c \
	recv.c \
	send.c \
	sphinx.c \
	utt.c \
	wordbuf.c \
	live.c \
	parse_args_file.c
OBJS = $(SRCS:.c=.o)

TRIPS_CFLAGS = -I..
# gf: 12/7/2007: Mac OSX Leopard has apparently broken the -L/-l incantation
#TRIPS_LIBS = -L../trlib -ltrlib -L../KQML -lKQML -L../util -lutil
TRIPS_LIBS = ../trlib/libtrlib.a ../KQML/libKQML.a ../util/libutil.a

SPHINX_BASE = ../Sphinx3

# Function for extracting setting from config.status
CHKS2CONFIG = $(shell sed -n 's/^s,@$(1)@,\(.*\),;.*/\1/p' $(SPHINX_BASE)/config.status)
# Function for extracting setting from $(SPHINX_BASE)/Makefile
CHKS3MAKE = $(shell sed -n 's/^$(1) = \(.*\)/\1/p' $(SPHINX_BASE)/Makefile)

# See if we're using PortAudio for Sphinx
AD_BACKEND := $(call CHKS3MAKE,ad_backend)
ifeq ($(AD_BACKEND),AD_BACKEND_PA)
# If so, pickup CFLAGS and LIBS
PORTAUDIO_CFLAGS := $(call CHKS3MAKE,PORTAUDIO_CFLAGS)
PORTAUDIO_LIBS   := $(call CHKS3MAKE,PORTAUDIO_LIBS)
endif

SPHINX_CFLAGS = -I$(SPHINX_BASE)/src \
		-I$(SPHINX_BASE)/src/libs3audio \
		-I$(SPHINX_BASE)/src/libs3decoder \
		-I$(SPHINX_BASE)/src/libutil \
		-I$(SPHINX_BASE)/include \
		$(PORTAUDIO_CFLAGS)
SPHINX_LIBS = $(SPHINX_BASE)/src/libs3audio/.libs/libs3audio.a \
	      $(SPHINX_BASE)/src/libs3decoder/.libs/libs3decoder.a \
	      $(SPHINX_BASE)/src/libutil/.libs/libs3util.a \
	      $(PORTAUDIO_LIBS) -lm
EXTRA_CFLAGS = $(TRIPS_CFLAGS) $(SPHINX_CFLAGS)
EXTRA_LIBS = $(TRIPS_LIBS) $(SPHINX_LIBS)

CONFIGDIR = ../config
include $(CONFIGDIR)/c/prog.mk

all:: $(WRAPPER)

$(WRAPPER): $(WRAPPER).sh
	sed -e 's@TRIPS_BASE_DEFAULT=.*$$@TRIPS_BASE_DEFAULT=$(prefix)@' \
	  $< >$@
	chmod a+x $@

ACOUSTIC_MODEL_DIR = $(SPHINX_BASE)/model/hmm/hub4_cd_continuous_8gau_1s_c_d_dd
standard = $(shell test -d $(ACOUSTIC_MODEL_DIR) && echo yes)
ifneq ($(standard),yes)
ACOUSTIC_MODEL_DIR = ../SpeechModels
endif

install:: $(PROG) $(WRAPPER)
	$(MKINSTALLDIRS) $(bindir)
	$(INSTALL_PROGRAM) $(WRAPPER) $(bindir)
	# Use a link instead of copying all the speech datafiles
	$(MKINSTALLDIRS) $(etcdir)
	-rm -f $(etcdir)/SpeechModels
	-ln -s `cd $(ACOUSTIC_MODEL_DIR) && pwd` $(etcdir)/SpeechModels
	# Utilities for listening to sphinx audio files (using sox)
	$(INSTALL_PROGRAM) playraw.sh $(bindir)/playraw
	$(INSTALL_PROGRAM) raw2aiff.sh $(bindir)/raw2aiff

clean::
	rm -f $(WRAPPER)

