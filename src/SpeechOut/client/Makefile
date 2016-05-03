#
# Makefile for TRIPS TrueTalk client
#
# George Ferguson, ferguson@cs.rochester.edu, 8 Jan 1996
# Time-stamp: <Mon Mar 26 17:29:17 EST 2001 ferguson>
#

MODULE = SpeechOut

PROG = ttc
SRCS = main.c input.c recv.c send.c truetalk.c audio.c audio_af.c audio_sun.c
HDRS = $(SRCS:.c=.h)
MANS = $(PROG).man tts.man tttalk.man
XTRA = tts.sh tttalk.pl

TTS_INCS = -I$(TT_BASE)/h
TTS_LIBS = -L$(TT_BASE)/lib -lttd -lwavesynt -lsolaris

CDEBUGFLAGS = -O
EXTRA_DEFS = -DDEBUG -I../.. -I..
EXTRA_LIBS = $(AFLIBS) $(TTS_LIBS) $(TRIPSLIBS:../%=../../%)
EXTRA_DEPS = $(TRIPSLIBS:../%=../../%)

EXTRA_DEFS_truetalk.c = $(TTS_INCS)
EXTRA_DEFS_audio_af.c = $(AFDEFS)

CONFIGDIR = ../../config
include $(CONFIGDIR)/rules/cprog.mk

all: $(PROG)
