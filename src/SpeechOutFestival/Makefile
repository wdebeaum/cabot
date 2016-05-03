#
# Makefile for SpeechOutFestival
#
# George Ferguson, ferguson@cs.rochester.edu, 9 Oct 2003
# Time-stamp: <Wed Feb 21 16:25:07 EST 2007 ferguson>
#

MODULE = SpeechOutFestival

PROG = SpeechOutFestival
SRCS = main.cpp
CSRCS = input.c \
	recv.c \
	send.c
HDRS =  $(CSRCS:.c=.h)
MANS = $(PROG).man
XTRA = Makefile

CONFIGDIR = ../config
include $(CONFIGDIR)/c++/prog.mk
include $(CONFIGDIR)/festival/defs.mk

# Festival needs older gcc; see README-gf.txt
CXX = g++32

EXTRA_CXXFLAGS = -I.. $(FESTIVAL_CXXFLAGS)
EXTRA_CFLAGS = -I..
TRIPS_LIBS = -L../trlib -ltrlib -L../KQML -lKQML -L../util -lutil

EXTRA_LDFLAGS =
EXTRA_LIBS = $(FESTIVAL_LIBS) $(TRIPS_LIBS)
EXTRA_DEPS =

# Some parts of this program are C...
include $(CONFIGDIR)/c/defs.mk
.c.o:
	$(CC) -c $(CFLAGS) $(EXTRA_CFLAGS_$<) $<

OBJS += $(CSRCS:.c=.o)
$(PROG): $(OBJS)

