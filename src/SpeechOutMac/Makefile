#
# Makefile for TRIPS MacTTS client
#
# George Ferguson, ferguson@cs.rochester.edu, 21 Sep 2002
# Time-stamp: <Tue Jan 12 15:40:50 EST 2010 ferguson>
#

MODULE = SpeechOutMac

SUBDIRS = dic

PROG = SpeechOutMac
SRCS = main.c input.c recv.c send.c tts.c pronunciation.c
OBJS = $(SRCS:.c=.o)
HDRS = $(SRCS:.c=.h)
MANS = $(PROG).man
XTRA =

EXTRA_CFLAGS = -I.. -DDEBUG
# gf: 12/7/2007: Mac OSX Leopard has apparently broken the -L/-l incantation
EXTRA_LIBS = -framework ApplicationServices \
		../trlib/libtrlib.a ../KQML/libKQML.a ../util/libutil.a
#		-L../trlib -ltrlib -L../KQML -lKQML -L../util -lutil
EXTRA_DEPS =

CONFIGDIR = ../config
include $(CONFIGDIR)/c/prog.mk

install::
	for d in $(SUBDIRS); do \
	  (cd $$d && $(MAKE) install) \
	done

mactts: mactts.c
	${CC} -framework ApplicationServices -o $@ $<


listvoices: listvoices.c
	${CC} -framework ApplicationServices -o $@ $<

pron: pronunciation.c
	${CC} -DTESTING -framework ApplicationServices -o $@ $^

# Cocoa app version of speech-out client
.PHONY: app
app:
	cd app; xcodebuild

runapp:
	./app/build/Release/SpeechOutMac.app/Contents/MacOS/SpeechOutMac
