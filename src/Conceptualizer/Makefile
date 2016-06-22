#
# File: Makefile for Conceptualizer
# Creator: Ian Perera
#

MAIN    = Conceptualizer
PACKAGE = TRIPS.Conceptualizer
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util json-simple-1.1.1 jblas-1.2.3 

JFLAGS	= -cp .:src:json-simple-1.1.1.jar:jblas-1.2.3.jar

SRCS = 	Conceptualizer.java

	

CLASSES = $(SRCS:.java=.class) 

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
# HACK to work around stuff in ./src/ being in the wrong place in TRIPS. Conceptualizer.jar
EXTRA_CLASSPATH = $(prefix)/src/Conceptualizer/src
include $(CONFIGDIR)/java/prog.mk

install::
	$(INSTALL_DATA) json-simple-1.1.1.jar jblas-1.2.3.jar $(etcdir)/java

