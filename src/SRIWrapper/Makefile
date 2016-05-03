#
# File: Makefile for SRIWrapper
# Creator: Ian Perera
#

MAIN    = SRIWrapper
PACKAGE = TRIPS.SRIWrapper
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util json-simple-1.1.1 jblas-1.2.3 

JFLAGS	= -cp .:src:json-simple-1.1.1.jar:jblas-1.2.3.jar

SRCS = 	SRIWrapper.java \
	src/utilities/JsonReader.java \
	src/environment/Block.java \
	src/environment/Scene.java \
	src/messages/BlockMessagePuller.java \
	src/messages/BlockMessageReader.java \
	src/spatialreasoning/PredicateType.java \
	src/spatialreasoning/Predicate.java \
	src/utilities/TextToSpeech.java \
	src/messages/NetworkConfiguration.java \
	src/messages/CommDataReader.java \
	src/spatialreasoning/Plan.java \
	src/spatialreasoning/Step.java \

	

CLASSES = $(SRCS:.java=.class) \
	src/spatialreasoning/Predicate$$1.class \

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
# HACK to work around stuff in ./src/ being in the wrong place in TRIPS.SRIWrapper.jar
EXTRA_CLASSPATH = $(prefix)/src/SRIWrapper/src
include $(CONFIGDIR)/java/prog.mk

install::
	$(INSTALL_DATA) json-simple-1.1.1.jar jblas-1.2.3.jar $(etcdir)/java

