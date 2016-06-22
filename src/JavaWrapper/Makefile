#
# File: Makefile for JavaWrapper
# Creator: George Ferguson
# Created: Thu Jul 19 15:26:56 2012
# Time-stamp: <Thu Jul 19 15:27:49 EDT 2012 ferguson>
#

CONFIGDIR=../config
#include $(CONFIGDIR)/java/prog.mk
include $(CONFIGDIR)/TextTagger/defs.mk

MAIN    = JavaWrapper
PACKAGE = TRIPS.JavaWrapper
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util edu.stanford.nlp
JFLAGS	= -cp .:src:$(CORENLP_CLASSPATH)

EXTRA_JAVA_FLAGS = $(CORENLP_JAVA_FLAGS)
$(info JFLAGS is $(JFLAGS))
SRCS = 	src/javawrapper/Turn.java \
	src/javawrapper/Dialogue.java \
	src/javawrapper/DialogueIndexer.java \
	src/javawrapper/Utilities.java \
	src/javawrapper/WordNetMapper.java \
	src/javawrapper/LFTerm.java \
	src/javawrapper/CoreNLPStitcher.java \
	src/corenlpwrapper/CoreNLPXmlProcessor.java \
	src/corenlpwrapper/CoreNLPSentence.java \
	src/corenlpwrapper/CoreNLPToken.java \
	src/corenlpwrapper/SemanticGraphConverter.java \
	JavaWrapper.java \
	

CLASSES = $(SRCS:.java=.class) \
	src/javawrapper/Turn$$Word.class \
	JavaWrapper$$TimeoutTask.class \
	JavaWrapper$$1.class \
	src/javawrapper/LFTerm$$1.class \



# HACK to work around stuff in ./src/ being in the wrong place in TRIPS.GroundingModule.jar
EXTRA_CLASSPATH = \
	$(prefix)/src/JavaWrapper/src:$(CORENLP_CLASSPATH):$(etcdir)/java/

JFLAGS = -cp .:src:$(EXTRA_CLASSPATH)


include $(CONFIGDIR)/java/prog.mk

clean::
	rm -f ./$(CLASSES)
