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
	src/utilities/FormatConversion.java \
	src/messages/NetworkConfiguration.java \
	src/messages/CommDataReader.java \
	src/messages/BlockMessageSender.java \
	src/spatialreasoning/Plan.java \
	src/spatialreasoning/Step.java \
	src/environment/StructureInstance.java \
	src/models/ModelBuilder.java \
	src/models/StructureModel.java \
	src/models/FeatureInference.java \
	src/features/Feature.java \
	src/features/FeatureGroup.java \
	src/features/BlockFeatureGroup.java \
	src/features/CountFeature.java \
	src/features/TemporalSequenceFeature.java \
	src/features/DirectionFeature.java \
	src/models/FeatureProjection.java \
	src/messages/CommDataSender.java \
	src/goals/Goal.java \
	src/goals/GoalStateHandler.java \
	src/messages/EvaluateHandler.java \
	

CLASSES = $(SRCS:.java=.class) \
	src/goals/Goal$$GoalType.class \
	src/goals/GoalStateHandler$$GoalState.class \
	src/goals/GoalStateHandler$$SystemState.class \

CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
# HACK to work around stuff in ./src/ being in the wrong place in TRIPS.SRIWrapper.jar
EXTRA_CLASSPATH = $(prefix)/src/SRIWrapper/src
include $(CONFIGDIR)/java/prog.mk

install::
	$(INSTALL_DATA) json-simple-1.1.1.jar jblas-1.2.3.jar $(etcdir)/java

