#
# File: Makefile for GroundingModule
# Creator: George Ferguson
# Created: Thu Jul 19 15:26:56 2012
# Time-stamp: <Thu Jul 19 15:27:49 EDT 2012 ferguson>
#

MAIN    = GroundingModule
PACKAGE = TRIPS.GroundingModule
USES    = TRIPS.TripsModule TRIPS.KQML TRIPS.util jblas-1.2.3 commons-math3-3.2edit
JFLAGS	= -cp .:src:jblas-1.2.3.jar:commons-math3-3.2edit.jar

SRCS =	src/owlground/WorldManager.java \
	src/owlground/language/Query.java \
	src/owlground/SocketFeatureManager.java \
	GroundingModule.java \
	src/owlground/objects/Property.java \
	src/owlground/regions/DataRegion.java \
	src/owlground/objects/WorldObject.java \
	src/owlground/spaces/SpaceManager.java \
	src/owlground/regions/PrimitiveRegion.java \
	src/owlground/regions/BallRegion.java \
	src/owlground/spaces/EuclideanSpace.java \
	src/owlground/perception/PerceptCluster.java \
	src/owlground/perception/Blob.java \
	src/owlground/regions/BoxRegion.java \
	src/owlground/FeatureManager.java \
	src/owlground/GroundedReasoner.java \
	src/owlground/objects/WorldObjectGroup.java \
	src/owlground/objects/AbstractDescriptor.java \
	src/owlground/objects/ObjectClass.java \
	src/owlground/perception/BlobGroup.java \
	src/owlground/perception/BlobGroupChain.java \
	src/owlground/perception/BlobChain.java \
	src/owlground/language/Demonstration.java \
	src/owlground/utilities/Conversions.java \
	src/owlground/language/Utterance.java \
	src/owlground/language/Word.java \
	src/owlground/Classifiers.java \
	src/owlground/objects/ObjectModel.java \
	src/owlground/utilities/Permute.java \
	src/owlground/regions/PerceptualDataRegion.java \
	src/owlground/utilities/AssociationFileCreator.java \
	src/owlground/utilities/Utilities.java \
	src/owlground/RepresentativeFeatureSelection.java \
	src/owlground/utilities/Debug.java \
	src/owlground/spaces/FeatureSpace.java \
	src/owlground/regions/KNearestRegion.java \
	src/owlground/regions/KNearestMalahanobisRegion.java \
	src/owlground/perception/Percept.java \
	src/owlground/Manifold.java \
	src/owlground/Evaluation.java \
	src/owlground/geometry/VidaExpertFileCreator.java \
	src/owlground/reference/Partition.java \
	src/owlground/reference/LatticeEdge.java \
	src/owlground/reference/PartitionGenerator.java \
	src/owlground/reference/LatticePath.java \
	src/owlground/reference/ReferenceLattice.java \
	src/owlground/reference/ReferenceReasoner.java \
	src/owlground/reference/SuperPartition.java \
	src/owlground/utilities/QuickSelect.java \
	src/owlground/reference/ReferenceTester.java \
	src/owlground/reference/LatticeLearner.java \
	src/owlground/reference/TUNAReader.java \
	src/owlground/reference/TUNATester.java \
	src/owlground/language/dialogue/Dialogue.java \
	src/owlground/language/dialogue/LFTerm.java \
	src/owlground/language/dialogue/PSState.java \
	src/owlground/language/dialogue/TRIPSProcessor.java \
	src/owlground/language/dialogue/Turn.java \
	src/owlground/language/Predicate.java \

# add these back in when they're actually used...
#JFLAGS 	= -cp .:weka.jar:lib.jar:owlapi-bin.jar
#SRCS = 	src/VisualizeClusterAssignments.java \
	src/com/clarkparsia/pellet/datatypes/types/text/RestrictedSpace.java \
	src/com/clarkparsia/pellet/datatypes/types/text/Space.java \
	src/com/clarkparsia/pellet/datatypes/DatatypeReasonerImpl.java \
	src/org/code/owlapi/examples/LoadingOntologies.java \
	src/backEnd/OWLBackend.java \
	src/backEnd/Main.java \


CLASSES = $(SRCS:.java=.class) \
	src/owlground/language/Query$$1.class \
	src/owlground/language/Query$$QueryType.class \
	src/owlground/language/Demonstration$$DemonstrationType.class \
	src/owlground/language/Demonstration$$ImplicationType.class \
	src/owlground/objects/Property$$1.class \
	src/owlground/objects/ObjectClass$$1.class \
	src/owlground/reference/ReferenceTester$$PositionComparator.class \


CONFIGDIR=../config
include $(CONFIGDIR)/defs.mk
# HACK to work around stuff in ./src/ being in the wrong place in TRIPS.GroundingModule.jar
EXTRA_CLASSPATH = \
	$(prefix)/src/GroundingModule/src
include $(CONFIGDIR)/java/prog.mk

install::
	$(INSTALL_DATA) jblas-1.2.3.jar commons-math3-3.2edit.jar $(etcdir)/java

