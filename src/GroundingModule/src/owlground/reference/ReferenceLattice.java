package owlground.reference;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.WorldManager;
import owlground.language.Demonstration;
import owlground.language.Utterance;
import owlground.language.Word;
import owlground.objects.AbstractDescriptor;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.objects.WorldObjectGroup;
import owlground.spaces.FeatureSpace;
import owlground.utilities.Statistics;

public strictfp class ReferenceLattice {

	List<AbstractDescriptor> descriptors;
	List<List<Partition>> lattice;
	List<List<LatticeEdge>> latticeEdges;
	List<Map<WorldObject,Double>> worldObjectProbabilities; // Distances to labels, aligned to descriptors list
	List<LatticePath> latticePaths;
	List<LatticePath> bestPaths;
	LatticeLearner latticeLearner;
	double averageVisualDistance;
	Set<WorldObject> targetSet;
	
	Demonstration demonstration;
	
	static boolean USE_PRAGMATICS = true;
	static boolean USE_GUESSING = false;
	static boolean USE_NEGATIVE_EXAMPLES = false;
	static boolean USE_NEGATIVE_EXAMPLES_WITH_GUESS = false;
	static boolean USE_ORDERED_PARTITIONS = false;
	static boolean USE_COMBINED_OBJECT_DISTANCE = false;
	static boolean USE_LABELED_DATA = false;
	
	static int MIN_UTTERANCES_FOR_LABEL = 1;
	static double OVERSPECIFICATION_PROBABILITY = .5;
	static double MIN_PARTITION_PROBABILITY = .0000000000001;
	static double MIN_PATH_PROBABILITY = .0000000000000001;
	static double MAX_PATHS_TO_GUESS = 100000000;
	
	private static final double PATH_EQUAL_EPSILON = .00000001;
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("reference.usepragmatics"))
			USE_PRAGMATICS = Boolean.parseBoolean(properties.get("reference.usepragmatics"));
		if (properties.containsKey("reference.useguessing"))
			USE_GUESSING = Boolean.parseBoolean(properties.get("reference.useguessing"));
		if (properties.containsKey("classifiers.usenegativeexamples"))
			USE_NEGATIVE_EXAMPLES = Boolean.parseBoolean(properties.get("classifiers.usenegativeexamples")); 
		if (properties.containsKey("classifiers.usenegativeexampleswithguess"))
			USE_NEGATIVE_EXAMPLES_WITH_GUESS = Boolean.parseBoolean(properties.get("classifiers.usenegativeexampleswithguess"));
		if (properties.containsKey("reference.useorderedpartitions"))
			USE_ORDERED_PARTITIONS = Boolean.parseBoolean(properties.get("reference.useorderedpartitions").toLowerCase()); 
		if (properties.containsKey("reference.usecombinedobjectdistance"))
			USE_COMBINED_OBJECT_DISTANCE = Boolean.parseBoolean(properties.get("reference.usecombinedobjectdistance"));
		if (properties.containsKey("reference.uselabeleddata"))
			USE_LABELED_DATA = Boolean.parseBoolean(properties.get("reference.uselabeleddata"));
		if (properties.containsKey("reference.reasoner.minutterancesforlabel"))
			MIN_UTTERANCES_FOR_LABEL = Integer.parseInt(properties.get("reference.reasoner.minutterancesforlabel").toLowerCase()); 
		if (properties.containsKey("reference.overspecificationprobability"))
			OVERSPECIFICATION_PROBABILITY = Double.parseDouble(properties.get("reference.overspecificationprobability").toLowerCase()); 
		if (properties.containsKey("reference.lattice.minpartitionprobability"))
			MIN_PARTITION_PROBABILITY = Double.parseDouble(properties.get("reference.lattice.minpartitionprobability").toLowerCase()); 
		if (properties.containsKey("reference.lattice.minpathprobability"))
			MIN_PATH_PROBABILITY = Double.parseDouble(properties.get("reference.lattice.minpathprobability").toLowerCase()); 
		if (properties.containsKey("reference.lattice.maxpathstoguess"))
			MAX_PATHS_TO_GUESS = Integer.parseInt(properties.get("reference.lattice.maxpathstoguess").toLowerCase()); 
		
	}
	
	
	public ReferenceLattice(Demonstration d)
	{
		LatticePath.setUsePragmatics(USE_PRAGMATICS);
		descriptors = new ArrayList<AbstractDescriptor>();
		lattice = new ArrayList<List<Partition>>();
		latticeEdges = new ArrayList<List<LatticeEdge>>();
		worldObjectProbabilities = new ArrayList<Map<WorldObject, Double>>();
		latticePaths = new ArrayList<LatticePath>();
		averageVisualDistance = 0;
		
		this.demonstration = d;
		addDescriptors();
		calculateWorldObjectProbabilities();
	}
	
	private void addDescriptors()
	{
		WorldObjectGroup focus = demonstration.getFocusWorldObjectGroup();
		WorldManager wm = demonstration.getUtterance().getSession().getWorldManager();
		for (String descriptor : focus.getUniversalProperties())
		{
			if (wm.getProperties().containsKey(descriptor))
				descriptors.add(wm.getProperties().get(descriptor));
		}
		
		descriptors.add(wm.getObjectClasses().get(focus.getClassName()));
		//System.out.println(descriptors.size() + " descriptors added");
	}
	
	public void construct()
	{
		generateLattice();
		generatePaths();
		prunePaths();
	}
	
	// Find the minimum distance to the descriptor for each of the WorldObjects
	public void calculateWorldObjectProbabilities()
	{
		worldObjectProbabilities.clear();
		for (AbstractDescriptor descriptor : descriptors)
		{
			System.out.println("Descriptor: " + descriptor);
			boolean descriptorUnknown = false;
			Map<WorldObject, Double> probabilities = new HashMap<WorldObject, Double>();

			descriptor.updatePerceptClusters();
			for (WorldObject wo : demonstration.getNonNullWorldObjects())
			{
				//System.out.println("World object");

				Map<FeatureSpace, DoubleMatrix> averageValues = wo.getAveragePerceptValues();
				//System.out.println(averageValues);
				//System.out.println("Representative feature map: ");
				//System.out.println(Property.getRepresentativeFeatureSpaceMap(wo.getSession()));
				
				FeatureSpace representativeFeature;
				if (descriptor instanceof ObjectClass)
				{
					representativeFeature = wo.getSession().getSpaceManager().getFeatureSpaceByName("zernike");
				}
				else if (!Property.getRepresentativeFeatureSpaceMap(wo.getSession()).containsKey(descriptor.getName()))
				{
					descriptorUnknown = true;
					break;
				}
				else
				{
					representativeFeature = Property.getRepresentativeFeatureSpaceMap(wo.getSession()).get(descriptor.getName());
				}
				//System.out.println("Representative feature: " + representativeFeature);
				
				//DoubleMatrix point = averageValues.get(representativeFeature);
				//System.out.println("Point to check: " + point);
				if (descriptor.getPerceptClusters().size() < 4 || 
						averageValues.get(representativeFeature) == null ||
						descriptor.getUtterances().size() < MIN_UTTERANCES_FOR_LABEL)
				{
					descriptorUnknown = true;
					break;
				}
				
				double distanceToDescriptor = Double.MAX_VALUE;
				if (USE_COMBINED_OBJECT_DISTANCE && descriptor instanceof ObjectClass)
				{
					List<Double> distances = descriptor.getKNearestMahalanobis(wo.getAveragePerceptValues(), 1);
					if (!distances.isEmpty())
						distanceToDescriptor = distances.get(0);
				}
				else
				{
					distanceToDescriptor = descriptor.getSquaredMahalanobisToMean(representativeFeature, averageValues.get(representativeFeature));
				}
				
				if (distanceToDescriptor == Double.MAX_VALUE)
				{
					descriptorUnknown = true;
					break;
				}
				//System.out.println("Distance to descriptor: " + distanceToDescriptor);
				double probability = Statistics.mahalanobisToProbability(distanceToDescriptor, 
						representativeFeature.getDimension());

				if (probability < .0000000000001)
				{
					descriptorUnknown = true;
					break;
				}
				
				probabilities.put(wo, probability);
				
			}
			if (descriptorUnknown)
				worldObjectProbabilities.add(null);
			else
			{
				//System.out.println("Object probabilities set");
				worldObjectProbabilities.add(probabilities);
			}
		}
	}
	
	public void updateEdges()
	{
		int columnIndex = 0;
		for (List<LatticeEdge> edgeSet : latticeEdges)
		{
			for (LatticeEdge edge : edgeSet)
			{
				if (USE_PRAGMATICS)
				{
					double edgeOverspecifiedOdds = latticeLearner.getLogRatioOfOverspecificationGivenFeatures(
							descriptors.get(columnIndex), 
							edge.getFrom().getPositiveExamples().size() + 
								edge.getFrom().getNegativeExamples().size(), 
							1, averageVisualDistance);
					
					double edgeOverspecifiedProbability = edgeOverspecifiedOdds / 
														(1 + edgeOverspecifiedOdds);

					if (edge.isOverSpecified())
						edge.setProbability(OVERSPECIFICATION_PROBABILITY);
					else
						edge.setProbability(1 - OVERSPECIFICATION_PROBABILITY);
					
				}			//System.out.println(edgeSet.size() + " edges for this transition");
			}
			
			columnIndex++;
		}
	}
	
	public void generateLattice() {
		System.out.println(descriptors.size() + " descriptors");
		for (int i = 0; i < descriptors.size(); i++)
		{
			PartitionGenerator pg = new PartitionGenerator();
			boolean isFinalPartitionSet = (i == descriptors.size() - 1);
			if (worldObjectProbabilities.get(i) == null || USE_ORDERED_PARTITIONS == false)
				lattice.add(pg.generateExhaustivePartitions(demonstration.getFocusWorldObjectGroup(),
															demonstration.getNonNullWorldObjects(), 
															isFinalPartitionSet));
			else
				lattice.add(pg.generateOrderedPartitions(demonstration.getNonNullWorldObjects(),
						descriptors.get(i), worldObjectProbabilities.get(i), isFinalPartitionSet));
			averageVisualDistance = pg.getAverageVisualDistance();
			if (i > 0)
			{
				// Add connecting edges if they do not lead to an empty intersective set
				latticeEdges.add(new ArrayList<LatticeEdge>());
				for (Partition from : lattice.get(i-1))
				{
					for (Partition to : lattice.get(i))
					{
						LatticeEdge edge = new LatticeEdge(from, to);
							
						if (!edge.emptyIntersection() || USE_PRAGMATICS == false)
						{
							latticeEdges.get(i-1).add(edge);
							from.addOutgoingEdge(edge);
						}
					}
				}
			}
			
			updateLabeledProbabilities(i);

		}
		
		normalizePartitionProbabilities();
		
	}
	
	private void pruneDeadPaths()
	{

		Iterator<LatticePath> pathIterator = latticePaths.iterator();
		while (pathIterator.hasNext())
		{
			if (pathIterator.next().getProbability() < 
					MIN_PATH_PROBABILITY)
				pathIterator.remove();
		}

	}
	
	public void updateLabeledProbabilities(int descriptorIndex)
	{
		for (List<Partition> partitionSet : lattice)
		{
			for (Partition p : partitionSet)
			{
				if (USE_LABELED_DATA == false)
				{
					p.setLabeledProbability(1.0);
				}
				else if (worldObjectProbabilities.get(descriptorIndex) != null)
					p.setLabeledProbability(worldObjectProbabilities.get(descriptorIndex));

//				System.out.println("Column " + i);
//				System.out.println("Partition with " + p.getPositiveExamples().size() + " pos examples");
//				System.out.println("Partition probability: " + p.getUnlabeledProbability());
//				System.out.println("Label probability: " + p.getLabeledProbability());
			}
		}		
	}
	
	private void normalizeEdgeProbabilities()
	{
		for (List<Partition> partitionSet : lattice)
		{
			for (Partition p : partitionSet)
			{
				double edgeSum = 0;
				
				for (LatticeEdge edge : p.getOutgoingEdges())
				{
					edgeSum += edge.getProbability();
				}
				
				double normalization = 1.0 / edgeSum;
				for (LatticeEdge edge : p.getOutgoingEdges())
					edge.setProbability(edge.getProbability() * normalization);
			}
		}
	}
	
	private void normalizePartitionProbabilities()
	{
		for (List<Partition> partitionSet : lattice)
		{
			double sum = 0.0;
			
			for (Partition p : partitionSet)
			{
				//System.out.println("Partition prob: " + p.getTotalProbability());
				sum += p.getTotalProbability();
				
			}
			
			//System.out.println("Normalizing constant " + sum);
			for (Partition p : partitionSet)
			{
				p.setNormalizedProbability(p.getTotalProbability() / sum);
				//System.out.println("Old value: " + p.getTotalProbability());
				//System.out.println("New value: " + p.getNormalizedProbability());
			}
		}
	}
	
	public void generatePaths()
	{
		// Single descriptor
		if (lattice.size() == 1)
		{
			for (Partition partition : lattice.get(0))
			{
				latticePaths.add(new LatticePath(partition));
			}
		}
		else
		{
			for (Partition partition: lattice.get(0))
			{
				generatePathsRecursive(partition, new LatticePath());
			}
		}
	}
	
	public void prunePartitions()
	{
		for (List<Partition> partitions : lattice)
		{
			Iterator<Partition> partitionIterator = partitions.iterator();
			while (partitionIterator.hasNext())
			{
				if (partitionIterator.next().getTotalProbability() < 
						MIN_PARTITION_PROBABILITY)
					partitionIterator.remove();
			}
		}
	}
	
	public void prunePaths()
	{
		//normalizeEdgeProbabilities();
		if (USE_PRAGMATICS == false)
			return;
		Iterator<LatticePath> pathIterator = latticePaths.iterator();
		while (pathIterator.hasNext())
		{
			LatticePath path = pathIterator.next();
			if (!demonstration.getFocusWorldObjectGroup().compatibleSet(path.getResult()) || 
					path.getEdges().size() < lattice.size() - 1)
			{
				pathIterator.remove();
			}
			else if (targetSet != null)
			{
				int matches = 0;
				boolean mismatch = false;
				

				for (WorldObject wo : path.getResult())
				{
					if (targetSet.contains(wo))
						matches++;
					else
					{
						mismatch = true;
						break;
					}
				}
				
				if (mismatch || matches < targetSet.size())
				{
					pathIterator.remove();
					continue;
				}
			}
			else
			{
				//System.out.println("Path distance: " + path.getProbability());
			}
		}
		
		System.out.println(latticePaths.size() + " paths left");
	}
	
	public TreeMap<Double,List<LatticePath>> getPathProbabilities()
	{
		TreeMap<Double,List<LatticePath>> probabilities = new TreeMap<Double, List<LatticePath>>();
		double totalProbability = 0;
		
		for (LatticePath latticePath : latticePaths)
		{
			double probability = latticePath.getProbability();
			if (!probabilities.containsKey(probability))
				probabilities.put(probability, new ArrayList<LatticePath>());
			
			probabilities.get(probability).add(latticePath);
			totalProbability += probability;
		}
		//System.out.println("Total path probability: " + totalProbability);
		
		return probabilities;
	}
	
	/**
	 * Return the set of LatticePaths that have the highest probability, within some epsilon value.
	 * This avoids problems where there are insignificant differences in the probability values.
	 * @return A set of the best paths
	 */
	public List<LatticePath> getBestPaths(double epsilon)
	{
		Map<Double,List<LatticePath>> probabilities = getPathProbabilities().descendingMap();
		
		List<LatticePath> bestPaths = new ArrayList<LatticePath>();
		double lastValue = -1;
		// Check the top two entries to see if they're close enough to lump together as best paths
		for(Entry<Double,List<LatticePath>> entry : probabilities.entrySet())
		{
			if (lastValue < 0)
			{
				bestPaths.addAll(entry.getValue());
				lastValue = entry.getKey();
				continue;
			}
			else if (lastValue - entry.getKey() < epsilon)
				bestPaths.addAll(entry.getValue());

			break;
		}
		this.bestPaths = bestPaths;
		
		return bestPaths;
	}
	
	/**
	 * Return the set of LatticePaths that have the highest probability, within some epsilon value.
	 * This avoids problems where there are insignificant differences in the probability values.
	 * @return A set of the best paths
	 */
	public List<LatticePath> getBestPaths()
	{
		return getBestPaths(PATH_EQUAL_EPSILON);
	}
	
	public boolean resolve(boolean reResolution, boolean lastGuess)
	{
		//System.out.println("Resolving utterance: " + demonstration.getUtterance().hashCode());
		//if (bestPaths != null)
		//	System.out.println("Old number of best paths: " + bestPaths.size());
		
		pruneDeadPaths();
		
		if (reResolution)
		{
			calculateWorldObjectProbabilities();
			for (int i = 0; i < descriptors.size(); i++)
				updateLabeledProbabilities(i);
		}
		
		normalizePartitionProbabilities();
		updateEdges();
		List<LatticePath> bestPaths = getBestPaths(PATH_EQUAL_EPSILON);

		dumpPaths();
		//System.out.println("New number of best paths: " + bestPaths.size());
		
		// Just make a guess and go with it
		if (bestPaths.size() == 0 || bestPaths.size() > 1)
		{
			if (lastGuess || (USE_GUESSING && bestPaths.size() <= MAX_PATHS_TO_GUESS))
			{
				int pathNumber = (int)(Math.random() * (bestPaths.size()-1));
				
				int i = 0;
				
				for (LatticePath path : bestPaths)
				{
					if (i < pathNumber)
						 continue;
					processPath(path,true);
					break;
				}
			}
			return false;
		}

		// Process the best path (there will only be one at this point)
		for (LatticePath path : bestPaths)
		{
			processPath(path, false);
		}
		
		return true;
	}
	
	public boolean processPath(LatticePath path, boolean guess)
	{
		int columnIndex = 0;
		if (path == null)
		{
			System.out.println("Null path");
			return false;
			
		}
			
		List<Partition> partitionList = path.getPartitionList(); 
		// Clears previous properties, fixes guesses
		for (WorldObject wo : demonstration.getNonNullWorldObjects())
		{
			wo.setFocus(false);
			//Note this means an object can't be updated across multiple demonstrations
			wo.getProperties().clear(); 
		}
		
		demonstration.getFocusWorldObjectGroup().clearWorldObjects();
		
		//System.out.println("Path result: ");
		// Update the WorldObjects to show we believe them to be the focus
		for (WorldObject wo : path.getResult())
		{
			//System.out.println("Object at: " + wo.getCurrentBlobChain().getAverageX());
			//System.out.println("In utterances: ");
			for (Utterance u : wo.getUtterances())
			{
				System.out.println(u.hashCode());
			}
			wo.setFocus(true);
			demonstration.getFocusWorldObjectGroup().addWorldObject(wo);
		}
		
		// Update the properties of the objects in each partition along the path
		for (Partition partition : partitionList)
		{
			AbstractDescriptor descriptor = descriptors.get(columnIndex);
			if (descriptor instanceof Property)
			{
				for (WorldObject wo : partition.getPositiveExamples())
				{
					wo.addProperty((Property)descriptor);
					
				}
				//NO-NEGATIVE TEST
				if (USE_NEGATIVE_EXAMPLES && (!guess || USE_NEGATIVE_EXAMPLES_WITH_GUESS))
				{
					for (WorldObject wo : partition.getNegativeExamples())
					{
						wo.addNegatedProperty((Property)descriptor);
					}
				}
			}
			else
			{
				for (WorldObject wo : partition.getPositiveExamples())
				{
					//System.out.println("Setting class name for object at " + 
					//				wo.getCurrentBlobChain().getAverageX() + 
					//				"  to: " + descriptor.getName());
					wo.setObjectClass((ObjectClass)descriptor);
				}
				//NO-NEGATIVE TEST
				if (USE_NEGATIVE_EXAMPLES)
				{
					for (WorldObject wo : partition.getNegativeExamples())
					{
						//System.out.println("Original class before setting to default: " + wo.getClassName());
						wo.setClassName(ObjectClass.defaultClass);
						wo.addNegatedClass((ObjectClass)descriptor);
					}
				}
			}
			columnIndex++;
		}
		//System.out.println("Adding feature data for " + 
		//demonstration.getNonNullWorldObjects() + " objects to world manager");
		for (WorldObject wo : demonstration.getNonNullWorldObjects())
		{
			wo.addFeatureDataToWorldManager();
		}
		

		latticeLearner.addPath(path, descriptors, averageVisualDistance);
		
		return true;
		
	}
	
	private void generatePathsRecursive(Partition nextSource, LatticePath pastPath)
	{
		Collection<LatticeEdge> nextEdges = nextSource.getOutgoingEdges();
		if (nextEdges.size() == 0)
			latticePaths.add(pastPath);
		
		for (LatticeEdge nextEdge : nextEdges)
		{
			LatticePath nextPath = new LatticePath(pastPath,nextEdge);
			if (nextPath.getResult().size() > 0 || USE_PRAGMATICS == false)
				generatePathsRecursive(nextEdge.getTo(), new LatticePath(pastPath,nextEdge));
		}
	}

	public List<AbstractDescriptor> getDescriptors() {
		return descriptors;
	}

	public LatticeLearner getLatticeLearner() {
		return latticeLearner;
	}

	public void setLatticeLearner(LatticeLearner latticeLearner) {
		this.latticeLearner = latticeLearner;
	}

	public Set<WorldObject> getTargetSet() {
		return targetSet;
	}

	public void setTargetSet(Set<WorldObject> targetSet) {
		this.targetSet = targetSet;
	}
	
	public void dumpPaths()
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(new FileOutputStream(
				    new File("lattices"), 
				    true /* append = true */));
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for Word debug.");
			e.printStackTrace();
			return;
		}

		for (LatticePath latticePath : latticePaths)
		{
			outputFile.println(latticePath.toString());
		}
		
		outputFile.close();

	}
	
}
