package owlground.reference;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.math3.distribution.NormalDistribution;
import org.apache.commons.math3.random.EmpiricalDistribution;
import org.apache.commons.math3.stat.descriptive.SummaryStatistics;
import org.apache.commons.math3.stat.regression.SimpleRegression;

import owlground.objects.AbstractDescriptor;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.spaces.FeatureSpace;


// Learn features to predict when an utterance is overspecified
// argmax P(Overspecification | Features) = argmax P(F_1, F_2,...|O) P(O)
// = argmax P(F_1|O)P(F_2|O)...P(O)
// P(F|O) = P(F & O) / P(O) 
//
// Features include:
// Number of objects
// Number of objects referred to
// Type of 
public class LatticeLearner {

	public int totalDemonstrations;
	private int totalOverspecifiedDescriptorOccurrences;
	private int totalNotOverspecifiedDescriptorOccurrences;
	private int totalPathEdges;
	public int overspecifiedDemonstrations;
	public int overspecifiedEdges;
	public NormalDistribution numberOfObjectsGivenOverspecified;
	public NormalDistribution numberOfObjectsGivenNotOverspecified;
	public NormalDistribution numberOfReferredObjectsGivenOverspecified;
	public NormalDistribution numberOfReferredObjectsGivenNotOverspecified;
	public NormalDistribution visualDistanceGivenOverspecified;
	public NormalDistribution visualDistanceGivenNotOverspecified;
	private ArrayList<LatticePath> paths;
	private ArrayList<Integer> numberOfObjectsOverspecifiedDataPoints;
	private ArrayList<Integer> numberOfReferredObjectsOverspecifiedDataPoints;
	private ArrayList<Double> visualDistancesOverspecifiedDataPoints;
	private HashMap<String,Integer> descriptorTypesOverspecifiedFrequencyTable;
	private ArrayList<Integer> numberOfObjectsNotOverspecifiedDataPoints;
	private ArrayList<Integer> numberOfReferredObjectsNotOverspecifiedDataPoints;
	private ArrayList<Double> visualDistancesNotOverspecifiedDataPoints;
	private HashMap<String,Integer> descriptorTypesNotOverspecifiedFrequencyTable;
	private boolean TUNAMode = true;
	
	private static final boolean FIXED_GAUSSIANS = false;
	
	private static double INITIAL_OVERSPECIFICATION_PROBABILITY = .7;
	
	public LatticeLearner() {
		
		numberOfObjectsOverspecifiedDataPoints = new ArrayList<Integer>();
		numberOfReferredObjectsOverspecifiedDataPoints = new ArrayList<Integer>();
		visualDistancesOverspecifiedDataPoints = new ArrayList<Double>();
		descriptorTypesOverspecifiedFrequencyTable = new HashMap<String, Integer>();
		numberOfObjectsNotOverspecifiedDataPoints = new ArrayList<Integer>();
		numberOfReferredObjectsNotOverspecifiedDataPoints = new ArrayList<Integer>();
		visualDistancesNotOverspecifiedDataPoints = new ArrayList<Double>();
		descriptorTypesNotOverspecifiedFrequencyTable = new HashMap<String, Integer>();
		totalPathEdges = 0;
		overspecifiedDemonstrations = 0;
		paths = new ArrayList<LatticePath>();
		
		if (FIXED_GAUSSIANS)
			recalculate();
		
	}
	
	private void addDescriptorTypeToOverspecifiedTable(String descriptorType)
	{
		if (!descriptorTypesOverspecifiedFrequencyTable.containsKey(descriptorType))
			descriptorTypesOverspecifiedFrequencyTable.put(descriptorType, 1);
		else
			descriptorTypesOverspecifiedFrequencyTable.put(descriptorType, 
					descriptorTypesOverspecifiedFrequencyTable.get(descriptorType) + 1);
			
	}
	
	private void addDescriptorTypeToNotOverspecifiedTable(String descriptorType)
	{
		if (!descriptorTypesNotOverspecifiedFrequencyTable.containsKey(descriptorType))
			descriptorTypesNotOverspecifiedFrequencyTable.put(descriptorType, 1);
		else
			descriptorTypesNotOverspecifiedFrequencyTable.put(descriptorType, 
					descriptorTypesNotOverspecifiedFrequencyTable.get(descriptorType) + 1);
			
	}
	
	private String typeStringForDescriptor(AbstractDescriptor descriptor)
	{
		String propertyType;
		if (descriptor instanceof ObjectClass)
			propertyType = "class";
		else if (Property.representativeFeatureMap.containsKey(descriptor.getName()))
			propertyType = Property.representativeFeatureMap.get(descriptor.getName());
		else
			propertyType = "class";
		
		return propertyType;
	}
	
	private void addDescriptorToTable(AbstractDescriptor descriptor, boolean overspecified)
	{
		String featureForDescriptor = typeStringForDescriptor(descriptor);
		if (overspecified)
		{
			if (!descriptorTypesOverspecifiedFrequencyTable.containsKey(featureForDescriptor))
				descriptorTypesOverspecifiedFrequencyTable.put(featureForDescriptor, 1);
			else
				descriptorTypesOverspecifiedFrequencyTable.put(featureForDescriptor, 
						descriptorTypesOverspecifiedFrequencyTable.get(featureForDescriptor) + 1);
		}
		else
		{
			if (!descriptorTypesNotOverspecifiedFrequencyTable.containsKey(featureForDescriptor))
				descriptorTypesNotOverspecifiedFrequencyTable.put(featureForDescriptor, 1);
			else
				descriptorTypesNotOverspecifiedFrequencyTable.put(featureForDescriptor, 
						descriptorTypesNotOverspecifiedFrequencyTable.get(featureForDescriptor) + 1);			
		}
			
	}
	
	public void addTunaDemonstration(boolean overspecified, int objects, int referredObjects, 
										Set<String> descriptorTypes,
										Set<String> overspecifiedDescriptorTypes, 
										int visualDiversity)
	{
		if (overspecifiedDescriptorTypes.size() > 0)
			overspecifiedDemonstrations++;
		totalDemonstrations++;
		
		
		for (String descriptorType : descriptorTypes)
		{
			if (!overspecifiedDescriptorTypes.contains(descriptorType))
			{
				addDescriptorTypeToNotOverspecifiedTable(descriptorType);
				totalPathEdges++;
				numberOfObjectsNotOverspecifiedDataPoints.add(objects);
				numberOfReferredObjectsNotOverspecifiedDataPoints.add(referredObjects);
				visualDistancesNotOverspecifiedDataPoints.add((double)visualDiversity);
			}
			else
			{
				addDescriptorTypeToOverspecifiedTable(descriptorType);
				overspecifiedEdges++;
				totalPathEdges++;
				numberOfObjectsOverspecifiedDataPoints.add(objects);
				numberOfReferredObjectsOverspecifiedDataPoints.add(referredObjects);
				visualDistancesOverspecifiedDataPoints.add((double)visualDiversity);
			}
		}

		recalculate();
	}
	
	public boolean testTunaDemonstration(boolean overspecified, int objects,  
										List<String> descriptors, int visualDiversity)
	{
		boolean hypothesizedOverspecified = false;
		System.out.println("Test");
		for (String descriptor : descriptors)
		{
			
			double result = getOddsOfOverspecificationGivenFeatures(descriptor,
					objects,visualDiversity);
			System.out.println("Odds: " + result);
			if (result > 1.0)
				hypothesizedOverspecified = true;
		}
			
		return (hypothesizedOverspecified == overspecified);
	}
	
	public boolean testTunaEdge(boolean overspecified, int objects,  
										String descriptor, int visualDiversity)
	{
		boolean hypothesizedOverspecified = false;

		double result = getOddsOfOverspecificationGivenFeatures(descriptor,
					objects,visualDiversity);
		System.out.println("Odds: " + result);
		if (result > 1.0)
			hypothesizedOverspecified = true;
		
		return (hypothesizedOverspecified == overspecified);
	}
	
	public void printPaths()
	{
		for (LatticePath path : paths)
		{
			System.out.println("Path " + path);
			Set<WorldObject> lastPositiveExamples = null;
			Partition lastPartition = null;
			for (Partition p : path.getPartitionList())
			{
				if (lastPositiveExamples != null)
				{
					lastPositiveExamples.retainAll(p.getPositiveExamples());
					if (lastPositiveExamples.equals(lastPartition.getPositiveExamples()))
						System.out.println("OVERSPECIFIED");
				}

				System.out.println("Positive examples:");
				for (WorldObject wo : p.getPositiveExamples())
				{
					System.out.println(wo);
				}
				lastPositiveExamples = new HashSet<WorldObject>(p.getPositiveExamples());
				lastPartition = p;
			}
			
		}
	}


	public void addPath(LatticePath path, List<AbstractDescriptor> descriptors, 
			double visualDistance)
	{
		if (FIXED_GAUSSIANS)
			return;
		paths.add(path);
		totalPathEdges += path.getEdges().size();
		totalDemonstrations++;
		
		int numberOfObjects = path.getPartitionList().get(0).getPositiveExamples().size() +
				path.getPartitionList().get(0).getNegativeExamples().size();
		int numberOfReferredObjects = path.getResult().size();
		
		System.out.println("Path overspecification: " + path.isOverspecified());
		if (path.isOverspecified())
			overspecifiedDemonstrations++;

		int edgeIndex = 0;
		for (LatticeEdge e : path.getEdges())
		{
			System.out.println("Edge overspecification: " + e.isOverSpecified());
			//if (e.isOverSpecified() && !(descriptors.get(edgeIndex + 1) instanceof ObjectClass))
			if (e.isOverSpecified())
			{
				//System.out.println("Edge is overspecified");
				overspecifiedEdges++;
				numberOfObjectsOverspecifiedDataPoints.add(numberOfObjects);
				numberOfReferredObjectsOverspecifiedDataPoints.add(numberOfReferredObjects);
				visualDistancesOverspecifiedDataPoints.add(visualDistance);
			}
			else
			{
				System.out.println("Adding " + numberOfObjects + " to not overspecified");
				numberOfObjectsNotOverspecifiedDataPoints.add(numberOfObjects);
				numberOfReferredObjectsNotOverspecifiedDataPoints.add(numberOfReferredObjects);
				visualDistancesNotOverspecifiedDataPoints.add(visualDistance);
			}
			//if (e.isOverSpecified() && !(descriptors.get(edgeIndex + 1) instanceof ObjectClass))
			if (e.isOverSpecified())
			{
				if (e.getFrom().getPositiveExamples().size() > 
				e.getTo().getPositiveExamples().size())
				{
					AbstractDescriptor descriptor = descriptors.get(edgeIndex);
					addDescriptorToTable(descriptor,e.isOverSpecified());
				}
				else
				{
					AbstractDescriptor descriptor = descriptors.get(edgeIndex+1);
					addDescriptorToTable(descriptor,e.isOverSpecified());
				}
			}
			else
			{
				AbstractDescriptor descriptor = descriptors.get(edgeIndex);
				addDescriptorToTable(descriptor,e.isOverSpecified());
			}
			edgeIndex++;
		}
		
		recalculate();
	}
	
	public void recalculate()
	{
		
/*		System.out.println(overspecifiedDemonstrations + " overspecified demonstrations");
		System.out.println(totalDemonstrations - overspecifiedDemonstrations + 
				" non-overspecified demonstrations");
		System.out.println(overspecifiedEdges + " overspecified edges");
		System.out.println(totalPathEdges + " total edges");
		System.out.println(numberOfObjectsOverspecifiedDataPoints.size() + " overspecified data points");
		System.out.println(numberOfObjectsNotOverspecifiedDataPoints.size() + " not overspecified data points");
		*/
		if (!hasSufficientData())
			return;
		
		
		double SD = 0;
		SummaryStatistics statistics = new SummaryStatistics();
		SummaryStatistics statisticsNot = new SummaryStatistics();
		for (Integer i : numberOfObjectsOverspecifiedDataPoints)
			statistics.addValue(i);
		statistics.addValue(1);
		statistics.addValue(2);
		
		for (Integer i : numberOfObjectsNotOverspecifiedDataPoints)
			statisticsNot.addValue(i);

		statisticsNot.addValue(1);
		statisticsNot.addValue(2);
		
		SD = (statisticsNot.getStandardDeviation() + statistics.getStandardDeviation()) / 2.0;
		
		if (!FIXED_GAUSSIANS)
			numberOfObjectsGivenOverspecified = new NormalDistribution(statistics.getMean(), 
				SD);
		else
			numberOfObjectsGivenOverspecified = new NormalDistribution(2.423, 
					.643);
		
		//System.out.println("Mean #objects ovs: " + statistics.getMean());
		//System.out.println("SD #objects ovs: " + SD);
		
		statistics = new SummaryStatistics();
		//System.out.println("Not overspecified number data points");

		
		if (!FIXED_GAUSSIANS)
			numberOfObjectsGivenNotOverspecified = new NormalDistribution(statistics.getMean(), 
				SD);
		else
			numberOfObjectsGivenNotOverspecified = new NormalDistribution(2.66, 
					.866);			
		
		//System.out.println("Mean #objects Novs: " + statistics.getMean());
		//System.out.println("SD #objects Novs: " + SD);

		//System.out.println("Overspecified referred number data points");
		statistics = new SummaryStatistics();
		for (Integer i : numberOfReferredObjectsOverspecifiedDataPoints)
		{
		//	System.out.print(i + " ");
			statistics.addValue(i);
		}
		statistics.addValue(0);
		statistics.addValue(2);
		
		if (!FIXED_GAUSSIANS)
			numberOfReferredObjectsGivenOverspecified = new NormalDistribution(statistics.getMean(), 
				statistics.getStandardDeviation());
		else
			numberOfReferredObjectsGivenOverspecified = new NormalDistribution(1.27, 
					.667);			
		
		//System.out.println("Mean: " + statistics.getMean());
		//System.out.println("SD: " + statistics.getStandardDeviation());
		
		statistics = new SummaryStatistics();
		for (Integer i : numberOfReferredObjectsNotOverspecifiedDataPoints)
			statistics.addValue(i);
		statistics.addValue(0);
		statistics.addValue(2);
		
		if (!FIXED_GAUSSIANS)
			numberOfReferredObjectsGivenNotOverspecified = new NormalDistribution(statistics.getMean(), 
				statistics.getStandardDeviation());
		else
			numberOfReferredObjectsGivenNotOverspecified = new NormalDistribution(1, 
					.5);
		
		//System.out.println("Mean: " + statistics.getMean());
		//System.out.println("SD: " + statistics.getStandardDeviation());
		
		statistics = new SummaryStatistics();
		for (Double d : visualDistancesOverspecifiedDataPoints)
			statistics.addValue(d);
		
		statisticsNot = new SummaryStatistics();
		for (Double d : visualDistancesNotOverspecifiedDataPoints)
			statisticsNot.addValue(d);
		
		SD = (statistics.getStandardDeviation() + statisticsNot.getStandardDeviation()) / 2.0;
		
		if (!FIXED_GAUSSIANS)
			visualDistanceGivenOverspecified = new NormalDistribution(statistics.getMean(), 
				SD);
		else
			visualDistanceGivenOverspecified = new NormalDistribution(.565, 
					.15);
		
		//System.out.println("Mean: " + statistics.getMean());
		//System.out.println("SD: " + statistics.getStandardDeviation());
		

		
		if (!FIXED_GAUSSIANS)
			visualDistanceGivenNotOverspecified = new NormalDistribution(statistics.getMean(), 
				SD);
		else
			visualDistanceGivenNotOverspecified = new NormalDistribution(.348, 
					.079);

		//System.out.println("Mean: " + statistics.getMean());
		//System.out.println("SD: " + statistics.getStandardDeviation());
		
		totalOverspecifiedDescriptorOccurrences = 0;
		//System.out.println("Overspecified:");
		
		if (FIXED_GAUSSIANS)
		{
			descriptorTypesOverspecifiedFrequencyTable.put("class", 22);
			descriptorTypesOverspecifiedFrequencyTable.put("rgb", 1);
		}
		for (Entry<String,Integer> entry : descriptorTypesOverspecifiedFrequencyTable.entrySet())
		{
			System.out.println(entry.getKey() + " : " + entry.getValue());
			totalOverspecifiedDescriptorOccurrences += entry.getValue();
		}
		//System.out.println("Not overspecified");
		if (FIXED_GAUSSIANS)
		{
			descriptorTypesNotOverspecifiedFrequencyTable.put("class", 1);
			descriptorTypesNotOverspecifiedFrequencyTable.put("rgb", 6);
		}
		totalNotOverspecifiedDescriptorOccurrences = 0;
		for (Entry<String,Integer> entry : descriptorTypesNotOverspecifiedFrequencyTable.entrySet())
		{
			System.out.println(entry.getKey() + " : " + entry.getValue());
			totalNotOverspecifiedDescriptorOccurrences += entry.getValue();
		}
	}
	
	public double getPriorOverspecificationProbability()
	{
		if (FIXED_GAUSSIANS)
			return .7666;
		
		if (!hasSufficientData())
			return INITIAL_OVERSPECIFICATION_PROBABILITY;
		
		//return .45;
		return (double)overspecifiedEdges / totalPathEdges;
	}
	
	public double getCountPriorProbability()
	{
		return (double)overspecifiedEdges / totalPathEdges;
	}
	
	public double getProbabilityOfNumberOfObjectsGivenOverspecification(int numberOfObjects)
	{
		if (!hasSufficientData())
			return 0;
		
		return ((double)numberOfObjectsGivenOverspecified.density(numberOfObjects));
	}
	
	public double getProbabilityOfNumberOfObjectsGivenNotOverspecification(int numberOfObjects)
	{
		if (!hasSufficientData())
			return 0;
		
		return ((double)numberOfObjectsGivenNotOverspecified.density(numberOfObjects));
	}
	
	public double getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
			int numberOfObjectsReferredTo)
	{
		if (!hasSufficientData())
			return 0;
		
		return ((double)numberOfReferredObjectsGivenOverspecified.density(
					numberOfObjectsReferredTo));
			
	}
	
	public double getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification(
			int numberOfObjectsReferredTo)
	{
		if (!hasSufficientData())
			return 0;
		
		return ((double)numberOfReferredObjectsGivenNotOverspecified.density(
					numberOfObjectsReferredTo));
			
	}
	
	public double getProbabilityOfVisualDistanceGivenOverspecification(double distance)
	{
		if (!hasSufficientData())
			return 0;
	
		
		return ((double)visualDistanceGivenOverspecified.density(distance));
	}
	
	public double getProbabilityOfVisualDistanceGivenNotOverspecification(double distance)
	{
		if (!hasSufficientData())
			return 0;
	
		
		return ((double)visualDistanceGivenNotOverspecified.density(distance));
	}
	
	public double getProbabilityOfDescriptorTypeGivenOverspecification(AbstractDescriptor descriptor)
	{
		return getProbabilityOfDescriptorTypeGivenOverspecification(
				typeStringForDescriptor(descriptor));
		
	}
	
	public double getProbabilityOfDescriptorTypeGivenNotOverspecification(AbstractDescriptor descriptor)
	{
		return getProbabilityOfDescriptorTypeGivenNotOverspecification(
				typeStringForDescriptor(descriptor));
	}
	
	public double getProbabilityOfDescriptorTypeGivenOverspecification(String propertyType)
	{
		if (!hasSufficientData())
			return 0;
		
		if (!descriptorTypesOverspecifiedFrequencyTable.containsKey(propertyType))
		{
			descriptorTypesOverspecifiedFrequencyTable.put(propertyType, 0);
		}

		// Add-1 smoothing
		int	descriptorOccurrences = descriptorTypesOverspecifiedFrequencyTable.get(propertyType) +
				1;
		return ((double)descriptorOccurrences /
				(totalOverspecifiedDescriptorOccurrences + descriptorTypesOverspecifiedFrequencyTable.size()));
		
	}
	
	public double getProbabilityOfDescriptorTypeGivenNotOverspecification(String propertyType)
	{
		if (!hasSufficientData())
			return 0;
		
		if (!descriptorTypesNotOverspecifiedFrequencyTable.containsKey(propertyType))
		{
			descriptorTypesNotOverspecifiedFrequencyTable.put(propertyType, 0);
		}
			
		// Add-1 smoothing
		int	descriptorOccurrences = descriptorTypesNotOverspecifiedFrequencyTable.get(propertyType) +
				1;
		return ((double)descriptorOccurrences /
				(totalNotOverspecifiedDescriptorOccurrences + descriptorTypesNotOverspecifiedFrequencyTable.size()));
		
	}
	
	public double getProbabilityOfOverspecificationGivenFeatures(AbstractDescriptor descriptor,
			int numberOfObjects, int numberOfReferredObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return INITIAL_OVERSPECIFICATION_PROBABILITY;
		
		return getProbabilityOfDescriptorTypeGivenOverspecification(descriptor) *
				getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects) *
				getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
						numberOfReferredObjects) *
				getPriorOverspecificationProbability();
	}
	
	public double getProbabilityOfOverspecificationGivenFeatures(String descriptorType,
			int numberOfObjects, int numberOfReferredObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return INITIAL_OVERSPECIFICATION_PROBABILITY;
		
		return getProbabilityOfDescriptorTypeGivenOverspecification(descriptorType) *
				getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects) *
				getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
						numberOfReferredObjects) *
				getPriorOverspecificationProbability();
	}
	
//	public double getProbabilityOfOverspecifiedGivenFeatures(String descriptorType,
//			int numberOfObjects,double visualDistance)
//	{
//		double probabilityOverspecifiedGivenPoint = 
//				getProbabilityOfDescriptorTypeGivenOverspecification(descriptorType) *
//				getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects) *
//				getProbabilityOfVisualDistanceGivenOverspecification(visualDistance);
//		
//		double probabilityNotOverspecifiedGivenPoint = 
//				getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptorType) *
//				getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects) * 
//				getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance);
//		
//		
//	}
	
	// Note: no referred objects feature
	public double getOddsOfOverspecificationGivenFeatures(String descriptorType,
			int numberOfObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return INITIAL_OVERSPECIFICATION_PROBABILITY;

		System.out.println("Descriptor: " + descriptorType);
		System.out.println("Number of objects:" + numberOfObjects);
		System.out.println("Visual diversity: " + visualDistance);
		double smoothedPrior = getPriorOverspecificationProbability();
		
		System.out.println("getProbabilityOfDescriptorTypeGivenOverspecification(descriptor): " +
				getProbabilityOfDescriptorTypeGivenOverspecification(descriptorType));
		//System.out.println("getProbabilityOfNumberOfObjectsGivenOverspecification: " + 
		//		getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects));
		//System.out.println("getProbabilityOfVisualDistanceGivenOverspecification: " +
		//		getProbabilityOfVisualDistanceGivenOverspecification(visualDistance));
		System.out.println("getPriorOverspecificationProbability: " +
				getPriorOverspecificationProbability());
		
		
		System.out.println("getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor): " +
				getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptorType));
		//System.out.println("getProbabilityOfNumberOfObjectsGivenNotOverspecification: " + 
		//		getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects));
		//System.out.println("getProbabilityOfVisualDistanceGivenNotOverspecification: " +
		//		getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance));
		
//		return (getProbabilityOfDescriptorTypeGivenOverspecification(descriptorType) *
//					//getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects) *
//						//getProbabilityOfVisualDistanceGivenOverspecification(visualDistance)) //*
//						getPriorOverspecificationProbability()) / 
//
//				(getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptorType) *
//					//getProbabilityOfNumberOfObjectsGivenNotOverspecification(
//					//	numberOfObjects) * 
//						//getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance) //*
//						 (1 - getPriorOverspecificationProbability()));
		return (getProbabilityOfDescriptorTypeGivenOverspecification(descriptorType) * smoothedPrior) /
				(getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptorType) * (1-smoothedPrior));
	}
	
	public double getOddsOfOverspecificationGivenFeatures(AbstractDescriptor descriptor,
			int numberOfObjects, int numberOfReferredObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return INITIAL_OVERSPECIFICATION_PROBABILITY;

		return (getProbabilityOfDescriptorTypeGivenOverspecification(descriptor) *
					getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects) *
					getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
						numberOfReferredObjects) *
						getPriorOverspecificationProbability()) / 
				(getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor) *
					getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects) *
					getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification(
						numberOfReferredObjects) * (1 - getPriorOverspecificationProbability()));
	}
	
	public double getLogRatioOfOverspecificationGivenFeatures(AbstractDescriptor descriptor,
			int numberOfObjects, int numberOfReferredObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return Math.log(INITIAL_OVERSPECIFICATION_PROBABILITY / 
							(1.0 - INITIAL_OVERSPECIFICATION_PROBABILITY));
		
		System.out.println("getProbabilityOfDescriptorTypeGivenOverspecification(descriptor): " +
				getProbabilityOfDescriptorTypeGivenOverspecification(descriptor));
		System.out.println("getProbabilityOfNumberOfObjectsGivenOverspecification: " + 
				getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects));
		System.out.println("getProbabilityOfNumberOfObjectsReferredToGivenOverspecification: " +
				getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(numberOfReferredObjects));
		System.out.println("getProbabilityOfVisualDistanceGivenOverspecification: " +
				getProbabilityOfVisualDistanceGivenOverspecification(visualDistance));
		System.out.println("getPriorOverspecificationProbability: " +
				getPriorOverspecificationProbability());
		
		
		System.out.println("getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor): " +
				getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor));
		System.out.println("getProbabilityOfNumberOfObjectsGivenNotOverspecification: " + 
				getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects));
		System.out.println("getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification: " +
				getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification(numberOfReferredObjects));
		System.out.println("getProbabilityOfVisualDistanceGivenNotOverspecification: " +
				getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance));
		
		System.out.println("Log values: ");
		System.out.println("getProbabilityOfDescriptorTypeGivenOverspecification(descriptor): " +
				Math.log(getProbabilityOfDescriptorTypeGivenOverspecification(descriptor)));
		System.out.println("getProbabilityOfNumberOfObjectsGivenOverspecification: " + 
				Math.log(getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects)));
		System.out.println("getProbabilityOfNumberOfObjectsReferredToGivenOverspecification: " +
				Math.log(getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(numberOfReferredObjects)));
		System.out.println("getProbabilityOfVisualDistanceGivenOverspecification: " +
				Math.log(getProbabilityOfVisualDistanceGivenOverspecification(visualDistance)));
		System.out.println("getPriorOverspecificationProbability: " +
				Math.log(getPriorOverspecificationProbability()));
		
		
		System.out.println("getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor): " +
				Math.log(getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor)));
		System.out.println("getProbabilityOfNumberOfObjectsGivenNotOverspecification: " + 
				Math.log(getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects)));
		System.out.println("getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification: " +
				Math.log(getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification(numberOfReferredObjects)));
		System.out.println("getProbabilityOfVisualDistanceGivenNotOverspecification: " +
				Math.log(getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance)));
		
		double numerator = (Math.log(getProbabilityOfDescriptorTypeGivenOverspecification(descriptor)) +
				Math.log(getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects)) +
//				Math.log(getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
//					numberOfReferredObjects)) +
				Math.log(getProbabilityOfVisualDistanceGivenOverspecification(visualDistance))+
					Math.log(getPriorOverspecificationProbability()));
		
		double denominator = (Math.log(getProbabilityOfDescriptorTypeGivenNotOverspecification(descriptor)) +
				Math.log(getProbabilityOfNumberOfObjectsGivenNotOverspecification(numberOfObjects)) +
//				Math.log(getProbabilityOfNumberOfObjectsReferredToGivenNotOverspecification(
//					numberOfReferredObjects)) +
				Math.log(getProbabilityOfVisualDistanceGivenNotOverspecification(visualDistance)) +
				Math.log(1 - getPriorOverspecificationProbability()));
		
		System.out.println("Numerator: " + numerator);
		System.out.println("Denominator: " + denominator);
		
		return numerator - denominator;
	}
	
	public double getLogProbabilityOfOverspecificationGivenFeatures(AbstractDescriptor descriptor,
			int numberOfObjects, int numberOfReferredObjects,double visualDistance)
	{
		if (!hasSufficientData())
			return Math.log(INITIAL_OVERSPECIFICATION_PROBABILITY);
		
		return Math.log(getProbabilityOfDescriptorTypeGivenOverspecification(descriptor)) +
				Math.log(getProbabilityOfNumberOfObjectsGivenOverspecification(numberOfObjects)) +
				Math.log(getProbabilityOfNumberOfObjectsReferredToGivenOverspecification(
						numberOfReferredObjects)) +
				Math.log(getPriorOverspecificationProbability());		
	}
	
	public boolean hasSufficientData()
	{
		if (FIXED_GAUSSIANS)
			return true;
		
		return !(overspecifiedDemonstrations < 3 || 
		(totalDemonstrations - overspecifiedDemonstrations) < 3 ||
		overspecifiedEdges < 3 ||
		(totalPathEdges - overspecifiedEdges) < 3);
	}
}
