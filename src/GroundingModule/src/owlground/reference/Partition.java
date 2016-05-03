package owlground.reference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Iterator;
import java.util.TreeMap;
import java.util.TreeSet;

import org.jblas.DoubleMatrix;

import owlground.objects.AbstractDescriptor;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.objects.WorldObjectGroup;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;
import owlground.spaces.FeatureSpaceValueTuple;
import owlground.spaces.SpaceManager;
import owlground.utilities.QuickSelect;
import owlground.utilities.Statistics;
import owlground.utilities.Utilities;

public strictfp class Partition {
	protected Collection<WorldObject> positiveExamples;
	protected Collection<WorldObject> negativeExamples;
	protected Collection<LatticeEdge> outgoingEdges;
	protected double labeledProbability;
	protected boolean unlabeledDistanceCalculated;
	protected double unlabeledProbability;
	protected double normalizedProbability;
	protected FeatureSpace unlabeledFeature;
	protected double metaProbability;
	protected ArrayList<Double> metaProbabilityCandidates;
	protected boolean metaProbabilityCalculated;
	
	private final boolean useUnlabeledProbability = false;
	protected boolean classNamePartition;
	
	public Partition(Collection<WorldObject> positiveExamples,
						Collection<WorldObject> negativeExamples, boolean classNamePartition)
	{
		this.positiveExamples = positiveExamples;
		this.negativeExamples = negativeExamples;
		this.outgoingEdges = new ArrayList<LatticeEdge>();
		this.unlabeledDistanceCalculated = false;
		unlabeledProbability = -1; // If this isn't set, then something's wrong
		labeledProbability = 1; // Don't count against it if there are no labels
		metaProbability = 1; // From reasoning over multiple examples
		unlabeledFeature = null;
		this.classNamePartition = classNamePartition;
		metaProbabilityCalculated = false;
	}
	
	public void mergePartition(Partition p)
	{
		positiveExamples.addAll(p.positiveExamples);
		negativeExamples.addAll(p.negativeExamples);
		unlabeledDistanceCalculated = false;
	}
	
	public boolean isCompatible(WorldObjectGroup wog)
	{
		if (!wog.isNull() && positiveExamples.size() < 1)
			return false;
		
		if (wog.isPlural() && positiveExamples.size() < 2)
			return false;
		
		if (!wog.isPlural() && positiveExamples.size() > 1)
			return false;
		
		return true;
	}
	
	public boolean equals(Partition p)
	{
		return this.positiveExamples.equals(p.positiveExamples);
	}

	public Collection<WorldObject> getPositiveExamples() {
		return positiveExamples;
	}

	public Collection<WorldObject> getNegativeExamples() {
		return negativeExamples;
	}
	
	public void addOutgoingEdge(LatticeEdge e)
	{
		outgoingEdges.add(e);
	}
	
	public Collection<LatticeEdge> getOutgoingEdges()
	{
		return outgoingEdges;
	}
	
	public void setLabeledProbability(Map<WorldObject,Double> probabilities)
	{
		double minPositiveProbability = 1;
		double maxNegativeProbability = 0;
		
		if (negativeExamples.size() == 0)
			maxNegativeProbability = 0;
		
		for (Entry<WorldObject, Double> entry : probabilities.entrySet())
		{
			if (positiveExamples.contains(entry.getKey()))
			{
				if (entry.getValue() < minPositiveProbability)
				{
					minPositiveProbability = entry.getValue();
				}
			}
			else
			{
				if (entry.getValue() > maxNegativeProbability)
				{
					maxNegativeProbability = entry.getValue();
				}
			}
		}
		//System.out.println("Min negative distance" + );
		labeledProbability = minPositiveProbability * (1 - maxNegativeProbability);
	}
	
	public void setLabeledProbability(double value)
	{
		labeledProbability = value;
	}
	
	private TreeMap<Double,FeatureSpace> getProbabilitiesForSplit(Collection<WorldObject> positiveExamples, 
																		Collection<WorldObject> negativeExamples,
																		Collection<FeatureSpace> features)
	{
		TreeMap<Double, FeatureSpace> results = new TreeMap<Double, FeatureSpace>();
		HashMap<WorldObject,Map<FeatureSpace,DoubleMatrix>> averagePerceptClusters = 
				new HashMap<WorldObject, Map<FeatureSpace,DoubleMatrix>>();
		
		for (WorldObject wo : positiveExamples)
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
		
		for (WorldObject wo : negativeExamples)
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
		

		
		for (FeatureSpace feature : features)
		{
			double maxPositiveDistance = 0.0;
			double minInterDistance = Double.MAX_VALUE;
			// Find maximum intraclass distance for positive examples
			for (WorldObject wo1 : positiveExamples)
			{
				for (WorldObject wo2 : positiveExamples)
				{
					if (wo1 == wo2)
						continue;
					double distance = feature.getSquaredMahalanobisDistance(averagePerceptClusters.get(wo1).get(feature), 
																	averagePerceptClusters.get(wo2).get(feature));
					
					if (distance > maxPositiveDistance)
						maxPositiveDistance = distance;
				}
			}
			
			// Find minimum interclass distance
			for (WorldObject wo1 : negativeExamples)
			{
				for (WorldObject wo2 : positiveExamples)
				{
					
					double distance = feature.getSquaredMahalanobisDistance(averagePerceptClusters.get(wo1).get(feature), 
																	averagePerceptClusters.get(wo2).get(feature));
					
					if (distance < minInterDistance)
						minInterDistance = distance;
				}
			}
			double probability;
			
			//System.out.println("Min inter distance: " + minInterDistance);
			//System.out.println("Max positive distance: " + maxPositiveDistance);
			//System.out.println("Feature dimension: " + feature.getDimension());
			if (negativeExamples.size() == 0)
			{
				probability = Statistics.mahalanobisToProbability(maxPositiveDistance, feature.getDimension());
			}
			else
			{
				probability = Statistics.mahalanobisToProbability(maxPositiveDistance, feature.getDimension()) *
						(1 - Statistics.mahalanobisToProbability(minInterDistance, feature.getDimension()));
			}
			//System.out.println("Probability: " + probability);
			
			results.put(probability, feature);
		
		}
		
		return results;
	}
																		
	
	public TreeMap<Double,FeatureSpace> getProbabilitiesOfSameClassAsPartition(Partition other)
	{
		HashMap<WorldObject,Map<FeatureSpace,DoubleMatrix>> averagePerceptClusters = 
				new HashMap<WorldObject, Map<FeatureSpace,DoubleMatrix>>();
		Set<FeatureSpace> features = null;

		for (WorldObject wo : positiveExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		for (WorldObject wo : negativeExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		for (WorldObject wo : other.positiveExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		for (WorldObject wo : other.negativeExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		//System.out.println("Features before removing shape features");
		//System.out.println(features);
		// If it's the end of the chain (a class name), just use shape features
		if (classNamePartition)
			removeNonShapeFeatures(features);
		
		//System.out.println("Features after removing shape features");
		//System.out.println(features);
		
		return getProbabilitiesForSplit(positiveExamples, negativeExamples,features);
	}
	
	
	private void calculateUnlabeledProbability()
	{
		HashMap<WorldObject,Map<FeatureSpace,DoubleMatrix>> averagePerceptClusters = 
				new HashMap<WorldObject, Map<FeatureSpace,DoubleMatrix>>();
		Set<FeatureSpace> features = null;

		for (WorldObject wo : positiveExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			if (averagePerceptClusters.get(wo).keySet().size() != 0)
				features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		for (WorldObject wo : negativeExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			if (averagePerceptClusters.get(wo).keySet().size() != 0)
				features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		
		//System.out.println("Features before removing shape features");
		//System.out.println(features);
		
		// If it's the end of the chain (a class name), just use shape features
		if (classNamePartition)
			removeNonShapeFeatures(features);
		
		//System.out.println("Features after removing shape features");
		//System.out.println(features);
		
		TreeMap<Double,FeatureSpace> result = getProbabilitiesForSplit(positiveExamples, negativeExamples, features);
		
		unlabeledProbability = result.lastKey();
		unlabeledFeature = result.lastEntry().getValue();
	}
	
	// Currently leaves texture in, testing for object recognition
	private void removeNonShapeFeatures(Set<FeatureSpace> features)
	{
		Iterator<FeatureSpace> featureSpaceIterator = features.iterator();
		
		while (featureSpaceIterator.hasNext())
		{
			FeatureSpace feature = featureSpaceIterator.next();
			//if (!(feature.getName().equals("zernike") || feature.getName().equals("hu") ||
			//		feature.getName().equals("haralick")))
			if (!(feature.getName().equals("zernike") || 
					feature.getName().equals("normalhistogram")))	
				featureSpaceIterator.remove();
		}
	}
	
	public double getUnlabeledProbability()
	{
		if (useUnlabeledProbability == false)
			return 1;
		if (!unlabeledDistanceCalculated)
			calculateUnlabeledProbability();
		
		return unlabeledProbability;
	}
	
	public double getLabeledProbability()
	{
		
		return labeledProbability;
	}
	
	public double getLogTotalProbability()
	{
		return Math.log(labeledProbability) + Math.log(getUnlabeledProbability()) + Math.log(getMetaProbability());
	}
	
	public double getTotalProbability()
	{

		//System.out.println("Total probability: " + labeledProbability * unlabeledProbability * getMetaProbability());
		//System.out.println("Labeled Prob:" + labeledProbability);
		//System.out.println("Unlabeled prob:" + getUnlabeledProbability());
		//System.out.println("Meta prob: " + getMetaProbability());
		return labeledProbability * getUnlabeledProbability() * getMetaProbability();
		//return (labeledProbability + getUnlabeledProbability() + getMetaProbability()) / 3.0;
	}

	public FeatureSpace getUnlabeledFeature() {
		if (!unlabeledDistanceCalculated)
			calculateUnlabeledProbability();
		
		return unlabeledFeature;
	}

	public double getNormalizedProbability() {
		return normalizedProbability;
	}

	public void setNormalizedProbability(double normalizedProbability) {
		this.normalizedProbability = normalizedProbability;
	}

	public double getMetaProbability() {
		if (metaProbabilityCandidates == null ||
				metaProbabilityCandidates.size() == 0)
			return 1;
		
		if (!metaProbabilityCalculated)
			calculateMetaProbability();
			
		return metaProbability;
	}
	
	private void calculateMetaProbability()
	{
		// Calculate the median metaprobability
		//metaProbability = QuickSelect.select(metaProbabilityCandidates.toArray(new Double[]{}), 
		//		metaProbabilityCandidates.size() / 2);
		double total = 0;
		for(double element : metaProbabilityCandidates)
		{
			total += element;
		}
		metaProbability = total / metaProbabilityCandidates.size();
		metaProbabilityCalculated = true;
		//metaProbability = 1;
		//System.out.println("Meta probability: " + metaProbability);
	}

	protected void addMetaProbability(double metaProbability) {
		if (metaProbabilityCandidates == null)
			metaProbabilityCandidates = new ArrayList<Double>();
		metaProbabilityCandidates.add(metaProbability);
		metaProbabilityCalculated = false;
	}
	
	public double getAverageVisualDistanceBetweenObjects()
	{
		HashMap<WorldObject,Map<FeatureSpace,DoubleMatrix>> averagePerceptClusters = 
				new HashMap<WorldObject, Map<FeatureSpace,DoubleMatrix>>();
		Set<FeatureSpace> features = null;

		for (WorldObject wo : positiveExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			if (averagePerceptClusters.get(wo).keySet().size() != 0)
				features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		for (WorldObject wo : negativeExamples)
		{
			averagePerceptClusters.put(wo,wo.getAveragePerceptValues());
			if (averagePerceptClusters.get(wo).keySet().size() != 0)
				features = new TreeSet<FeatureSpace>(averagePerceptClusters.get(wo).keySet());
		}
		
		TreeMap<Double,FeatureSpace> result = getProbabilitiesForSplit(positiveExamples, negativeExamples, features);
		double total = 0.0;
		
		for (Double probability : result.keySet())
			total += probability;
		
		unlabeledProbability = result.lastKey();
		unlabeledFeature = result.lastEntry().getValue();
		
		return total / result.size();
	}
	
	
}
