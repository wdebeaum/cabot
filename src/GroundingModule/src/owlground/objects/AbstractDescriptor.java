package owlground.objects;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.jblas.DoubleMatrix;
import org.jblas.Solve;

import owlground.language.Utterance;
import owlground.perception.Percept;
import owlground.perception.PerceptCluster;
import owlground.regions.DataRegion;
import owlground.spaces.FeatureSpace;
import owlground.spaces.SpaceManager;

public abstract class AbstractDescriptor {
	protected HashMap<Long, PerceptCluster> perceptClusters;
	protected HashMap<Long, PerceptCluster> negativePerceptClusters;
	protected HashSet<WorldObject> examples;
	protected HashSet<Utterance> utterances;
	protected String name;
	protected HashSet<String> comentions;
	protected HashMap<FeatureSpace, Double> featureRatios;
	protected boolean useHandDistance = false;
	
	public AbstractDescriptor(String name)
	{
		this.name = name;
		perceptClusters = new HashMap<Long, PerceptCluster>();
		negativePerceptClusters = new HashMap<Long, PerceptCluster>();
		utterances = new HashSet<Utterance>();
		comentions = new HashSet<String>();
		featureRatios = new HashMap<FeatureSpace, Double>();
		examples = new HashSet<WorldObject>();
	}
	
	public HashMap<FeatureSpace, Double> getFeatureRatios() {
		return featureRatios;
	}


	public HashMap<Long, PerceptCluster> getPerceptClusters() {
		return perceptClusters;
	}
	
	
	public HashMap<Long, PerceptCluster> getNegativePerceptClusters()
	{
		return negativePerceptClusters;
	}

	public HashSet<Utterance> getUtterances() {
		return utterances;
	}
	
	public void addComentions(Collection<String> comentions)
	{
		comentions.addAll(comentions);
		
		// Remove itself from comentions
		if (comentions.contains(this.name))
			comentions.remove(this.name);
	}
	
	public void addPerceptCluster(PerceptCluster pc)
	{
		perceptClusters.put(pc.getTimestamp(), pc);
	}
	
	public void addPerceptClusters(Collection<PerceptCluster> newPerceptClusters)
	{
		for (PerceptCluster pc: newPerceptClusters)
		{
			perceptClusters.put(pc.getTimestamp(), pc);
			
			for (Entry<FeatureSpace, Percept> e : pc.getPercepts().entrySet())
			{
				//System.out.println("Adding percept for " + e.getKey());
				
				if (e.getKey().getRegion(this) == null)
					e.getKey().addDefaultRegion(this);
				e.getKey().getRegion(this).addAttendedPercept(e.getValue());
			}
		}
		
		addPerceptClustersRegions();
	}
	
	public void addNegativePerceptClusters(Collection<PerceptCluster> newPerceptClusters)
	{
		for (PerceptCluster pc: newPerceptClusters)
		{
			negativePerceptClusters.put(pc.getTimestamp(), pc);
			
//			for (Entry<FeatureSpace, Percept> e : pc.getPercepts().entrySet())
//			{
//				//System.out.println("Adding percept for " + e.getKey());
//				
//				if (e.getKey().getRegion(this) == null)
//					e.getKey().addDefaultRegion(this);
//				e.getKey().getRegion(this).addAttendedPercept(e.getValue());
//			}
		}
		
		//addNegativePerceptClustersRegions();
	}
	
	public void addPerceptClustersRegions()
	{
		for (PerceptCluster pc: perceptClusters.values())
		{	
			for (Entry<FeatureSpace, Percept> e : pc.getPercepts().entrySet())
			{
				FeatureSpace fs = e.getKey();
				DataRegion region = fs.getRegion(this);
				if (region == null)
				{
					fs.addDefaultRegion(this);
				}
				fs.getRegion(this).addAttendedPercept(e.getValue());
			}
		}		
	}

	
	public void addUtterance(Utterance u)
	{
		utterances.add(u);
	}
	
	public int updatePerceptClusters()
	{
//		for (WorldObject wo : examples)
//		{
//			perceptClusters.putAll(wo.getPerceptClusters());
//		}
		for (Utterance u : utterances)
		{
			for (WorldObject wo : u.getWorldObjects())
			{
				perceptClusters.putAll(wo.getPerceptClusters());
			}
		}
		// Needs to be changed to only WorldObjects, rather than
		// whole utterance
		if (useHandDistance)
		{
			for (Utterance u: utterances)
			{
				perceptClusters.putAll(u.getAttendedPerceptClusters());
			}
		}
		
		addPerceptClustersRegions();
		
		return perceptClusters.size();
	}
	
	public DoubleMatrix getMeanForFeature(FeatureSpace featureSpace)
	{
		
		
		DoubleMatrix result = null;
		for (PerceptCluster pc : perceptClusters.values())
		{
			if (result == null)
			{
				result = DoubleMatrix.zeros(pc.getPercepts().get(featureSpace).getValue().length);
			}
			HashMap<FeatureSpace,Percept> percepts = pc.getPercepts();
			Percept percept = percepts.get(featureSpace);
			DoubleMatrix value = percept.getValue();
			result.addi(value);
		}
		
		result.divi(perceptClusters.size());
		
		return result;
	}
	
	public DoubleMatrix getUtteranceMeanForFeature(FeatureSpace featureSpace)
	{
		DoubleMatrix totalResult = null;
		
		for (Utterance u : utterances)
		{
			System.out.println("Utterance " + u.getUttNum());
			DoubleMatrix result = null;
			if (u.getAttendedPerceptClusters().isEmpty())
				continue;
			
			
			for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
			{
				System.out.println("PerceptCluster " + pc);
				if (result == null)
					result = DoubleMatrix.zeros(pc.getPercepts().get(featureSpace).getValue().length);
				if (totalResult == null)
					totalResult = DoubleMatrix.zeros(result.length);
				result.addi(pc.getPercepts().get(featureSpace).getValue());
			}
			result.divi(u.getAttendedPerceptClusters().size());
			
			totalResult.addi(result);
		}
		
		totalResult.divi(utterances.size());
		
		return totalResult;
	}
	
/*	public double getScaledSumOfSquaresForFeature(String featureName)
	{
		
		DoubleMatrix mean = getMeanForFeature(featureName);
		double result = 0f;
		int numUtterances = 0;
		for (Utterance u : utterances)
		{
			if (u.getPerceptClusters().size() > 0)
			{
				// Get mean for the utterance
				DoubleMatrix utteranceMean = DoubleMatrix.zeros(mean.length);
				for (PerceptCluster pc : u.getPerceptClusters().values())
				{
					if (pc.getPercepts().containsKey(featureName))
						utteranceMean.addi(pc.getPercepts().get(featureName).getValue());
				}
				utteranceMean.divi(u.getPerceptClusters().size());
				
				result += utteranceMean.distance2(mean);
				numUtterances++;
			}
			
		}
		
		return result / numUtterances;
	}*/
	
	public double getScaledSumOfSquaresForFeature(FeatureSpace featureSpace)
	{
		
		DoubleMatrix mean = getMeanForFeature(featureSpace);
		double result = 0f;
		int numUtterances = 0;
		for (Utterance u : utterances)
		{
			if (u.getAttendedPerceptClusters().size() > 0)
			{
				// Get mean for the utterance
				DoubleMatrix utteranceMean = DoubleMatrix.zeros(mean.length);
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					if (pc.getPercepts().containsKey(featureSpace))
						utteranceMean.addi(pc.getPercepts().get(featureSpace).getValue());
				}
				utteranceMean.divi(u.getAttendedPerceptClusters().size());
				
				result += utteranceMean.distance2(mean);
				numUtterances++;
			}
			
		}
		
		return result / numUtterances;
	}
	
/*	public double getPCScaledSumOfSquaresForFeature(String featureName)
	{
		
		DoubleMatrix mean = getMeanForFeature(featureName);
		double result = 0f;

		for (PerceptCluster pc : perceptClusters.values())
		{
			result += pc.getPercepts().get(featureName).getValue().distance2(mean);
		}
		if (perceptClusters.size() == 0)
			System.out.println("No perceptClusters");
		
		return result / perceptClusters.size();
	}*/
	public double getPCScaledSumOfSquaresForFeature(FeatureSpace featureSpace)
	{
		
		DoubleMatrix mean = getMeanForFeature(featureSpace);
		double result = 0f;

		for (PerceptCluster pc : perceptClusters.values())
		{
			result += pc.getPercepts().get(featureSpace).getValue().distance2(mean);
		}
		if (perceptClusters.size() == 0)
			System.out.println("No perceptClusters");
		
		return result / perceptClusters.size();
	}
	
	public Map<FeatureSpace,DoubleMatrix> getUtteranceMeanForAllFeatures()
	{
		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		
		Set<FeatureSpace> featureSpaces = null;
		
		for (PerceptCluster pc : perceptClusters.values())
		{
			featureSpaces = pc.getPercepts().keySet();
			if (featureSpaces != null && featureSpaces.size() > 0)
				break;
		}
		if (featureSpaces == null)
			return new HashMap<FeatureSpace,DoubleMatrix>();
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			result.put(featureSpace, getUtteranceMeanForFeature(featureSpace));
		}
		
		return result;		
	}
	
	public Map<FeatureSpace,DoubleMatrix> getMeanForAllFeatures()
	{

		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		
		Set<FeatureSpace> featureSpaces = null;
		
		for (PerceptCluster pc : perceptClusters.values())
		{
			featureSpaces = pc.getPercepts().keySet();
			if (featureSpaces != null && featureSpaces.size() > 0)
				break;
		}
		if (featureSpaces == null)
			return new HashMap<FeatureSpace,DoubleMatrix>();
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			result.put(featureSpace, getMeanForFeature(featureSpace));
		}
		
		return result;
	}
	
	public DoubleMatrix getCombinedMean()
	{
		DoubleMatrix result = DoubleMatrix.zeros(getTotalNumberOfDimensions());
		
		for (PerceptCluster pc : perceptClusters.values())
		{
			result.addi(pc.combinedMatrix());
		}
		result.divi(perceptClusters.size());
		
		return result;
		
	}
	
	public Map<FeatureSpace,Double> getScaledSumOfSquaresForAllFeatures()
	{

		HashMap<FeatureSpace,Double> result = new HashMap<FeatureSpace, Double>();
		
		Set<FeatureSpace> featureSpaces = getFeatureSpaces();
		
		if (featureSpaces == null)
		{
			//System.out.println("No features found in perceptClusters");
			return new HashMap<FeatureSpace,Double>();
		}
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			result.put(featureSpace, getScaledSumOfSquaresForFeature(featureSpace));
		}
		
		return result;
	}
	
	//TODO I think
	public DoubleMatrix getVectorVarianceForFeature(FeatureSpace featureSpace)
	{
		
		DoubleMatrix mean = getMeanForFeature(featureSpace);
		DoubleMatrix meanDistance = DoubleMatrix.zeros(mean.length);
		
		int numUtterances = 0;
		for (Utterance u : utterances)
		{
			if (u.getAttendedPerceptClusters().size() > 0)
			{
				// Get mean for the utterance
				DoubleMatrix utteranceMean = DoubleMatrix.zeros(mean.length);
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					if (pc.getPercepts().containsKey(featureSpace))
						utteranceMean.addi(pc.getPercepts().get(featureSpace).getValue());
				}
				utteranceMean.divi(u.getAttendedPerceptClusters().size());
				
				meanDistance.addi((utteranceMean.sub(mean)).mul(utteranceMean.sub(mean)));
				numUtterances++;
			}
			
		}
		
		return meanDistance.div(numUtterances);
	}
	
	public DoubleMatrix getCovarianceForFeature(FeatureSpace featureSpace)
	{
		DoubleMatrix mean = getMeanForFeature(featureSpace);
		DoubleMatrix covarianceMatrix = DoubleMatrix.zeros(mean.length, mean.length);
		
		for (int i = 0; i < mean.length; i++)
		{
			for(int j = 0; j < mean.length; j++)
			{
				double result = 0.0d;
				for (PerceptCluster pc : perceptClusters.values())
				{
					result += (pc.getPercepts().get(featureSpace).getValue().get(i) - mean.get(i)) *
							  (pc.getPercepts().get(featureSpace).getValue().get(j) - mean.get(j));
				}
				
				covarianceMatrix.put(j + i * mean.length, result / perceptClusters.size());
				
			}
		}

		return covarianceMatrix;
	}
	
	public DoubleMatrix getCombinedCovariance()
	{
		DoubleMatrix mean = getCombinedMean();
		DoubleMatrix covarianceMatrix = DoubleMatrix.zeros(mean.length, mean.length);
		
		for (int i = 0; i < mean.length; i++)
		{
			for(int j = 0; j < mean.length; j++)
			{
				double result = 0.0d;
				for (PerceptCluster pc : perceptClusters.values())
				{
					DoubleMatrix combinedMatrix = pc.combinedMatrix();
					result += (combinedMatrix.get(i) - mean.get(i)) *
							  (combinedMatrix.get(j) - mean.get(j));
				}
				
				covarianceMatrix.put(j + i * mean.length, result / perceptClusters.size());
				
			}
		}

		return covarianceMatrix;
	}
	
	public Map<FeatureSpace,DoubleMatrix> getCovarianceForAllFeatures()
	{

		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		
		Set<FeatureSpace> featureSpaces = getFeatureSpaces();
		
		if (featureSpaces == null)
		{
			//System.out.println("No features found in perceptClusters");
			return new HashMap<FeatureSpace,DoubleMatrix>();
		}
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			result.put(featureSpace, getCovarianceForFeature(featureSpace));
		}
		
		return result;
	}
	
	public Map<FeatureSpace,DoubleMatrix> getPrecisionForAllFeatures()
	{
		double ridgeEpsilon = .0000001;
		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		
		Set<FeatureSpace> featureSpaces = getFeatureSpaces();
		
		if (featureSpaces == null)
		{
			//System.out.println("No features found in perceptClusters");
			return new HashMap<FeatureSpace,DoubleMatrix>();
		}
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			DoubleMatrix covariance = getCovarianceForFeature(featureSpace);
			//Ridging for singular matrices
			covariance.addi(DoubleMatrix.eye(covariance.columns).mul(ridgeEpsilon));
			
			DoubleMatrix covarianceInverse = Solve.solveSymmetric(covariance, DoubleMatrix.eye(covariance.rows));
			result.put(featureSpace, covarianceInverse);
		}
		
		return result;
	}
	
	public DoubleMatrix getPrecisionForAllData()
	{
		double ridgeEpsilon = .0000001;
		int dimensions = 0;
		for (PerceptCluster pc : perceptClusters.values())
		{
			dimensions = pc.getNumberOfDimensions();
			if (dimensions > 0)
				break;
		}
		
		DoubleMatrix result = new DoubleMatrix(dimensions);

		DoubleMatrix covariance = getCombinedCovariance();
		//Ridging for singular matrices
		covariance.addi(DoubleMatrix.eye(covariance.columns).mul(ridgeEpsilon));
		
		DoubleMatrix covarianceInverse = Solve.solveSymmetric(covariance, DoubleMatrix.eye(covariance.rows));
		return covarianceInverse;

	}
	
	
	//TODO I think
	public Map<FeatureSpace,DoubleMatrix> getVectorVarianceForAllFeatures()
	{

		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		
		Set<FeatureSpace> featureSpaces = getFeatureSpaces();
		
		if (featureSpaces == null)
		{
			System.out.println("No features found in perceptClusters");
			return new HashMap<FeatureSpace,DoubleMatrix>();
		}
		
		for (FeatureSpace featureSpace : featureSpaces)
		{
			result.put(featureSpace, getVectorVarianceForFeature(featureSpace));
		}
		
		return result;
	}
	
	public String getName() {
		return name;
	}
	
	public String getWord() 
	{
		return name.split("W::")[1].toLowerCase();
	}
	
	public double getMinimumDistanceForFeature(FeatureSpace featureSpace, DoubleMatrix point)
	{
		double minimumValue = Double.MAX_VALUE;
		
		System.out.println(perceptClusters.size() + " PC's in label " + name);
		for (PerceptCluster pc : perceptClusters.values())
		{
			if (!pc.getPercepts().containsKey(featureSpace))
			{
				continue;
			}
			if (pc.featureDistance(featureSpace, point) < minimumValue)
			{
				minimumValue = pc.featureDistance(featureSpace, point);
			}
		}
		
		return minimumValue;
	}
	
	public List<Double> getKNearestMahalanobis(Map<FeatureSpace, DoubleMatrix> points, int k)
	{
		List<Double> results = new ArrayList<Double>();
		
		Map<FeatureSpace, DoubleMatrix> precisionMatrices = getPrecisionForAllFeatures();
		for (PerceptCluster pc : perceptClusters.values())
		{
			for (FeatureSpace featureSpace : pc.getPercepts().keySet())
			{
				if (pc.getPercepts().containsKey(featureSpace) && points.containsKey(featureSpace))
				{
					DoubleMatrix difference = points.get(featureSpace).sub(
												pc.getPercepts().get(featureSpace).getValue());

					DoubleMatrix left = difference.transpose().mmul(precisionMatrices.get(featureSpace));

					double scaledDistance = Math.sqrt(left.mmul(difference).get(0));
					results.add(scaledDistance);
				}
					
			}
		}
		Collections.sort(results);
		return results.subList(0, Math.min(k, results.size()));		
	}
	
	public double getSquaredMahalanobisToMean(FeatureSpace feature, DoubleMatrix point)
	{
		
		DoubleMatrix mean = getMeanForFeature(feature);
		
		Map<FeatureSpace, DoubleMatrix> precisionMatrices = getPrecisionForAllFeatures();

		DoubleMatrix difference = point.sub(mean);
		DoubleMatrix left = difference.transpose().mmul(precisionMatrices.get(feature));
		double distance = left.mmul(difference).get(0);

		return distance;
	}
	

	/**
	 * Get a list of distances to the closest points labeled with this abstract descriptor
	 * @param points A map from FeatureSpaces to the point to be tested for that feature
	 * @param k Number of 
	 * @param totalNumberOfPerceptClusters The number of percept clusters in all classes or properties
	 * @return The closest points that are labeled with this AbstractDescriptor
	 */
	public List<Double> getKNearestMahalanobisWeighted(Map<FeatureSpace, DoubleMatrix> points, int k, int totalNumberOfPerceptClusters)
	{
		return getKNearestMahalanobisWeighted(points,k,totalNumberOfPerceptClusters,true);	
	}
	
	/**
	 * Get a list of distances to the closest points labeled with this abstract descriptor
	 * @param points A map from FeatureSpaces to the point to be tested for that feature
	 * @param k Number of 
	 * @param totalNumberOfPerceptClusters The number of percept clusters in all classes or properties
	 * @return The closest points that are labeled with this AbstractDescriptor
	 */
	public List<Double> getKNearestMahalanobisWeighted(Map<FeatureSpace, DoubleMatrix> points, int k, int totalNumberOfPerceptClusters,
			boolean positiveExamples)
	{
		List<Double> results = new ArrayList<Double>();
		
		Map<FeatureSpace, DoubleMatrix> precisionMatrices = getPrecisionForAllFeatures();
		
		Collection<PerceptCluster> perceptClustersToTest;
		if (positiveExamples)
			perceptClustersToTest = perceptClusters.values();
		else
			perceptClustersToTest = negativePerceptClusters.values();
		
		for (PerceptCluster pc : perceptClustersToTest)
		{
			double totalDistance = 0.0d;
			
			// The feature ratios aren't important, just getting the feature names
			for (FeatureSpace featureSpace : pc.getPercepts().keySet())
			{
				if (pc.getPercepts().containsKey(featureSpace) && points.containsKey(featureSpace))
				{
					DoubleMatrix difference = points.get(featureSpace).sub(
												pc.getPercepts().get(featureSpace).getValue());

					DoubleMatrix left = difference.transpose().mmul(precisionMatrices.get(featureSpace));

					double scaledDistance = Math.sqrt(left.mmul(difference).get(0));
					int dimension = points.get(featureSpace).length;
					// Scaling according to Brown and Kopolowitz (1979)
					scaledDistance = scaledDistance * Math.pow(((double)perceptClusters.size()) / 
															totalNumberOfPerceptClusters , 1.0d / dimension);
					
					totalDistance += scaledDistance;
					//results.add(scaledDistance);
				}
				else
				{
					totalDistance = Double.MAX_VALUE;
					break;
				}
				
				results.add(totalDistance);	
			}
		}
		Collections.sort(results);
		return results.subList(0, Math.min(k, results.size()));		
	}
	
	public List<Double> getKNearestMahalanobisWeightedCombined(Map<FeatureSpace, DoubleMatrix> points, int k, int totalNumberOfPerceptClusters)
	{
		List<Double> results = new ArrayList<Double>();
		
		TreeMap<FeatureSpace, DoubleMatrix> sortedPoints = new TreeMap<FeatureSpace, DoubleMatrix>(points);
		// Flatten features into one vector
		int dimensions = 0;
		
		for (DoubleMatrix v : sortedPoints.values())
		{
			dimensions += v.length;
		}
		
		DoubleMatrix combined = null;

		for (DoubleMatrix v : points.values())
		{
			if (combined == null)
				combined = v;
			else
				combined = DoubleMatrix.concatVertically(combined, v);
		}
		
		// Find closest perceptClusters
		
		DoubleMatrix precisionMatrix = getPrecisionForAllData();
		for (PerceptCluster pc : perceptClusters.values())
		{
			double totalDistance = 0.0d;
			
			DoubleMatrix pcCombined = pc.combinedMatrix();
			

			DoubleMatrix difference = combined.sub(pcCombined);

			DoubleMatrix left = difference.transpose().mmul(precisionMatrix);

			double scaledDistance = Math.sqrt(left.mmul(difference).get(0));

			// Scaling according to Brown and Kopolowitz (1979)
			scaledDistance = scaledDistance * Math.pow(((double)perceptClusters.size()) / 
											totalNumberOfPerceptClusters , 1.0d / dimensions);
					
			totalDistance += scaledDistance;
					//results.add(scaledDistance);

			results.add(totalDistance);
					
			
		}
		Collections.sort(results);
		return results.subList(0, Math.min(k, results.size()));		
	}
	
	
	public List<Double> getKNearestWeighted(Map<FeatureSpace, DoubleMatrix> points, 
			Map<FeatureSpace,Double> minDistances, Map<FeatureSpace,Double> maxDistances, int k, double maxRatio)
	{
		List<Double> results = new ArrayList<Double>();
		for (PerceptCluster pc : perceptClusters.values())
		{
			for (FeatureSpace featureSpace : pc.getPercepts().keySet())
			{
				if (pc.getPercepts().containsKey(featureSpace) && points.containsKey(featureSpace) && featureRatios.get(featureSpace) < maxRatio)
				{
					double scaledDistance = points.get(featureSpace).distance2(pc.getPercepts().get(featureSpace).getValue()) -
							minDistances.get(featureSpace);
					scaledDistance = scaledDistance / (maxDistances.get(featureSpace) - minDistances.get(featureSpace));
					results.add(scaledDistance);
				}
					
			}
		}
		Collections.sort(results);
		return results.subList(0, Math.min(k, results.size()));
	}
	
	public Map<FeatureSpace,Double> 
	getMinimumDistances(Map<FeatureSpace,DoubleMatrix> features)
	{
		HashMap<FeatureSpace,Double> result = new HashMap<FeatureSpace, Double>();
		
		for (FeatureSpace featureSpace : features.keySet())
		{
			result.put(featureSpace, getMinimumDistanceForFeature(featureSpace, features.get(featureSpace)));
			
		}
		return result;
		
	}
	
	public double getAverageDistanceForFeature(FeatureSpace featureSpace, DoubleMatrix point)
	{
		double distance = 0f;
		int numPerceptClusters = 0;
		for (PerceptCluster pc : perceptClusters.values())
		{
			if (pc.getPercepts().containsKey(featureSpace))
			{
				numPerceptClusters++;
				distance += pc.featureDistance(featureSpace, point);
			}
		}
		
		return distance / numPerceptClusters;
	}
	
	public int getTotalNumberOfDimensions()
	{
		int dimensions = 0;
		for (PerceptCluster pc : perceptClusters.values())
		{
			dimensions = pc.getNumberOfDimensions();
			if (dimensions > 0)
				return dimensions;
				
		}
		return -1;
	}

	
	public String toString()
	{
		
		String result = name + "\n";
		result += utterances.size() + " utterances.\n";
		result += perceptClusters.size() + " percept clusters.\n";
		Map<FeatureSpace, DoubleMatrix> meanValues = getMeanForAllFeatures();
		Map<FeatureSpace, DoubleMatrix> utteranceMeanValues = getUtteranceMeanForAllFeatures();
		result += "Mean values:\n";
		for (FeatureSpace featureSpace : meanValues.keySet())
		{
			result += " " + featureSpace.getName() + " : " + utteranceMeanValues.get(featureSpace) + "\n";
		}
		result += "Feature ratios:\n";
		for (FeatureSpace featureSpace : featureRatios.keySet())
		{
			result += " " + featureSpace.getName() + " : " + featureRatios.get(featureSpace) + "\n";
		}
		
		return result;
	}
	
	public String getFileSafeName()
	{
		String[] colonSplit = name.split(":");
		String safeName = colonSplit[colonSplit.length-1];
		safeName = safeName.split("\\)")[0];
		return safeName;
	}
	
	private Set<FeatureSpace> getFeatureSpaces()
	{
		Set<FeatureSpace> featureSpaces = null;
		
		for (PerceptCluster pc : perceptClusters.values())
		{
			featureSpaces = pc.getPercepts().keySet();
			if (featureSpaces != null && featureSpaces.size() > 0)
				break;
		}
		
		return featureSpaces;
	}
	
	private SpaceManager getSpaceManager()
	{
		Set<FeatureSpace> featureSpaces = getFeatureSpaces();
		
		for (FeatureSpace fs : featureSpaces)
		{
			return fs.getSpaceManager();
		}
		
		return null;
	}

	public HashSet<WorldObject> getExamples() {
		return examples;
	}
	
	public void addExample(WorldObject wo)
	{
		examples.add(wo);
	}
	
	public void removeExample(WorldObject wo)
	{
		examples.remove(wo);
	}
	

}
