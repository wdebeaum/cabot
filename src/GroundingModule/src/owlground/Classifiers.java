package owlground;

import java.util.HashMap;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.jblas.DoubleMatrix;

import owlground.language.Word;
import owlground.objects.ObjectClass;
import owlground.objects.ObjectModel;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.perception.Percept;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;

public class Classifiers {
	
	private static boolean USE_MEDIAN_SUBBLOB_CLASSES = true;
	/* 
	 * Use the feature data that cooccurs with this WorldObject
	 * to generate the class of the object. 
	 * @param wo The world object to generate properties for
	 */
	public static void generatePerceivedObjectClass(WorldManager wm, WorldObject wo)
	{
		// Get range of distance results for all objects to normalize
		HashMap<FeatureSpace, Double> minDistances = new HashMap<FeatureSpace, Double>();
		HashMap<FeatureSpace, Double> maxDistances = new HashMap<FeatureSpace, Double>();
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			Map<FeatureSpace, Double> distances = oc.getMinimumDistances(wo.getAveragePerceptValues());
			for (FeatureSpace featureSpace: distances.keySet())
			{
				if (!minDistances.containsKey(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) < minDistances.get(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				
				if (!maxDistances.containsKey(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) > maxDistances.get(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
				
			}
			
		}
		double minDistance = Double.MAX_VALUE;
		ObjectClass bestObjectClass = null;
		
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			if (oc.updatePerceptClusters() == 0)
				continue;
			
			double totalDistance = 0.0f;
			for (FeatureSpace featureSpace : oc.getRepresentativeFeatures())
			{
				double scaledDistance = oc.getMinimumDistanceForFeature(featureSpace,
											oc.getMeanForFeature(featureSpace)) - 
				minDistances.get(featureSpace);
				scaledDistance = scaledDistance / 
						(maxDistances.get(featureSpace) - minDistances.get(featureSpace));
				totalDistance += scaledDistance;
			}
			
			double featureScaledDistance = totalDistance / oc.getRepresentativeFeatures().size();
			if (featureScaledDistance < minDistance)
			{
				minDistance = featureScaledDistance;
				bestObjectClass = oc;
			}
		}
		if (bestObjectClass != null)
			wo.setObjectClass(bestObjectClass);
	}
	
	/*
	 * Choose the property according to K-nearest neighbor - deprecated because it automatically loads
	 * closest blob to hand.
	 * Features are prechosen. 
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	@Deprecated
	public static float generatePerceivedPropertiesWithKNearestDistanceAndPrechosenFeatures(WorldManager wm, 
			WorldObject wo, int minUtterances, int k)
	{
		final String NO_PROPERTY = "NO_PROPERTY";
		// Average over the frames the object is seen in
		wo.updatePerceptClustersToAttended();
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		float confidence = 0f;
		Map<String, FeatureSpace> representativeFeatureSpaceMap = Property.getRepresentativeFeatureSpaceMap(wo.getSession());
		for (FeatureSpace featureSpace: new HashSet<FeatureSpace>(representativeFeatureSpaceMap.values()))
		{
			TreeMap<Double, String> tempDistancePropertyNameMapping = new TreeMap<Double, String>();
			DoubleMatrix worldObjectPoint = averagePerceptValues.get(featureSpace);
			if (worldObjectPoint == null)
				continue;
			for (FeatureManager fm : wm.getSessions())
			{
				for (PerceptCluster pc: fm.getAttendedPerceptClusters().values())
				{
					HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
					Percept otherPercept = otherPercepts.get(featureSpace);
					DoubleMatrix otherPoint = otherPercept.getValue();
					double distance = worldObjectPoint.distance2(otherPoint);
					//System.out.println("Utterance: \n" + pc.getUtterance());
					for (WorldObject otherWo : pc.getUtterance().getWorldObjects())
					{
						if (otherWo == null)
							continue;
						String matchingPropertyName = NO_PROPERTY;
						
						// Check to see if this point has a label corresponding to this feature space
						for (String propertyName : otherWo.getProperties())
						{
							if (representativeFeatureSpaceMap.containsKey(propertyName) && 
									representativeFeatureSpaceMap.get(propertyName).equals(featureSpace))
							{
								matchingPropertyName = propertyName;
								break;
							}
						}
						 
						if (matchingPropertyName.equals(NO_PROPERTY) || 
								wm.getProperties().get(matchingPropertyName).getUtterances().size() >= minUtterances)
							tempDistancePropertyNameMapping.put(distance, matchingPropertyName);
					}
					
				}
			}
			Map<String, Double> votingResults = new HashMap<String, Double>();
	
			int i = 0;
			for (Entry<Double,String> e : tempDistancePropertyNameMapping.entrySet())
			{
				if (i >= k)
					break;

				String propertyName = e.getValue();

				if (!votingResults.containsKey(propertyName))
					votingResults.put(propertyName,0.);
				
				double weight = 1.0f / e.getKey();
				if (propertyName.equals(NO_PROPERTY))
					weight = weight / k;
				votingResults.put(propertyName, votingResults.get(propertyName) + weight);			
				i++;
			}

			TreeMap<Double, String> sortedVotingResults = new TreeMap<Double, String>();
			
			for (String propertyName : votingResults.keySet())
			{
				sortedVotingResults.put(votingResults.get(propertyName),propertyName);
			}
			
			if (!sortedVotingResults.lastEntry().getValue().equals(NO_PROPERTY))
				wo.getPerceivedProperties().add(sortedVotingResults.lastEntry().getValue());
			
		}
		return confidence;

	}

	/*
	 * Choose the property according to K-nearest neighbor - deprecated because it automatically loads
	 * closest blob to hand.
	 * Features are prechosen. 
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */

	public static float generatePerceivedPropertiesWithKNearestPrechosen(WorldManager wm, 
			WorldObject wo, int minUtterances, int k)
	{
		final String NO_PROPERTY = "NO_PROPERTY";
		// Average over the frames the object is seen in
		//wo.updatePerceptClustersToAttended();
		
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		//System.out.println("Number of features: " + wo.getAveragePerceptValues());
		float confidence = 0f;
		Map<String, FeatureSpace> representativeFeatureSpaceMap = Property.getRepresentativeFeatureSpaceMap(wo.getSession());
		//System.out.println("Representative Feature space map: " + representativeFeatureSpaceMap);
		// Go through different features finding ones to classify by
		for (FeatureSpace featureSpace: new HashSet<FeatureSpace>(representativeFeatureSpaceMap.values()))
		{
			//System.out.println("Feature space: " + featureSpace);
			TreeMap<Double, String> tempDistancePropertyNameMapping = new TreeMap<Double, String>();
			//TreeMap<Double, String> tempNegativeDistancePropertyNameMapping = new TreeMap<Double,String>();
			DoubleMatrix worldObjectPoint = averagePerceptValues.get(featureSpace);
			if (worldObjectPoint == null)
				continue;
			for (Property property : wm.getProperties().values())
			{
				//System.out.println("Testing property: " + property + " " + property.getPerceptClusters().size() + " PC's");
				for (PerceptCluster pc: property.getPerceptClusters().values())
				{
					
					HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
					Percept otherPercept = otherPercepts.get(featureSpace);
					DoubleMatrix otherPoint = otherPercept.getValue();
					double distance = worldObjectPoint.distance2(otherPoint);

					for (WorldObject otherWo : pc.getUtterance().getWorldObjects())
					{
						if (otherWo == null)
							continue;
						String matchingPropertyName = NO_PROPERTY;
						
						// Check to see if this point has a label corresponding to this feature space
						if (representativeFeatureSpaceMap.containsKey(property.getName()) &&
								representativeFeatureSpaceMap.get(property.getName()).getName().equals(featureSpace.getName()))
							matchingPropertyName = property.getName();
						 
						if (matchingPropertyName.equals(NO_PROPERTY) || 
								wm.getProperties().get(matchingPropertyName).getUtterances().size() >= minUtterances)
							tempDistancePropertyNameMapping.put(distance, matchingPropertyName);
					}
				}
				// For negative properties, add negative weights
				for (PerceptCluster pc : property.getNegativePerceptClusters().values())
				{
					HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
					Percept otherPercept = otherPercepts.get(featureSpace);
					DoubleMatrix otherPoint = otherPercept.getValue();
					double distance = worldObjectPoint.distance2(otherPoint);

					for (WorldObject otherWo : pc.getUtterance().getWorldObjects())
					{
						if (otherWo == null)
							continue;
						String matchingPropertyName = NO_PROPERTY;
						
						// Check to see if this point has a label corresponding to this feature space
						if (representativeFeatureSpaceMap.containsKey(property.getName()) &&
								representativeFeatureSpaceMap.get(property.getName()).getName().equals(featureSpace.getName()))
							matchingPropertyName = property.getName();
						 
						if (!matchingPropertyName.equals(NO_PROPERTY) &&
								wm.getProperties().get(matchingPropertyName).getUtterances().size() >= minUtterances)
							tempDistancePropertyNameMapping.put(distance, "!" + matchingPropertyName);
					}					
				}
			}
			Map<String, Double> votingResults = new HashMap<String, Double>();
	
			int index = 0;

			for (Entry<Double,String> e : tempDistancePropertyNameMapping.entrySet())
			{
				if (index >= k)
					break;

				String propertyName = e.getValue();
				String absolutePropertyName = propertyName.replace("!","");
				if (!votingResults.containsKey(absolutePropertyName))
					votingResults.put(absolutePropertyName,0.);
				
				double weight = 1.0f / e.getKey();
				if (propertyName.equals(NO_PROPERTY))
					weight = weight / k;
				if (propertyName.contains("!"))
					votingResults.put(propertyName, votingResults.get(absolutePropertyName) - weight);
				else
					votingResults.put(propertyName, votingResults.get(propertyName) + weight);			
				index++;
			}

			TreeMap<Double, String> sortedVotingResults = new TreeMap<Double, String>();
			
			for (String propertyName : votingResults.keySet())
			{
				sortedVotingResults.put(votingResults.get(propertyName),propertyName);
			}
			//System.out.println("Sorted results");
			//System.out.println(sortedVotingResults);
			
			if (sortedVotingResults.isEmpty())
				continue;
			
			if (!sortedVotingResults.lastEntry().getValue().equals(NO_PROPERTY))
				wo.getPerceivedProperties().add(sortedVotingResults.lastEntry().getValue());
			
		}
		return confidence;

	}
	

	/*
	 * Choose the property according to K-nearest neighbor
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */	
	public static void generatePerceivedPropertiesWithKNearestDistance(WorldManager wm, WorldObject wo, int minUtterances, int k)
	{
		HashMap<FeatureSpace, HashSet<Property>> representativeProperties = new HashMap<FeatureSpace, HashSet<Property>>();
		
		// Get representative properties and store them in the Map
		for (Property p: wm.getProperties().values())
		{
			if (p.updatePerceptClusters() == 0)
				continue;
			
			if (p.getRepresentativeFeature() == null)
				continue;
			
			if (!representativeProperties.containsKey(p.getRepresentativeFeature()))
				representativeProperties.put(p.getRepresentativeFeature(), new HashSet<Property>());
			
			representativeProperties.get(p.getRepresentativeFeature()).add(p);
		}
		
		final String NO_PROPERTY = "NO_PROPERTY";
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		for (FeatureSpace featureSpace: representativeProperties.keySet())
		{
			TreeMap<Double, String> tempDistancePropertyNameMapping = new TreeMap<Double, String>();
			DoubleMatrix worldObjectPoint = averagePerceptValues.get(featureSpace);
			if (worldObjectPoint == null)
				continue;
			for (FeatureManager fm : wm.getSessions())
			{
				for (PerceptCluster pc: fm.getAttendedPerceptClusters().values())
				{
					HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
					Percept otherPercept = otherPercepts.get(featureSpace);
					DoubleMatrix otherPoint = otherPercept.getValue();
					double distance = worldObjectPoint.distance2(otherPoint);
					//System.out.println("Utterance: \n" + pc.getUtterance());
					
					// If something goes wrong, check here
					for (WorldObject otherWo : pc.getUtterance().getWorldObjects())
					{
						if (otherWo == null)
							continue;
						String matchingPropertyName = NO_PROPERTY;
						for (String propertyName : otherWo.getProperties())
						{
							if (wm.getProperties().get(propertyName).getRepresentativeFeature() != null && 
									wm.getProperties().get(propertyName).getRepresentativeFeature().equals(featureSpace))
							{
								matchingPropertyName = propertyName;
								break;
							}
						}
						 
						if (matchingPropertyName.equals(NO_PROPERTY) || 
								wm.getProperties().get(matchingPropertyName).getUtterances().size() >= minUtterances)
							tempDistancePropertyNameMapping.put(distance, matchingPropertyName);
					}
					
				}
			}
			Map<String, Double> votingResults = new HashMap<String, Double>();
	
			int i = 0;
			for (Entry<Double,String> e : tempDistancePropertyNameMapping.entrySet())
			{
				if (i >= k)
					break;

				String propertyName = e.getValue();

				if (!votingResults.containsKey(propertyName))
					votingResults.put(propertyName,0.);
				
				double weight = 1.0 / e.getKey();
				if (propertyName.equals(NO_PROPERTY))
					weight = weight / k;
				votingResults.put(propertyName, votingResults.get(propertyName) + weight);			
				i++;
			}

			TreeMap<Double, String> sortedVotingResults = new TreeMap<Double, String>();
			
			for (String propertyName : votingResults.keySet())
			{
				sortedVotingResults.put(votingResults.get(propertyName),propertyName);
			}
			
			if (!sortedVotingResults.lastEntry().getValue().equals(NO_PROPERTY))
				wo.getPerceivedProperties().add(sortedVotingResults.lastEntry().getValue());
			
		}

	}
	
	/*
	 * Choose the property according to K-nearest neighbor across multiple feature spaces normalized 
	 * according to their min and max extents.
	 * Features are prechosen. 
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedPropertiesWithMultiKNearestDistance(WorldManager wm, WorldObject wo, 
			int minUtterances, int k, double featureThreshold)
	{
		HashMap<FeatureSpace, Double> minDistances = new HashMap<FeatureSpace, Double>();
		HashMap<FeatureSpace, Double> maxDistances = new HashMap<FeatureSpace, Double>();
		for (Property p : wm.getProperties().values())
		{
			Map<FeatureSpace, Double> distances = p.getMinimumDistances(wo.getAveragePerceptValues());
			for (FeatureSpace featureSpace: distances.keySet())
			{
				if (!minDistances.containsKey(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) < minDistances.get(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				
				if (!maxDistances.containsKey(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) > maxDistances.get(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
			}
			
		}
		
		TreeMap<Double, Property> distancePropertyMapping = new TreeMap<Double, Property>();
		Map<Property, Double> votingResults = new HashMap<Property, Double>();

		for (Property p : wm.getProperties().values())
		{
			if (p.getUtterances().size() < minUtterances)
				continue;
			
			List<Double> pDistances = p.getKNearestWeighted(wo.getAveragePerceptValues(), 
								minDistances, maxDistances, k, featureThreshold);
			for (double distance : pDistances)
			{
				distancePropertyMapping.put(distance,p);
			}
			
		}
		
		for (double distance : distancePropertyMapping.navigableKeySet())
		{
			if (!votingResults.containsKey(distancePropertyMapping.get(distance)))
				votingResults.put(distancePropertyMapping.get(distance),0.);
			
			double weight = distancePropertyMapping.get(distance).getPerceptClusters().size();
			
			votingResults.put(distancePropertyMapping.get(distance), 
					votingResults.get(distancePropertyMapping.get(distance)) + weight);
				
		}
		
		TreeMap<Double, Property> sortedVotingResults = new TreeMap<Double, Property>();
		
		for (Property p: votingResults.keySet())
		{
			sortedVotingResults.put(votingResults.get(p),p);
		}
		
		if (sortedVotingResults.size() != 0)
			wo.getPerceivedProperties().add(sortedVotingResults.lastEntry().getValue().getName());

	}
	
	/*
	 * Choose the object class as the category with the smallest mean distance of points to the given WorldObject.
	 * Features are prechosen. 
	 * @param wo The world object to generate properties for
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedObjectClassWithMeanDistanceAndPrechosenFeatures(WorldManager wm, WorldObject wo, int minUtterances)
	{	
		
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		
		Map<FeatureSpace, Double> scaledSumOfSquares = wm.getScaledSumOfSquares();
	
		// Get representative properties and store them in the Map
		double bestObjectClassRatio = .8f;
		ObjectClass bestObjectClass = null;
		
		Map<String, FeatureSpace> representativeFeatureSpaceMap = ObjectClass.getRepresentativeFeatureSpaceMap(wo.getSession());
		
		for (ObjectClass oc: wm.getObjectClasses().values())
		{
			if (oc.updatePerceptClusters() == 0)
				continue;
			
			if (oc.getUtterances().size() < minUtterances)
				continue;
			
			if (!representativeFeatureSpaceMap.containsKey(oc.getName()))
				continue;
			
			// Retrieve the representative feature for the objectClass
			FeatureSpace featureSpace = representativeFeatureSpaceMap.get(oc.getName());
			
			double ratio = (double) (oc.getAverageDistanceForFeature(featureSpace, averagePerceptValues.get(featureSpace)) /
								scaledSumOfSquares.get(featureSpace));
			oc.getFeatureRatios().put(featureSpace, ratio);
			if (ratio < bestObjectClassRatio)
			{
				bestObjectClassRatio = ratio;
				bestObjectClass = oc;
			}
		}
		
		if (bestObjectClass != null)
		{
			wo.setObjectClass(bestObjectClass);
		}
			
	}

	
	/* 
	 * Use the feature data that cooccurs with this WorldObject
	 * to generate propertes of the object. 
	 * @param wo The world object to generate properties for
	 */
	public static void generatePerceivedProperties(WorldManager wm, WorldObject wo)
	{
		HashMap<FeatureSpace, HashSet<Property>> representativeProperties = new HashMap<FeatureSpace, HashSet<Property>>();
		
		// Get representative properties and store them in the Map
		for (Property p: wm.getProperties().values())
		{
			// Skip if there are not corresponding percept clusters
			if (p.updatePerceptClusters() == 0)
				continue;
			
			if (p.getRepresentativeFeature() == null)
				continue;
			
			if (!representativeProperties.containsKey(p.getRepresentativeFeature()))
				representativeProperties.put(p.getRepresentativeFeature(), new HashSet<Property>());
			
			representativeProperties.get(p.getRepresentativeFeature()).add(p);
		}
		
		// Just prints out the properties
		for (FeatureSpace s : representativeProperties.keySet())
		{
			if (s == null || s.equals("null"))
				continue;
			System.out.println(s);
			for (Property p: representativeProperties.get(s))
			{
				System.out.println("Average value: " + p.getMeanForFeature(p.getRepresentativeFeature()));
			}
		}
		
		// Find closest matchings
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		
		for (FeatureSpace s : averagePerceptValues.keySet())
		{
			System.out.println(s);
			System.out.println(averagePerceptValues.get(s));
		}
		
		for(FeatureSpace featureSpace : representativeProperties.keySet())
		{
			if (featureSpace == null)
				continue;
			
			double minDistance = Double.MAX_VALUE;
			Property bestProperty = null;
			for (Property p: representativeProperties.get(featureSpace))
			{
				
				 double distance = p.getMinimumDistanceForFeature(featureSpace, averagePerceptValues.get(featureSpace));

				 if (distance < minDistance)
				 {
					 minDistance = distance;
					 bestProperty = p;
				 }
			}
			if (bestProperty != null)
				wo.getPerceivedProperties().add(bestProperty.getName());
		}

	}
	/*
	 * Choose the property as the one with the smallest mean distance of points to the given WorldObject.
	 * Features are prechosen. 
	 * @param wo The world object to generate properties for
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedPropertiesWithMeanDistanceAndPrechosenFeatures(
			WorldManager wm, WorldObject wo, int minUtterances, int descriptionLength, double featureDistanceThreshold)
	{	
		
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		Map<FeatureSpace, Double> scaledSumOfSquares = wm.getScaledSumOfSquares();
		Map<String, FeatureSpace> representativeFeatureSpaceMap = Property.getRepresentativeFeatureSpaceMap(wo.getSession());
		
		System.out.println("Values for WorldObject " + wo.getObjectId());
		for (FeatureSpace featureSpace : averagePerceptValues.keySet())
		{
			System.out.println(featureSpace + ": " + averagePerceptValues.get(featureSpace));
		}
		
		
		for (int i = 0; i < descriptionLength; i++)
		{
			
			// Get representative properties and store them in the Map
			double bestPropertyRatio = featureDistanceThreshold;
			Property bestProperty = null;
			
			for (Property p: wm.getProperties().values())
			{
				if (p.updatePerceptClusters() == 0)
					continue;
				
				if (p.getUtterances().size() < minUtterances)
					continue;
				
				if (!Property.representativeFeatureMap.containsKey(p.getName()))
					continue;
				
				FeatureSpace featureSpace = representativeFeatureSpaceMap.get(p.getName());
				
				double ratio = (double) (p.getAverageDistanceForFeature(featureSpace, averagePerceptValues.get(featureSpace)) /
									scaledSumOfSquares.get(featureSpace));
				p.getFeatureRatios().put(featureSpace, ratio);
				if (ratio < bestPropertyRatio && !wo.getPerceivedProperties().contains(p.getName()))
				{
					bestPropertyRatio = ratio;
					bestProperty = p;
				}
			}
			
			if (bestProperty != null)
			{
				wo.getPerceivedProperties().add(bestProperty.getName());
			}
			
		}
	}
	/*
	 * Choose the object name according to K-nearest neighbor using Mahalanobis Distance instead of Euclidean
	 * distance.
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedObjectClassWithKNearestMahalanobisDistance(WorldManager wm, WorldObject wo, int minUtterances, int k)
	{
		TreeMap<Double, String> distancePropertyMapping = new TreeMap<Double, String>();
		Map<String, Double> votingResults = new HashMap<String, Double>();
		Map<String, ObjectClass> objectClasses = new HashMap<String, ObjectClass>();
		
		int totalNumObjectClassPerceptClusters = wm.getNumberOfObjectClassPerceptClusters();
		
		System.out.println("Number of object classes: " + wm.getObjectClasses().size());
		
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			if (oc.getUtterances().size() < minUtterances || oc.getName().equals(ObjectClass.defaultClass) || ObjectClass.isFalseWord(oc))
				continue;
			
			if (oc.getName().equals(ObjectClass.nullClass))
				continue;
			objectClasses.put(oc.getName(), oc);

			List<Double> pDistances = oc.getKNearestMahalanobisWeighted(wo.getAveragePerceptValues(), k, 
					totalNumObjectClassPerceptClusters);

			for (double distance : pDistances)
			{
				// Distance is closest feature
				distancePropertyMapping.put(distance,oc.getName());
			}
		
			List<Double> pNegativeDistances = oc.getKNearestMahalanobisWeighted(wo.getAveragePerceptValues(), k, 
					totalNumObjectClassPerceptClusters, false);

			for (double distance : pNegativeDistances)
			{
				// Distance is closest feature
				distancePropertyMapping.put(distance,"!" + oc.getName());
			}
			
		}
		//for (double distance : distancePropertyMapping.navigableKeySet())
		for (Entry<Double,String> entry: distancePropertyMapping.entrySet())
		{
			String ocName = entry.getValue();
			String absoluteOcName = ocName.replace("!", "");
		
			if (!votingResults.containsKey(absoluteOcName))
				votingResults.put(absoluteOcName,0.);
			
			double weight = 1.0 / entry.getKey();
			
			// Flip if it's a negative example
			if (ocName.startsWith("!"))
				weight = -weight;
			
			votingResults.put(absoluteOcName, votingResults.get(absoluteOcName) + weight);
				
		}
		
		TreeMap<Double, String> sortedVotingResults = new TreeMap<Double, String>();
		
		for (String objectClassName: votingResults.keySet())
		{
			if (!objectClassName.equals(ObjectClass.nullClass))
				sortedVotingResults.put(votingResults.get(objectClassName),objectClassName);
		}
		
		if (sortedVotingResults.size() != 0)
		{
			wo.setObjectClass(objectClasses.get(sortedVotingResults.lastEntry().getValue()));
		}
		else
		{
			System.out.println("Could not find a valid class name label");
		}
		
	}
	
	/*
	 * Choose the property according to K-nearest neighbor using Mahalanobis Distance instead of Euclidean
	 * distance.
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedObjectClassWithKNearestMahalanobisDistanceCombined(WorldManager wm, WorldObject wo, int minUtterances, int k)
	{
		TreeMap<Double, ObjectClass> distancePropertyMapping = new TreeMap<Double, ObjectClass>();
		Map<ObjectClass, Double> votingResults = new HashMap<ObjectClass, Double>();
		
		int totalNumObjectClassPerceptClusters = wm.getNumberOfObjectClassPerceptClusters();
		
		System.out.println("Number of object classes: " + wm.getObjectClasses().size());
		
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			if (oc.getUtterances().size() < minUtterances)
				continue;
			//List<Double> pDistances = oc.getKNearestMahalanobis(wo.getAveragePerceptValues(), k);
			List<Double> pDistances = oc.getKNearestMahalanobisWeightedCombined(wo.getAveragePerceptValues(), k, 
					totalNumObjectClassPerceptClusters);
			double totalDistance = 0.0d;
			for (double distance : pDistances)
			{
				// Distance is closest feature
				distancePropertyMapping.put(distance,oc);
				
				//totalDistance += distance;
			}
			// Distance is sum of weighted features
			//distancePropertyMapping.put(totalDistance,oc);
		}
		
		for (double distance : distancePropertyMapping.navigableKeySet())
		{
			if (!votingResults.containsKey(distancePropertyMapping.get(distance)))
				votingResults.put(distancePropertyMapping.get(distance),0.);
			
			double weight = 1.0 / distance;
				
			
			votingResults.put(distancePropertyMapping.get(distance), 
					votingResults.get(distancePropertyMapping.get(distance)) + weight);
				
		}
		
		TreeMap<Double, ObjectClass> sortedVotingResults = new TreeMap<Double, ObjectClass>();
		
		for (ObjectClass oc: votingResults.keySet())
		{
			sortedVotingResults.put(votingResults.get(oc),oc);
		}
		
		if (sortedVotingResults.size() != 0)
			wo.setObjectClass(sortedVotingResults.lastEntry().getValue());
		
	}
	
	/*
	 * Choose the object class according to K-nearest neighbor 
	 * @param wo The world object to generate properties for
	 * @param k The k-value for k-nearest neighbor
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedObjectClassWithKNearestDistance(WorldManager wm, WorldObject wo, int minUtterances, int k, double featureThreshold)
	{
		HashMap<FeatureSpace, Double> minDistances = new HashMap<FeatureSpace, Double>();
		HashMap<FeatureSpace, Double> maxDistances = new HashMap<FeatureSpace, Double>();
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			Map<FeatureSpace, Double> distances = oc.getMinimumDistances(wo.getAveragePerceptValues());
			for (FeatureSpace featureSpace: distances.keySet())
			{
				if (!minDistances.containsKey(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) < minDistances.get(featureSpace))
					minDistances.put(featureSpace, distances.get(featureSpace));
				
				if (!maxDistances.containsKey(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
				else if (distances.get(featureSpace) > maxDistances.get(featureSpace))
					maxDistances.put(featureSpace, distances.get(featureSpace));
			}
			
		}
		
		TreeMap<Double, ObjectClass> distancePropertyMapping = new TreeMap<Double, ObjectClass>();
		Map<ObjectClass, Double> votingResults = new HashMap<ObjectClass, Double>();

		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			if (oc.getUtterances().size() < minUtterances)
				continue;
			
			List<Double> pDistances = oc.getKNearestWeighted(wo.getAveragePerceptValues(), 
								minDistances, maxDistances, k, featureThreshold);
			for (double distance : pDistances)
			{
				distancePropertyMapping.put(distance,oc);
			}
			
		}
		
		for (double distance : distancePropertyMapping.navigableKeySet())
		{
			if (!votingResults.containsKey(distancePropertyMapping.get(distance)))
				votingResults.put(distancePropertyMapping.get(distance),0.);
			
			double weight = 1.0 / distance;
			
			votingResults.put(distancePropertyMapping.get(distance), 
					votingResults.get(distancePropertyMapping.get(distance)) + weight);
				
		}
		
		TreeMap<Double, ObjectClass> sortedVotingResults = new TreeMap<Double, ObjectClass>();
		
		for (ObjectClass oc: votingResults.keySet())
		{
			sortedVotingResults.put(votingResults.get(oc),oc);
		}
		
		if (sortedVotingResults.size() != 0)
			wo.setObjectClass(sortedVotingResults.lastEntry().getValue());
		
		
		
	}
	/*
	 * Choose the property as the one with the smallest mean distance of points to the given WorldObject.
	 * @param wo The world object to generate properties for
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedPropertiesWithMeanDistance(WorldManager wm, WorldObject wo, int minUtterances, int descriptionLength)
	{	
		
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		Map<FeatureSpace, Double> scaledSumOfSquares = wm.getScaledSumOfSquares();
		
		for (int i = 0; i < descriptionLength; i++)
		{
			
			// Get representative properties and store them in the Map
			double bestPropertyRatio = Double.MAX_VALUE;
			Property bestProperty = null;
			
			for (Property p: wm.getProperties().values())
			{
				if (p.updatePerceptClusters() == 0)
					continue;
				
//				if (p.getRepresentativeFeature() == null)
//					continue;
				
				if (p.getUtterances().size() < minUtterances)
					continue;

				
				for (FeatureSpace featureSpace : averagePerceptValues.keySet())
				{
					double ratio = (double) (p.getAverageDistanceForFeature(featureSpace, averagePerceptValues.get(featureSpace)) /
									scaledSumOfSquares.get(featureSpace));
					p.getFeatureRatios().put(featureSpace, ratio);
					if (ratio < bestPropertyRatio && !wo.getPerceivedProperties().contains(p.getName()))
					{
						bestPropertyRatio = ratio;
						bestProperty = p;
					}
				}
			}
			
			if (bestProperty != null)
			{
				wo.getPerceivedProperties().add(bestProperty.getName());
			}
			
		}
	}
	
	/*
	 * Choose the property as the one with the smallest mean distance of points to the given WorldObject.
	 * @param wo The world object to generate properties for
	 * @param minUtterances The minimum number of utterances before considering a property 
	 */
	public static void generatePerceivedObjectClassWithMeanDistance(WorldManager wm, WorldObject wo, int minUtterances)
	{
		Map<FeatureSpace, Double> averageScaledSumOfSquares = wm.getScaledSumOfSquares();
		Map<FeatureSpace, DoubleMatrix> averageFeaturePoints = wo.getAveragePerceptValues();
		double minRatio = Double.MAX_VALUE;
		ObjectClass bestObjectClass = null;
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			if (oc.getUtterances().size() < minUtterances)
				continue;

			for (FeatureSpace featureSpace : oc.getRepresentativeFeatures())
			{
				
				double ratio = (double) ((oc.getAverageDistanceForFeature(featureSpace, 
									averageFeaturePoints.get(featureSpace)) /
									averageScaledSumOfSquares.get(featureSpace)));
		
				if (ratio < minRatio)
				{
					minRatio = ratio;
					bestObjectClass = oc;
				}
			}
		}
		
		if (bestObjectClass != null)
			wo.setObjectClass(bestObjectClass);	
	}
	
	public static void generateWordsWithKNearestDistance(WorldManager wm, WorldObject wo, int minUtterances, int k, int descriptionLength)
	{
		TreeMap<Double, Word> distanceWordMapping = new TreeMap<Double, Word>();
		Map<Word, Double> votingResults = new HashMap<Word, Double>();
		
		for (Word w : wm.getWords().values())
		{
//			if (w.getUtterances().size() < minUtterances ||
//					!w.isDescriptiveWord())
//				continue;
			if (w.getUtterances().size() < minUtterances)
				continue;
			
			//System.out.println("Classifying with word: " + w.getName());
			//List<Double> pDistances = oc.getKNearestMahalanobis(wo.getAveragePerceptValues(), k);
			List<Double> pDistances = w.getKNearestMahalanobisWeighted(wo.getAveragePerceptValues(), k, 
					wm.getNumberOfWordPerceptClusters());
			double totalDistance = 0.0d;
			for (double distance : pDistances)
			{
				// Distance is closest feature
				distanceWordMapping.put(distance,w);
				
				//totalDistance += distance;
			}
			// Distance is sum of weighted features
			//distancePropertyMapping.put(totalDistance,oc);
		}
		
		for (double distance : distanceWordMapping.navigableKeySet())
		{
			if (!votingResults.containsKey(distanceWordMapping.get(distance)))
				votingResults.put(distanceWordMapping.get(distance),0.);
			
			double weight = 1.0 / distance;
			
			votingResults.put(distanceWordMapping.get(distance), 
					votingResults.get(distanceWordMapping.get(distance)) + weight);
				
		}
		
		TreeMap<Double, Word> sortedVotingResults = new TreeMap<Double, Word>();
		
		for (Word w: votingResults.keySet())
		{
			sortedVotingResults.put(votingResults.get(w),w);
		}
		
		int chosenWords = 0;
		
		if (sortedVotingResults.size() != 0)
		{
			for (Double score : sortedVotingResults.descendingKeySet())
			{
				if (chosenWords >= descriptionLength)
					break;
				
				wo.getPerceivedWords().add(sortedVotingResults.get(score).getName());
				chosenWords++;
			}
			
		}
		else
			System.out.println("No results for word classification.");
	}
	
	public static void generatePerceivedObjectClassWithObjectModels(WorldManager wm, WorldObject wo)
	{
		double minDistance = Double.MAX_VALUE;
		ObjectClass bestObjectClass = null;
		System.out.println("Generating classes with Object Models");
		TreeMap<Integer, Double> minDistancePerVariableNumber = new TreeMap<Integer, Double>();
		TreeMap<Integer, ObjectClass> bestObjectClassPerVariableNumber = new TreeMap<Integer, ObjectClass>();
		
		// Object models are associated with object classes
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			//System.out.println("Object class: " + oc.getName());
			// If there's no object model, skip it for now
			List<ObjectModel> objectModels = oc.getObjectModels();
			if (objectModels.size() == 0)
				continue;
			
			for (ObjectModel om : objectModels)
			{
				int numberOfVariables = om.getVariables().size();
				if (!minDistancePerVariableNumber.containsKey(numberOfVariables))
					minDistancePerVariableNumber.put(numberOfVariables, Double.MAX_VALUE);
				double distance = om.getDistance(wo);
				System.out.println("Distance to " + oc.getName() + ": " + distance);
				
				if (distance < minDistancePerVariableNumber.get(numberOfVariables))
				{
					minDistancePerVariableNumber.put(numberOfVariables, distance);
					bestObjectClassPerVariableNumber.put(numberOfVariables,oc);
				}
			}
		}
		if (bestObjectClassPerVariableNumber.size() > 0)
		{
			// Look for an object model matching the median number of subblobs over all object occurrences
			// If no models match, try matching to fewer subblobs
			int medianNumberOfSubblobs = wo.getMedianSubblobs();
			if (USE_MEDIAN_SUBBLOB_CLASSES && bestObjectClassPerVariableNumber.containsKey(medianNumberOfSubblobs))
			{
				System.out.println("Best object class: " + bestObjectClassPerVariableNumber.get(medianNumberOfSubblobs).getName());
				wo.setObjectClass(bestObjectClassPerVariableNumber.get(medianNumberOfSubblobs));
			}
			else
			{
				System.out.println("Best object class: " + bestObjectClassPerVariableNumber.lastEntry().getValue().getName());
				wo.setObjectClass(bestObjectClassPerVariableNumber.lastEntry().getValue());
			}
		}
		else
			System.out.println("No matching class");
			
	}
	
	
}
