package owlground.perception;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.language.Demonstration;
import owlground.language.Utterance;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.spaces.FeatureSpace;

public class BlobGroupChain {
	private Utterance utterance;
	private List<BlobGroup> blobGroups;
	private Demonstration demonstration;
	private boolean idsResolved; 	//Whether we have consistent ID's through the chain
	private int maxId = -1;
	private Map<Integer,BlobChain> blobChains;
	
	// The maximum distance in pixels an object can go and be considered the same blob
	private static final int MAX_DISTANCE = 100;
	private static final int MIN_FRAMES = 6;
	
	
	public BlobGroupChain(Utterance utterance, Demonstration demonstration)
	{
		this.utterance = utterance;
		this.demonstration = demonstration;
		idsResolved = false;
		blobGroups = new ArrayList<BlobGroup>();
		blobChains = new HashMap<Integer,BlobChain>();
		
		if (utterance != null)
		{
			for (BlobGroup bg : utterance.getBlobGroups().values())
			{
				blobGroups.add(bg);
			}
		}
	}
	/*
	 * Enforces consistent blob ids by choosing ID's that minimize distance 
	 * between blobs from one frame to the next.
	 */
	public void resolveBlobIds()
	{
		BlobGroup lastBlobGroup = null;
		int currentIndex = 0;
		System.out.println("# of blobgroups: " + blobGroups.size());
		for (BlobGroup bg : blobGroups)
		{
			// If this is the first blob group, assign initial IDs to all of the blobs
			if (lastBlobGroup == null)
			{	
				for (Blob b : bg.getBlobs())
				{
					b.setId(currentIndex);
					addBlob(b);
					currentIndex++;
					continue;
				}
			}
			// If it isn't the first one, assign ID's that are consistent with the previous one
			else
			{
				
				HashSet<Integer> chosenIds = new HashSet<Integer>();
				for (Blob lastBlob : lastBlobGroup.getBlobs())
				{
					for (Entry<Double,Blob> e : bg.getDistancesToBlob(lastBlob).entrySet())
					{
						Blob newBlob = e.getValue();
						double distance = e.getKey();
						
						// Don't assign any id if all are too far away
						if (distance > MAX_DISTANCE)
							break;
						// This blob ID has already been assigned, choose the next one
						if (chosenIds.contains(newBlob.getId()))
							continue;
						
						newBlob.setId(lastBlob.getId());
						addBlob(newBlob);
					}
				}
				
				// Generate new ID's for any leftover blobs
				for (Blob newBlob : bg.getBlobs())
				{
					if (newBlob.getId() == -1)
					{
						newBlob.setId(currentIndex);
						addBlob(newBlob);
						currentIndex++;
					}
				}
			}
			lastBlobGroup = bg;
			
		}
		
		idsResolved = true;
		maxId = currentIndex;
	}
	
	private void addBlob(Blob b)
	{
		if (b.getId() < 0)
			throw new IllegalArgumentException("Blob ID must be set");
		
		if (!blobChains.containsKey(b.getId()))
			blobChains.put(b.getId(), new BlobChain());
			
		blobChains.get(b.getId()).add(b);
		idsResolved = false;
	}
	
	public TreeMap<Double, BlobChain> getBlobChainHandDistances(int minFrames)
	{
		if (!idsResolved)
			resolveBlobIds();
		
		TreeMap<Double, BlobChain> results = new TreeMap<Double,BlobChain>();
		
		for (BlobChain bc : blobChains.values())
		{
			if (bc.length() >= minFrames)
				results.put(bc.getAverageDistanceToHands(), bc);
		}
		
		return results;
	}
	
	public void pruneShortBlobChains(int minSize)
	{
		Iterator<Entry<Integer, BlobChain>> blobChainIterator = blobChains.entrySet().iterator();
		while (blobChainIterator.hasNext())
		{
			Entry<Integer, BlobChain> blobChainEntry = blobChainIterator.next();
			if (blobChainEntry.getValue().length() < minSize)
			{
				blobChainIterator.remove();
			}
		}
	}
	
	/*
	 * Find the perceptual similarity distances for the BlobChains according to
	 * how closely they match the inferred properties of the given WorldObject.
	 * 
	 * It basically is going through all of the known objects it has seen to 
	 * figure out which BlobChain is closest to the properties of the given
	 * WorldObject. It's a greedy way of using existing knowledge when there
	 * are multiple objects.
	 */
	public TreeMap<Double, BlobChain> getBlobChainDistances(WorldObject wo, Collection<WorldObject> demonstratedObjects)
	{
		TreeMap<Double, BlobChain> results = new TreeMap<Double, BlobChain>();
		// Average over the frames the object is seen in
		wo.updatePerceptClustersToAttended();
		Map<FeatureSpace, DoubleMatrix> averagePerceptValues = wo.getAveragePerceptValues();
		Map<String, FeatureSpace> representativeFeatureSpaceMap = Property.getRepresentativeFeatureSpaceMap(wo.getSession());
		
		if (wo.getProperties().isEmpty())
			return null;
		
		for (BlobChain bc : getBlobChains().values())
		{
			double totalBlobChainDistance = 0;
			for (String property : wo.getProperties())
			{
				double minDistance = Double.MAX_VALUE;
				FeatureSpace featureSpace = representativeFeatureSpaceMap.get(property);
				
				for (WorldObject otherWo : demonstratedObjects)
				{
					otherWo.updatePerceptClustersToAttended();
					for (String otherProperty : otherWo.getProperties())
					{
						if (property.equals(otherProperty))
						{
							for (PerceptCluster pc : otherWo.getPerceptClusters().values())
							{
								HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
								Percept otherPercept = otherPercepts.get(featureSpace);
								if (otherPercept == null)
									continue;
								DoubleMatrix otherPoint = otherPercept.getValue();
								DoubleMatrix thisPoint = averagePerceptValues.get(featureSpace);
								double distance = thisPoint.distance2(otherPoint);
								
								if (distance < minDistance)
								{
									minDistance = distance;
								}
							}
						}
					}
				}
				totalBlobChainDistance += minDistance;
								
			}
			
			for (String negatedProperty : wo.getNegatedProperties())
			{
				double minDistance = Double.MAX_VALUE;
				FeatureSpace featureSpace = representativeFeatureSpaceMap.get(negatedProperty);
				
				for (WorldObject otherWo : demonstratedObjects)
				{
					for (String otherProperty : otherWo.getProperties())
					{
						if (negatedProperty.equals(otherProperty))
						{
							for (PerceptCluster pc : otherWo.getPerceptClusters().values())
							{
								HashMap<FeatureSpace, Percept> otherPercepts = pc.getPercepts();
								Percept otherPercept = otherPercepts.get(featureSpace);
								DoubleMatrix otherPoint = otherPercept.getValue();
								DoubleMatrix thisPoint = averagePerceptValues.get(featureSpace);
								double distance = thisPoint.distance2(otherPoint);
								
								if (distance < minDistance)
								{
									minDistance = distance;
								}
							}
						}
					}
				}
				totalBlobChainDistance -= minDistance;
			}
			results.put(totalBlobChainDistance, bc);
		}
		
		return results;

	}
	
	public Map<Integer, BlobChain> getBlobChains()
	{
		if (!idsResolved)
			resolveBlobIds();
		
		return blobChains;
	}
	
	public int size()
	{
		if (!idsResolved)
			resolveBlobIds();
		
		return blobChains.size();
	}
	
}
