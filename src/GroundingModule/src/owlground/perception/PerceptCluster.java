package owlground.perception;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.language.Utterance;
import owlground.spaces.FeatureSpace;
import owlground.spaces.SpaceManager;
/*
 * A collection of all Percepts calculated at time timestamp. Also stores
 * the utterance it is part of. A PerceptCluster corresponds to all of the
 * feature data for one object for one frame.
 */
public class PerceptCluster {
	Utterance utterance;
	HashMap<FeatureSpace, Percept> percepts;
	long timestamp;
	
	/*
	 * Average the input PerceptClusters to create one PerceptCluster 
	 * representing the object
	 */
	public static Map<FeatureSpace, DoubleMatrix> getAveragePerceptCluster(Collection<PerceptCluster> perceptClusters)
	{
		HashMap<FeatureSpace, DoubleMatrix> averageValues = new HashMap<FeatureSpace, DoubleMatrix>();
		for (PerceptCluster pc : perceptClusters)
		{
			// Initialize averageValues
			if (averageValues.size() == 0)
			{
				for (FeatureSpace featureSpace : pc.getPercepts().keySet())
				{
					averageValues.put(featureSpace, DoubleMatrix.zeros(pc.getPercepts().get(featureSpace).getValue().length));
				}
			}
			
			for (FeatureSpace featureSpace : pc.getPercepts().keySet())
			{
				averageValues.get(featureSpace).addi(pc.getPercepts().get(featureSpace).getValue());
			}
			
		}
		
		for (FeatureSpace featureSpace : averageValues.keySet())
		{
			averageValues.get(featureSpace).divi(perceptClusters.size());
		}
		
		return averageValues;
		
	}
	
	public PerceptCluster(Utterance utterance, long timestamp)
	{
		this.utterance = utterance;
		this.timestamp = timestamp;
		percepts = new HashMap<FeatureSpace, Percept>();
	}
	
	public long getTimestamp() {
		return timestamp;
	}

	public Utterance getUtterance() {
		return utterance;
	}
	public HashMap<FeatureSpace, Percept> getPercepts() {
		return percepts;
	}
	
	public HashMap<String, Percept> getPerceptsByName()
	{
		HashMap<String, Percept> result = new HashMap<String, Percept>();
		for (Entry<FeatureSpace, Percept> e : getPercepts().entrySet())
		{
			result.put(e.getKey().getName(), e.getValue());
		}
		
		return result;
	}
	
	public void addPercepts(ArrayList<Percept> percepts)
	{
		for (Percept p : percepts)
		{
			addPercept(p);
		}
	}
	
	public void addPercept(Percept p)
	{
		SpaceManager sm = utterance.getSession().getSpaceManager();
		if (sm.getFeatureSpaceByName(p.getFeatureSpace().getName()) == null)
			sm.addFeatureSpace(p.getFeatureSpace());
		percepts.put(p.getFeatureSpace(), p);
	}
	
	public String toString()
	{
		String toReturn = timestamp + "\n" + utterance.toString() + "\n";
		for (Percept p : percepts.values())
		{
			toReturn += p.toString() + "\n";
		}
		
		return toReturn;
	}
	
	public DoubleMatrix combinedMatrix()
	{
		
		DoubleMatrix combined = null;
		TreeMap<FeatureSpace, Percept> sortedPercepts = new TreeMap<FeatureSpace,Percept>(percepts);
		for (Percept p : sortedPercepts.values())
		{
			if (combined == null)
				combined = p.getValue();
			else
				combined = DoubleMatrix.concatVertically(combined, p.getValue());
		}
		
		return combined;
	}
	
	public int getNumberOfDimensions()
	{
		int dimensions = 0;
		
		for (Percept p : percepts.values())
		{
			dimensions += p.getValue().length;
		}
		
		return dimensions;
	}
	
	public double distance(PerceptCluster pc)
	{
		double totalDistance = 0f;
		for (FeatureSpace s : percepts.keySet())
		{
			if (pc.getPercepts().containsKey(s))
				totalDistance += percepts.get(s).distance(pc.getPercepts().get(s));
		}
		return totalDistance;
	}
	
	public Map<FeatureSpace, DoubleMatrix> toPerceptMap()
	{
		Map<FeatureSpace, DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		for (Percept p : percepts.values())
			result.put(p.getFeatureSpace(), p.getValue());
		
		return result;
	}
	
	public double featureDistance(FeatureSpace featureSpace, DoubleMatrix value)
	{		
		if (value == null || value.length == 0)
			return Double.MAX_VALUE;
		return percepts.get(featureSpace).getValue().distance2(value);

	}
	
	public FeatureManager getSession()
	{
		return utterance.getSession();
	}
	
	
}
