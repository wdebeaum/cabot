package owlground.reference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import owlground.objects.AbstractDescriptor;
import owlground.objects.WorldObject;
import owlground.objects.WorldObjectGroup;

public class PartitionGenerator {
	private double averageVisualDistance;
	
	public PartitionGenerator()
	{
		
	}
	
	// From Piva at http://stackoverflow.com/a/4640130
	public List<Partition> generateExhaustivePartitions(WorldObjectGroup focus, 
			Collection<WorldObject> worldObjects, boolean classNamePartition)
	{
		List<Partition> partitions = new ArrayList<Partition>();
		WorldObject[] worldObjectArray = worldObjects.toArray(new WorldObject[]{});
		int numberOfObjects = worldObjects.size();
		if (numberOfObjects > 10)
			throw new RuntimeException("Too many objects in scene");
		int numberOfPartitions = 1 << numberOfObjects; 
		//System.out.println("Number of partitions to create: " + numberOfPartitions);
		// Count and use bits to determine partition
		for (int i = 1; i < numberOfPartitions; i++)
		{
			List<WorldObject> positiveExamples = new ArrayList<WorldObject>();
			List<WorldObject> negativeExamples = new ArrayList<WorldObject>();
			// Don't create empty partitions
			for (int j = 0; j < numberOfObjects; j++)
			{
				// If the bit value is one, add to the positive set
				if (((i >> j) & 1) == 1)
					positiveExamples.add(worldObjectArray[j]);
				else
					negativeExamples.add(worldObjectArray[j]);
			}
			

			Partition partition = new Partition(positiveExamples, negativeExamples,classNamePartition);
			
			if (negativeExamples.isEmpty())
				averageVisualDistance = partition.getAverageVisualDistanceBetweenObjects();
			
			partitions.add(partition);
		}
		
		//System.out.println(partitions.size() + " partitions generated");
		
		return partitions;
	}
	
	/**
	 * Use labeled probability data to sort objects from least likely to most likely,
	 * then generate all possible partitions in the list maintaining ordering. The 
	 * intuition is that if some object is in the positive example set, then all other
	 * objects more likely to be assigned the given label will also be in this set.
	 * @param worldObjects
	 * @param descriptor
	 * @param worldObjectProbabilities
	 * @param classNamePartition
	 * @return
	 */
	public List<Partition> generateOrderedPartitions(Collection<WorldObject> worldObjects,
													AbstractDescriptor descriptor, 
													Map<WorldObject,Double> worldObjectProbabilities,
													boolean classNamePartition)
	{
		List<Partition> partitions = new ArrayList<Partition>();
		TreeMap<Double, WorldObject> sortedObjectProbabilities = new TreeMap<Double, WorldObject>();
		
		for (Entry<WorldObject, Double> entry : worldObjectProbabilities.entrySet())
			sortedObjectProbabilities.put(entry.getValue(), entry.getKey());
		
		List<WorldObject> sortedObjects = new ArrayList<WorldObject>();
		
		for (WorldObject wo : sortedObjectProbabilities.values())
			sortedObjects.add(wo);
		
		// Add a partition for every possible combination of objects such that
		// all objects above an object's probability are in the positive set,
		// and in the negative set otherwise
		List<WorldObject> negativeExamples = new LinkedList<WorldObject>();
		for (int i = 0; i < sortedObjects.size() + 1; i++)
		{
			if (i > 0)
				negativeExamples.add(sortedObjects.get(i-1));
			List<WorldObject> positiveExamples = new LinkedList<WorldObject>();
			
			for (int j = i; j < sortedObjects.size(); j++)
			{
				positiveExamples.add(sortedObjects.get(j));
			}
			
			Partition p = new Partition(positiveExamples,
					new ArrayList<WorldObject>(negativeExamples),classNamePartition);
			partitions.add(p);
			
		}
		
		return partitions;
	}

	public double getAverageVisualDistance() {
		return averageVisualDistance;
	}
	
	
}
