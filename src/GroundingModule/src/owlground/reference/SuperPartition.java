package owlground.reference;

import java.util.HashSet;
import java.util.Set;

import owlground.objects.WorldObject;

public class SuperPartition extends Partition {

	private Set<Partition> subPartitions;
	
	public SuperPartition(HashSet<WorldObject> positiveExamples,
			HashSet<WorldObject> negativeExamples, boolean classNamePartition) {
		super(positiveExamples, negativeExamples, classNamePartition);
		subPartitions = new HashSet<Partition>();
	}
	
	public SuperPartition(boolean classNamePartition)
	{
		this(new HashSet<WorldObject>(),new HashSet<WorldObject>(), classNamePartition);
	}

	public SuperPartition()
	{
		this(new HashSet<WorldObject>(),new HashSet<WorldObject>(), false);
	}

	public void mergePartition(Partition p)
	{
		positiveExamples.addAll(p.positiveExamples);
		negativeExamples.addAll(p.negativeExamples);
		unlabeledDistanceCalculated = false;
		subPartitions.add(p);
	}

	public void updateSubPartitionMetaProbabilities()
	{
		double metaProbability = getUnlabeledProbability();
		for (Partition p : subPartitions)
		{
			p.addMetaProbability(metaProbability);
		}
	}

	public Set<Partition> getSubPartitions() {
		return subPartitions;
	}
}
