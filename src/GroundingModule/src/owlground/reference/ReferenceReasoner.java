package owlground.reference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import owlground.language.Demonstration;
import owlground.objects.AbstractDescriptor;

/**
 * Takes in multiple demonstrations and figures out the correct partitions to
 * resolve the referents.
 * @author iperera
 *
 */
public class ReferenceReasoner {

	private static boolean ENABLED = false;
	private static int MAX_DEMONSTRATIONS = 3;
	private List<Demonstration> demonstrations;
	private HashMap<AbstractDescriptor, HashMap<ReferenceLattice,Set<Partition>>> descriptorPartitions;

	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("reference.usereasoner"))
			ENABLED = Boolean.parseBoolean(properties.get("reference.usereasoner"));
		if (properties.containsKey("reference.reasoner.maxdemonstrations"))
			MAX_DEMONSTRATIONS = Integer.parseInt(properties.get("reference.reasoner.maxdemonstrations"));
		
	}
	
	/**
	 * Takes in a Collection of unresolved Demonstrations to process them simultaneously and 
	 * resolve them.
	 * @param demonstrations
	 */
	public ReferenceReasoner(Collection<Demonstration> demonstrations) {
		this.demonstrations = new ArrayList<Demonstration>();
		this.descriptorPartitions = new HashMap<AbstractDescriptor, 
												HashMap<ReferenceLattice,Set<Partition>>>();
		
		for (Demonstration d : demonstrations)
		{
			ReferenceLattice lattice = d.getLattice();
			List<AbstractDescriptor> descriptors = lattice.getDescriptors();
			List<LatticePath> bestPaths = lattice.getBestPaths();
			
			// Set up the map from descriptors to lattices containing that descriptor
			for (AbstractDescriptor descriptor : descriptors)
			{
				if (!descriptorPartitions.containsKey(descriptor))
					descriptorPartitions.put(descriptor, new HashMap<ReferenceLattice, Set<Partition>>());
				descriptorPartitions.get(descriptor).put(lattice, new HashSet<Partition>());
			}
			
			// Go through the best paths and add the partitions to the descriptor as possible
			// partitions
			for (LatticePath p : bestPaths)
			{
				List<Partition> partitions = p.getPartitionList();
				//System.out.println(partitions.size() + " partitions");
				for (int i = 0; i < partitions.size(); i++)
				{
					
					AbstractDescriptor descriptor = descriptors.get(i);
					//System.out.println("Descriptor: " + descriptor.getName());
					descriptorPartitions.get(descriptor).get(lattice).add(partitions.get(i));
				}
			}
		}
	}
	
	public void updateMetaProbabilities()
	{
		System.out.println("Updating meta probabilities");
		for (AbstractDescriptor descriptor : descriptorPartitions.keySet())
		{
			//System.out.println("For descriptor: " + descriptor.getName());
			List<SuperPartition> superPartitions = new ArrayList<SuperPartition>();
			HashMap<Partition, Set<Partition>> edges = new HashMap<Partition,Set<Partition>>();
			List<ReferenceLattice> latticeList = 
					new ArrayList<ReferenceLattice>(
							descriptorPartitions.get(descriptor).keySet());
			
			// A non-recursive method for generating all combinations of partitions 
			// from different demonstrations
			List<Set<Partition>> combinationLattice = new ArrayList<Set<Partition>>(); 
			//System.out.println("Lattice size for : " + descriptor.getName());
			int latticeNumber = 1;
			for (ReferenceLattice lattice : latticeList)
			{
				if (latticeNumber > MAX_DEMONSTRATIONS)
					break;
				latticeNumber++;
				//System.out.println(descriptorPartitions.get(descriptor).get(lattice).size() + " partitions in lattice");
				//System.out.println("Probabilities");
				//for (Partition p : descriptorPartitions.get(descriptor).get(lattice))
				//	System.out.println(p.getTotalProbability());
				combinationLattice.add(descriptorPartitions.get(descriptor).get(lattice));
			}
			
			
			
			// Create the lattice
			for (int i = 0; i < combinationLattice.size() - 1; i++)
			{
				for (Partition p : combinationLattice.get(i))
				{
					edges.put(p, new HashSet<Partition>());
					for (Partition next : combinationLattice.get(i+1))
					{
						edges.get(p).add(next);
					}
				}
			}
			
			// Generate the SuperPartitions to cover all possible
			// combinations of Partitions from different demonstrations
			// by tracing paths through the lattice
			for (Partition p : combinationLattice.get(0))
			{
				LinkedList<Partition> initialPath = new LinkedList<Partition>();
				initialPath.add(p);
				generatePathsRecursive(descriptor,edges,superPartitions,initialPath);
			}
			
			// Update the SuperPartition scores
			for (SuperPartition sp : superPartitions)
			{
				sp.updateSubPartitionMetaProbabilities();
			}
		
		}
		
	}
	
	private void generatePathsRecursive(AbstractDescriptor descriptor,
										Map<Partition,Set<Partition>> edges,
										List<SuperPartition> superPartitions,
										LinkedList<Partition> pastPath)
	{
		if (!edges.containsKey(pastPath.getLast()))
		{
			SuperPartition sp = new SuperPartition();
			for (Partition p : pastPath)
			{
				sp.mergePartition(p);
			}
			superPartitions.add(sp);
			return;
		}
		Set<Partition> nextPartitions = edges.get(pastPath.getLast());

		for (Partition next : nextPartitions)
		{
			LinkedList<Partition> newPath = new LinkedList<Partition>(pastPath);
			newPath.addLast(next);
			generatePathsRecursive(descriptor,edges,superPartitions, newPath);
		}
	}
	
	public static boolean isEnabled()
	{
		return ENABLED;
	}
	


}
