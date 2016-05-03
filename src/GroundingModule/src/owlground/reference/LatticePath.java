package owlground.reference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import owlground.objects.WorldObject;
import owlground.utilities.Utilities;

public strictfp class LatticePath {

	private List<LatticeEdge> edges;
	private Collection<WorldObject> result;
	private Partition partition;
	public static double OVERSPECIFICATION_EXPECTATION ; // Change in the setUsePragmatics method
	
	public LatticePath()
	{
		edges = new LinkedList<LatticeEdge>();
		result = new HashSet<WorldObject>();
		partition = null;
	}
	
	public LatticePath(LatticePath toCopy, LatticeEdge toAdd)
	{
		edges = new LinkedList<LatticeEdge>(toCopy.edges);
		result = new HashSet<WorldObject>(toCopy.result);
		partition = null;
		addEdge(toAdd);
	}
	
	public LatticePath(Partition partition)
	{
		edges = new LinkedList<LatticeEdge>();
		result = partition.getPositiveExamples();
		this.partition = partition;
	}
	
	public static void setUsePragmatics(boolean usePragmatics)
	{
		//.6 seems best without pragmatics
		if (usePragmatics)
			OVERSPECIFICATION_EXPECTATION = 0;
		else
			OVERSPECIFICATION_EXPECTATION = .6;
	}
	
	public void addEdge(LatticeEdge edge)
	{
		edges.add(edge);
		if (result.size() == 0)
		{
			result = new HashSet<WorldObject>(edge.getIntersection());
			
		}
		else
		{
			result.retainAll(edge.getIntersection());
		}
	}
	
	public Collection<WorldObject> getResult()
	{
		return result;
	}

	
	public double getProbability()
	{	
		if (edges.size() == 0)
			return partition.getNormalizedProbability();
		
		double probability = Math.log(edges.get(0).getFrom().getNormalizedProbability());
		//double overspecificationProbability = Math.pow(OVERSPECIFICATION_EXPECTATION, 1/(edges.size() + 1));
		double overspecificationProbability = OVERSPECIFICATION_EXPECTATION;
		// This is taken care of in ReferenceLattice now
		//if (edges.get(0).getFrom().getNegativeExamples().size() == 0)
		//	probability += Math.log(overspecificationProbability);
		for (LatticeEdge edge : edges)
		{
			
			probability += Math.log(
							edge.getTo().getNormalizedProbability());
			probability += edge.getLogProbability();
			
			// Overspecified?
			
			//if (edge.getFrom().getPositiveExamples().equals(edge.getTo().getPositiveExamples()))
			
//			if (edge.getIntersection().equals(edge.getFrom().getPositiveExamples()))
//			{
//				
//				probability += Math.log(overspecificationProbability);
//			}
		}
		//System.out.println("Log likelihood: " + probability);
		//System.out.println("Probability: " + Math.exp(probability));
		return Math.exp(probability);
	}

	public List<LatticeEdge> getEdges() {
		return edges;
	}
	
	public List<Partition> getPartitionList()
	{
		List<Partition> result = new ArrayList<Partition>();
		if (partition != null && edges.isEmpty())
			result.add(partition);
		if (edges.isEmpty())
			return result;
		
		result.add(edges.get(0).getFrom());
		for (LatticeEdge edge: edges)
		{
			result.add(edge.getTo());
		}
		
		return result;
	}
	
	public boolean isOverspecified()
	{
		for (int i = 0; i < edges.size(); i++)
		{
			if (edges.get(i).isOverSpecified())
				return true;
		}
		return false;
	}
	
	public String toString()
	{
		StringBuilder result = new StringBuilder();
		if (partition != null)
			result.append(partition.getTotalProbability());
		else if (edges.size() > 0)
			result.append(edges.get(0).getFrom().getTotalProbability());
		else
			return "";
		for (LatticeEdge e : edges)
		{
			result.append(" ");
			result.append(e.getProbability());
			result.append(" ");
			result.append(e.getTo().getTotalProbability());
		}
		return result.toString();
	}
}
