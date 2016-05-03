package owlground.reference;

import java.util.HashSet;
import java.util.Set;

import owlground.objects.WorldObject;

public strictfp class LatticeEdge {
	private Partition from;
	private Partition to;
	private double logProbability;
	private boolean active;
	private Set<WorldObject> intersection; // Set of worldobjects reached by applying to and from partitions
	private boolean overSpecified;
	
	public LatticeEdge(Partition from, Partition to)
	{
		this.from = from;
		this.to = to;
		calculateIntersection();
		overSpecified = (from.getPositiveExamples().equals(intersection));
	}
	
	
	
	private void calculateIntersection()
	{
		intersection = new HashSet<WorldObject>(from.getPositiveExamples());
		intersection.retainAll(to.getPositiveExamples());
	}
	
	public Set<WorldObject> getIntersection()
	{
		return intersection;
	}
	
	
	
	public double overspecificationProbability()
	{
		return 1;
	}
	
	public boolean emptyIntersection()
	{
		return (intersection.size() == 0);
	}

	public Partition getFrom() {
		return from;
	}

	public Partition getTo() {
		return to;
	}



	public boolean isOverSpecified() {

		return overSpecified;
	}



	public double getLogProbability() {
		return logProbability;
	}

	public void setProbability(double probability)
	{
		this.logProbability = Math.log(probability);
	}
	
	public double getProbability()
	{
		return Math.exp(logProbability);
	}

	public void setLogProbability(double logProbability) {
		this.logProbability = logProbability;
	}

}
