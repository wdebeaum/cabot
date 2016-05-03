package owlground.perception;

import org.jblas.DoubleMatrix;

import owlground.spaces.FeatureSpace;
import owlground.spaces.SpaceManager;

/*
 * A feature vector belonging to a cluster. Corresponds to a single feature, 
 * such as RGB.
 */
public class Percept {
	// The feature space it belongs to
	private FeatureSpace featureSpace;
	private DoubleMatrix value;
	
	// The PerceptCluster it belongs to
	private PerceptCluster cluster;

	public Percept(String spaceName, DoubleMatrix value, PerceptCluster cluster)
	{
		this.value = value;
		this.cluster = cluster;
		this.featureSpace = cluster.getSession().getSpaceManager().getFeatureSpaceByName(spaceName);
	}
	
	public Percept(FeatureSpace featureSpace, DoubleMatrix value, PerceptCluster cluster)
	{
		
		this.value = value;
		this.cluster = cluster;
		this.featureSpace = featureSpace;
	}
	
	public DoubleMatrix getValue() {
		return value;
	}
	
	public String getSpaceName() {
		return featureSpace.getName();
	}
	
	public FeatureSpace getFeatureSpace()
	{
		return featureSpace;
	}
	
	public String toString()
	{
		return getSpaceName() + " " + value.toString();
	}
	
	public double distance(Percept p)
	{
		return value.distance2(p.getValue());
	}
	
	public double squaredDistance(Percept p)
	{
		return value.squaredDistance(p.getValue());
	}
	
	public SpaceManager getSpaceManager()
	{
		return cluster.getSession().getSpaceManager();
	}
}
