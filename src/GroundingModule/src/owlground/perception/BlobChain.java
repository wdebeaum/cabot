package owlground.perception;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jblas.DoubleMatrix;

import owlground.objects.WorldObject;
import owlground.spaces.FeatureSpace;

public class BlobChain {
	private List<Blob> blobs;
	private WorldObject assignedWorldObject;
	private int id;
	
	public BlobChain()
	{
		assignedWorldObject = null;
		blobs = new ArrayList<Blob>();
		id = -1;
	}
	
	public BlobChain(List<Blob> blobList)
	{
		assignedWorldObject = null;
		blobs = blobList;
		id = -1;
	}
	
	public void add(Blob b)
	{
		blobs.add(b);
	}
	
	public double getAverageDistanceToHands()
	{
		double totalDistance = 0;
		for (Blob b : blobs)
		{
			totalDistance += b.getMinHandDistance();
		}
		return totalDistance / blobs.size();
	}
	
	public double getAverageDistanceToRightHand()
	{
		double totalDistance = 0;
		for (Blob b : blobs)
		{
			totalDistance += b.getRightHandDistance();
		}
		return totalDistance / blobs.size();
	}
	
	public double getAverageDistanceToLeftHand()
	{
		double totalDistance = 0;
		for (Blob b : blobs)
		{
			totalDistance += b.getRightHandDistance();
		}
		return totalDistance / blobs.size();
	}
	
	public int getAverageX()
	{
		double totalX = 0;
		for (Blob b : blobs)
		{
			totalX += b.getX();
		}
		return (int)(totalX / blobs.size());		
	}
	
	public int getAverageY()
	{
		double totalY = 0;
		for (Blob b : blobs)
		{
			totalY += b.getY();
		}
		return (int)(totalY / blobs.size());		
	}
	
	public int length()
	{
		return blobs.size();
	}
	
	public Map<FeatureSpace,DoubleMatrix> getAveragePerceptValues()
	{
		List<PerceptCluster> perceptClusters = new ArrayList<PerceptCluster>();
		
		for (Blob b : blobs)
		{
			perceptClusters.add(b.getPerceptCluster());
		}
		
		return PerceptCluster.getAveragePerceptCluster(perceptClusters);
	}
	
	public List<PerceptCluster> getPerceptClusters()
	{
		List<PerceptCluster> result = new ArrayList<PerceptCluster>();
		
		for (Blob b : blobs)
		{
			result.add(b.getPerceptCluster());
		}
		
		return result;
	}
	
	public HashMap<Long,PerceptCluster> getTimestampedPerceptClusters()
	{
		HashMap<Long, PerceptCluster> result = new HashMap<Long, PerceptCluster>();
		
		for (Blob b : blobs)
		{
			result.put(b.getPerceptCluster().getTimestamp(),b.getPerceptCluster());
		}
		
		return result;
	}

	public WorldObject getAssignedWorldObject() {
		return assignedWorldObject;
	}

	public void setAssignedWorldObject(WorldObject assignedWorldObject) {
		this.assignedWorldObject = assignedWorldObject;
	}

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}
	
	
}
