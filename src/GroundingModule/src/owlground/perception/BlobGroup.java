package owlground.perception;

import java.util.LinkedList;
import java.util.List;
import java.util.TreeMap;

public class BlobGroup {
	List<Blob> blobs;
	private static double MAX_HAND_DISTANCE = 100;

	public BlobGroup()
	{
		blobs = new LinkedList<Blob>();
	}
	
	public BlobGroup(List<Blob> blobList)
	{
		blobs = new LinkedList<Blob>(blobList);
	}
	
	public void addBlob(Blob b)
	{
		blobs.add(b);
	}
	
	public int size()
	{
		return blobs.size();
	}
	
	public long getTimestamp()
	{
		if (blobs.size() == 0)
			return -1;
		return blobs.get(0).getPerceptCluster().getTimestamp();
	}
	
	public String generateRelativeLocation(Blob b)
	{
		// The objects [left of/right of/above/below] the input blob
		Blob left = null;
		Blob right = null;
		Blob above = null;
		Blob below = null;
		
		for (Blob other : blobs)
		{
			if (other == b)
				continue;
			
			// Coordinates are for the top left corner of each object
			if (other.getX() > b.getX() + b.getWidth())
				right = other;
			else if (other.getX() + other.getWidth() < b.getX())
				left = other;
			
			if (other.getY() + other.getHeight() < b.getY())
				above = other;
			else if (other.getY() > b.getY() + b.getHeight())
				below = other;
			
		}
		
		return "";
	}
	
	/**
	 * Get the Blob closest to the subject's hand
	 * @return the closest blob
	 */
	public Blob getClosestBlobToHand()
	{
		Blob best = null;
		for (Blob b : blobs)
		{
			if (best == null)
			{
				best = b;
			}
			else
			{
				boolean better = false;

				better = (b.getMinHandDistance() < best.getMinHandDistance() &&
									b.getMinHandDistance() < MAX_HAND_DISTANCE);

				if (better)
					best = b;
			}
		}

		if (best.getMinHandDistance() < MAX_HAND_DISTANCE)
			return best;
		
		return null;
	
	}
	
	public Blob getClosestBlob(Blob blob)
	{
		Blob closestBlob = null;
		double shortestDistance = Double.MAX_VALUE;
		for (Blob b : blobs)
		{
			double distance = Math.pow(b.getCenterX() - blob.getCenterX(),2.0) +
								Math.pow(b.getCenterY() - blob.getCenterY(), 2.0);
			if (distance < shortestDistance)
			{
				shortestDistance = distance;
				closestBlob = b;
			}
		}
		
		return closestBlob;
	}
	
	public TreeMap<Double, Blob> getDistancesToBlob(Blob blob)
	{
		TreeMap<Double, Blob> results = new TreeMap<Double, Blob>();
		
		for (Blob b : blobs)
		{
			double distance = Math.pow(b.getCenterX() - blob.getCenterX(),2.0) +
								Math.pow(b.getCenterY() - blob.getCenterY(), 2.0);
			results.put(distance, b);
		}	
		
		return results;
	}
	
	public List<Blob> getBlobs()
	{
		return blobs;
	}
	
	
}
