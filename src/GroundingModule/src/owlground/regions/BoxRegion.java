package owlground.regions;

import java.util.Collection;

import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Hyperplane;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.apache.commons.math3.geometry.partitioning.Side;
import org.apache.commons.math3.geometry.partitioning.SubHyperplane;
import org.jblas.DoubleMatrix;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;
/*
 * An n-dimensional rectangular region of a Space. Won't be used yet. 
 */
public class BoxRegion extends PerceptualDataRegion {

	protected DoubleMatrix maxCorner;
	protected DoubleMatrix minCorner;
	
	public DoubleMatrix getMaxCorner() {
		return maxCorner;
	}

	public DoubleMatrix getMinCorner() {
		return minCorner;
	}

	public BoxRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		
		maxCorner = DoubleMatrix.zeros(s.getDimension());
		minCorner = DoubleMatrix.zeros(s.getDimension());
	}

	public void setMaxCorner(DoubleMatrix maxCorner) {
		this.maxCorner = maxCorner;
		updateEmpty();
	}

	public void setMinCorner(DoubleMatrix minCorner) {
		this.minCorner = minCorner;
		updateEmpty();
	}
	
	public void updateEmpty()
	{
		if (maxCorner.gt(minCorner).sum() > 0.0)
			empty = false;
		else
			empty = true;		
	}

	public boolean contains(Percept p) {
		return !(p.getValue().gt(maxCorner).sum() < 1 || p.getValue().lt(minCorner).sum() < 1);
	}

	public double edgeDistance(Percept p)
	{
		DoubleMatrix pValue = p.getValue();
		DoubleMatrix closestPoint = pValue.dup();
		
		// Clamp values to find closest point in box
		for (int dim = 0; dim < closestPoint.length; dim++)
		{
			if (closestPoint.get(dim) > maxCorner.get(dim))
				closestPoint.put(dim, maxCorner.get(dim));
			if (closestPoint.get(dim) < minCorner.get(dim))
				closestPoint.put(dim, minCorner.get(dim));
		}
		
		return featureSpace.distance(pValue, closestPoint);
	}


	public boolean addAttendedPercept(Percept p) {
		// Adds the percept and changes the region if needed. Returns true if the region is updated,
		// false otherwise. 
		
		percepts.add(p);
		
		if (!contains(p))
		{
			DoubleMatrix maxCorner = DoubleMatrix.ones(featureSpace.getDimension()).mul(Double.NEGATIVE_INFINITY);
			DoubleMatrix minCorner = DoubleMatrix.ones(featureSpace.getDimension()).mul(Double.POSITIVE_INFINITY);
			for (Percept pPrime: percepts)
			{
				DoubleMatrix pPrimeValue = pPrime.getValue();
				DoubleMatrix greaterIndex = pPrimeValue.gt(maxCorner);
				DoubleMatrix lessIndex = pPrimeValue.lt(minCorner);
				
				maxCorner.put(greaterIndex, pPrimeValue.get(greaterIndex));
				minCorner.put(lessIndex, pPrimeValue.get(lessIndex));

			}
			
			updateEmpty();
			
			return true;
		}
		return false;
	}

	public boolean contains(DataRegion r) {
		if (r instanceof BoxRegion)
		{
			BoxRegion br = (BoxRegion)r;
			// A hack for getting truth values from the results
			return (br.minCorner.gt(minCorner).min() > 0.1 && br.maxCorner.lt(maxCorner).min() > .1);
		}
		return false;
		
	}

	@Override
	public DataRegion intersection(DataRegion r) {
		
		if (r instanceof BoxRegion)
		{
			BoxRegion br = (BoxRegion)r;
			DoubleMatrix intersectionMin = DoubleMatrix.zeros(featureSpace.getDimension());
			DoubleMatrix intersectionMax = DoubleMatrix.zeros(featureSpace.getDimension());

			
			for (int i = 0; i < featureSpace.getDimension(); i++)
			{
				intersectionMin.put(i, Math.max(this.minCorner.get(i), br.getMinCorner().get(i)));
				intersectionMax.put(i, Math.min(this.maxCorner.get(i), br.getMaxCorner().get(i)));
			}
			BoxRegion intersection = new BoxRegion(featureSpace, descriptor);
			intersection.setMaxCorner(intersectionMax);
			intersection.setMinCorner(intersectionMin);
			
			return intersection;
			
		}
		return null;
		
	}

	@Override
	public DataRegion union(DataRegion r) {
		if (r instanceof BoxRegion)
		{
			BoxRegion br = (BoxRegion)r;
			DoubleMatrix intersectionMin = DoubleMatrix.zeros(featureSpace.getDimension());
			DoubleMatrix intersectionMax = DoubleMatrix.zeros(featureSpace.getDimension());
			
			
			
			for (int i = 0; i < featureSpace.getDimension(); i++)
			{
				intersectionMin.put(i, Math.min(this.minCorner.get(i), br.getMinCorner().get(i)));
				intersectionMax.put(i, Math.max(this.maxCorner.get(i), br.getMaxCorner().get(i)));
			}
			BoxRegion intersection = new BoxRegion(featureSpace, descriptor);
			intersection.setMaxCorner(intersectionMax);
			intersection.setMinCorner(intersectionMin);
			
			return intersection;
			
		}
		return null;
	}

	@Override
	public DataRegion exclude(Collection<DataRegion> c) {
		// Not finished
		DoubleMatrix excludedMin = minCorner.dup();
		DoubleMatrix excludedMax = maxCorner.dup();
		
		for (DataRegion r : c)
		{
			if (r instanceof BoxRegion)
			{
				BoxRegion br = (BoxRegion)r;

				for (int i = 0; i < featureSpace.getDimension(); i++)
				{
					excludedMin.put(i, Math.max(excludedMin.get(i), br.getMaxCorner().get(i)));
					excludedMax.put(i, Math.min(excludedMax.get(i), br.getMinCorner().get(i)));
				}
				BoxRegion intersection = new BoxRegion(featureSpace, descriptor);
				intersection.setMaxCorner(excludedMax);
				intersection.setMinCorner(excludedMin);
				
				//return intersection;
				return null;
				
			}
		}
		return null;
	}

	@Override
	public boolean update() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public double squaredDistance(Percept p) {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public double distance(Percept p) {
		// TODO Auto-generated method stub
		return 0;
	}

}
