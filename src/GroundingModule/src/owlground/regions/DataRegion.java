package owlground.regions;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jblas.DoubleMatrix;
import org.jblas.Solve;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;
/*
 * Holds and adjusts size according to Percepts. Belongs to a Space. 
 * Won't be used yet.
 */
public abstract class DataRegion{

	protected FeatureSpace featureSpace;
	protected HashSet<Percept> percepts;
	private DoubleMatrix covariance;
	private DoubleMatrix mean;
	private DoubleMatrix precision;
	protected boolean empty;
	protected boolean modified = true;
	protected AbstractDescriptor descriptor;
	
	public DataRegion(FeatureSpace s, AbstractDescriptor descriptor)
	{
		featureSpace = s;
		percepts = new HashSet<Percept>();
		empty = true;
		this.descriptor = descriptor;
	}

	
	public boolean isEmpty() {
		return empty;
	}

	public HashSet<Percept> getPercepts() {
		return percepts;
	}
	
	public Set<DoubleMatrix> getPerceptValues()
	{
		HashSet<DoubleMatrix> values = new HashSet<DoubleMatrix>();
		for (Percept p : percepts)
		{
			values.add(p.getValue());
		}
		
		return values;
	}
	
	public abstract double squaredDistance(Percept p);
	public abstract double distance(Percept p);
	public abstract DataRegion intersection(DataRegion r);
	public abstract DataRegion union(DataRegion r);
	public abstract DataRegion exclude(Collection<DataRegion> c);
	public boolean update()
	{
		if (!modified)
			return false;
		
		getPrecision();
		modified = false;
		
		return true;
	}
	
	public boolean addAttendedPercept(Percept p)
	{
		percepts.add(p);
		modified = true;
		
		return true;
	}
	
	public boolean addPercepts(Collection<Percept> percepts)
	{
		this.percepts.addAll(percepts);
		modified = true;
		
		return true;
	}
	

	public FeatureSpace getFeatureSpace() {
		return featureSpace;
	}
	
	protected DataRegion emptyClone()
	{
		try {
			DataRegion clone = (DataRegion) super.clone();
			clone.percepts = new HashSet<Percept>();
			clone.empty = true;
			clone.modified = true;
			return clone;
			
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}
	
	public DoubleMatrix getMean()
	{
		
		if (!modified)
			return mean;
		
		DoubleMatrix result = null;
		Collection<DoubleMatrix> values = getPerceptValues();
		for (DoubleMatrix value : values)
		{
			if (result == null)
			{
				result = DoubleMatrix.zeros(featureSpace.getDimension());
			}
			
			result.addi(value);
		}
		result.divi(values.size());
		
		mean = result;
		
		return result;
	}
	
	public DoubleMatrix getCovariance()
	{
		if (!modified)
			return covariance;
		
		DoubleMatrix mean = getMean();
		DoubleMatrix covarianceMatrix = DoubleMatrix.zeros(mean.length, mean.length);
		Collection<DoubleMatrix> values = getPerceptValues();
		
		for (int i = 0; i < mean.length; i++)
		{
			for(int j = 0; j < mean.length; j++)
			{
				double result = 0.0d;
				for (DoubleMatrix value : values)
				{
					result += (value.get(i) - mean.get(i)) *
							  (value.get(j) - mean.get(j));
				}
				
				covarianceMatrix.put(j + i * mean.length, result / values.size());
				
			}
		}
		
		covariance = covarianceMatrix;

		return covarianceMatrix;
	}
	
	public DoubleMatrix getPrecision()
	{
		if (!modified)
			return precision;
		
		double ridgeEpsilon = .0000001;
		HashMap<FeatureSpace,DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
	
		
		DoubleMatrix covariance = getCovariance();
		//Ridging for singular matrices
		covariance.addi(DoubleMatrix.eye(covariance.columns).mul(ridgeEpsilon));
			
		DoubleMatrix covarianceInverse = Solve.solveSymmetric(covariance, DoubleMatrix.eye(covariance.rows));
		
		precision = covarianceInverse;
		
		return covarianceInverse;
	}

}
