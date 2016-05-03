package owlground.regions;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jblas.DoubleMatrix;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;

public class KNearestMalahanobisRegion extends KNearestRegion {

	public KNearestMalahanobisRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#squaredDistance(owlground.Percept)
	 */
	@Override
	public double squaredDistance(Percept p) {
		List<Double> results = getKNearestMahalanobisSquared(p,1);
		if (results.size() > 0)
			return results.get(0);
		else
			return Double.POSITIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#distance(owlground.Percept)
	 */
	@Override
	public double distance(Percept p) {
		List<Double> results = getKNearestMahalanobis(p,1);
		if (results.size() > 0)
			return results.get(0);
		else
			return Double.POSITIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#update()
	 */
	@Override
	public boolean update() {
		// TODO Auto-generated method stub
		return false;
	}
	
	public List<Double> getKNearestMahalanobis(Percept p, int k)
	{
		List<Double> results = getKNearestMahalanobisSquared(p, k);
		for (Double value: results)
		{
			value = Math.sqrt(value);
		}
		
		return results;
	}
	
	public List<Double> getKNearestMahalanobisSquared(Percept p, int k)
	{
		List<Double> results = new ArrayList<Double>();
		
		DoubleMatrix precisionMatrix = getPrecision();
		for (DoubleMatrix value : getPerceptValues())
		{
			DoubleMatrix difference = p.getValue().sub(value);

			DoubleMatrix left = difference.transpose().mmul(precisionMatrix);

			double scaledDistance = left.mmul(difference).get(0);
			results.add(scaledDistance);
		}
		Collections.sort(results);
		return results.subList(0, Math.min(k, results.size()));		
	}
}
