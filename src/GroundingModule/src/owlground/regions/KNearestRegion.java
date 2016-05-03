/**
 * 
 */
package owlground.regions;

import java.util.Collection;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;

/**
 * @author iperera
 *
 */
public class KNearestRegion extends PerceptualDataRegion {

	public KNearestRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#squaredDistance(owlground.Percept)
	 */
	@Override
	public double squaredDistance(Percept p) {
		double minDistance = Double.POSITIVE_INFINITY;
		
		for (Percept otherPercept : percepts)
		{
			double currentDistance = otherPercept.squaredDistance(p);
			if (currentDistance < minDistance)
				minDistance = currentDistance;
		}
		return minDistance;
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#distance(owlground.Percept)
	 */
	@Override
	public double distance(Percept p) {
		return Math.sqrt(squaredDistance(p));
	}

	/* (non-Javadoc)
	 * @see owlground.DataRegion#update()
	 */
	@Override
	public boolean update() {
		// TODO Auto-generated method stub
		return false;
	}
}
