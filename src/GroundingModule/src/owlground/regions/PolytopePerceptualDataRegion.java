package owlground.regions;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.math3.geometry.Space;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.jblas.DoubleMatrix;

import owlground.geometry.QHullWrapper;
import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;
import owlground.utilities.Utilities;
import quickhull3d.Point3d;
import quickhull3d.QuickHull3D;

public class PolytopePerceptualDataRegion<T extends Space> extends PerceptualDataRegion {

	protected Region<T> polytope;
	public static boolean convexHullGenerationEnabled = true;
	
	public PolytopePerceptualDataRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		
		switch (s.getDimension())
		{
			case 1:	
				polytope = (Region<T>)(new IntervalsSet());
				break;
			case 2:
				polytope = (Region<T>)(new PolygonsSet());
				break;
			case 3:
				polytope = (Region<T>)(new PolyhedronsSet());
				break;
			default:
				throw new InvalidParameterException("A polytope region can't be created in dimensions 0 or > 3");
		}
		
		modified = true;
		
	}

	public boolean contains(Percept p) {
		update();
		return (polytope.checkPoint(Utilities.<T>vectorFromDoubleMatrix(p.getValue())) != Region.Location.OUTSIDE);
	}

	
	public boolean contains(DataRegion r, double containedRatio) {
		update();
		if (r instanceof PolytopePerceptualDataRegion<?>)
		{
			Region otherRegion = ((PolytopePerceptualDataRegion<?>)r).getPolytope();
			return polytope.contains(otherRegion);
		}
		
		int totalPercepts = 0;
		int containedPercepts = 0;
		for (Percept p : r.getPercepts())
		{
			totalPercepts++;
			if (contains(p))
				containedPercepts++;
		}
		
		if (totalPercepts == 0)
			return false;
		
		return (((double)containedPercepts / totalPercepts) > containedRatio);
		
	}
	
	public boolean contains(DataRegion r)
	{
		update();
		return contains(r,1.0);
	}

	

	public Region getPolytope() {
		update();
		return polytope;
	}

	public void setPolytope(Region polytope) {
		this.polytope = polytope;
	}
	
	public boolean addAttendedPercept(Percept p)
	{
		percepts.add(p);
		
		modified = true;
		
		return true;
	}

	@Override
	public boolean update() {
		
		if (!modified)
			return false;
		

		
		super.update();
		
		if (convexHullGenerationEnabled == false)
			return false;
		
		if (featureSpace.getName().equals("rgb_var"))
			return false;
		System.out.println("Getting convex hull for region in space " + featureSpace.getName() + " with dimension " + featureSpace.getDimension());
		
		if (getPerceptValues().size() < 6)
			return false;
		
		QHullWrapper qHullWrapper = new QHullWrapper(featureSpace.getName() + "-" + descriptor.getFileSafeName(), true);
		polytope = qHullWrapper.getConvexHull(getPerceptValues());
		
		//System.out.println("Polytope size: " + polytope.getSize());
		if (polytope != null)
		{
			double percentageOut = checkPoints(polytope) * 100;
			System.out.println("Polytope volume: " + polytope.getSize());
			System.out.println(percentageOut + "% of points are outside the polytope");
/*			HashSet<Vector3D> newPolytopeVertices = Utilities.getPoints((PolyhedronsSet)polytope);
			if (newPolytopeVertices.size() > 5 && !Double.isInfinite(polytope.getSize()))
			{
				QHullWrapper qHullModWrapper = new QHullWrapper(featureSpace.getName() + "-" + descriptor.getFileSafeName() + "-mod", false);
				Region polytopeMod = qHullModWrapper.getConvexHullVector3D(newPolytopeVertices);
				if (polytopeMod != null)
				{
					double modPercentageOut = checkPoints(polytopeMod) * 100;
					System.out.println("Modified Polytope volume: " + polytopeMod.getSize());
					System.out.println(modPercentageOut + "% of points are outside the polytope");
				}
			}
			else 
			{
				System.out.println("Not enough modified points");
			}*/
		}

		modified = false;
		return true;
	}

	@Override
	public double squaredDistance(Percept p) {
		update();
		return Utilities.squaredDistanceToPolytope(p.getValue(), polytope);
	}

	@Override
	public double distance(Percept p) {
		return Math.sqrt(squaredDistance(p));
	}

	private double checkPoints(Region polytope)
	{
		int pointsOutside = 0;
		for (Percept p: percepts)
		{
			if (featureSpace.getDimension() == 3)
			{
				Vector point = Utilities.vectorFromDoubleMatrix(p.getValue());
				
				if (polytope.checkPoint(point) == Region.Location.OUTSIDE)
				{
					//System.out.println("Point " + p.getValue() + " not in polytope");
					//System.out.println("Point " + point + " not in polytope");
					double distance = Utilities.squaredDistanceToPolytope(point, polytope);
					//System.out.println("Distance: " + distance);
					pointsOutside++;
				}
			}
		}
		return ((double)pointsOutside) / percepts.size();
	}

}
