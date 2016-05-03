package owlground.geometry;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.jblas.DoubleMatrix;

import owlground.utilities.Utilities;

public class TestGeometry {
	
	public static final double EPSILON = .00001;

	public static boolean testCommonsGeometry()
	{
		
		return false;
	}
	
	public static boolean testConvexHull()
	{
		boolean passedTest = true;
		
		if (!testPolyhedronConvexHull())
			passedTest = false;
		if (!testPolygonConvexHull())
			passedTest = false;
		if (!testIntervalConvexHull())
			passedTest = false;
		
		return passedTest;
	}
	
	public static boolean testPolyhedronConvexHull()
	{
		boolean passedTest = true;
		List<DoubleMatrix> points = new ArrayList<DoubleMatrix>();
		points.add(new DoubleMatrix(new double[]{1,0,0}));
		points.add(new DoubleMatrix(new double[]{0,1,0}));
		points.add(new DoubleMatrix(new double[]{0,0,1}));
		points.add(new DoubleMatrix(new double[]{-1,0,0}));
		points.add(new DoubleMatrix(new double[]{0,-1,0}));
		points.add(new DoubleMatrix(new double[]{0,0,-1}));
		points.add(new DoubleMatrix(new double[]{0,0,0}));
		
		QHullWrapper qHull = new QHullWrapper("testpolyhedron",true);
		PolyhedronsSet convexHull = (PolyhedronsSet)(qHull.getConvexHull(points));
		double onPolyhedronDistance = Utilities.squaredDistanceToPolytope(new Vector3D(.5,.5,0), convexHull);
		
		if (onPolyhedronDistance < EPSILON)
		{
			System.out.println("Test passed: Distance was " + onPolyhedronDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + onPolyhedronDistance + ", should be < " + EPSILON);
			passedTest = false;
		}
		
		double abovePolyhedronDistance = Utilities.squaredDistanceToPolytope(new Vector3D(0,2,0), convexHull);
		
		if (Math.abs(abovePolyhedronDistance - 1)  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + abovePolyhedronDistance + ", should be 1");
		}
		else
		{
			System.out.println("Test failed: Distance was " + abovePolyhedronDistance + ", should be 1");
			passedTest = false;
		}
		
		double inPolyhedronDistance = Utilities.squaredDistanceToPolytope(new Vector3D(0,0,0), convexHull);
		
		if (inPolyhedronDistance  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + inPolyhedronDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + inPolyhedronDistance + ", should be < " + EPSILON);
			passedTest = false;
		}

		return passedTest;
	}
	
	public static boolean testPolygonConvexHull()
	{
		boolean passedTest = true;
		
		List<DoubleMatrix> points = new ArrayList<DoubleMatrix>();
		points.add(new DoubleMatrix(new double[]{1,0}));
		points.add(new DoubleMatrix(new double[]{0,1}));
		points.add(new DoubleMatrix(new double[]{0,0}));
		points.add(new DoubleMatrix(new double[]{-1,0}));
		points.add(new DoubleMatrix(new double[]{0,-1}));
		
		QHullWrapper qHull = new QHullWrapper("testpolygon",true);
		PolygonsSet convexHull = (PolygonsSet)(qHull.getConvexHull(points));
		
		double onPolygonDistance = Utilities.squaredDistanceToPolytope(new Vector2D(.5,.5), convexHull);
		
		
		if (onPolygonDistance < EPSILON)
		{
			System.out.println("Test passed: Distance was " + onPolygonDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + onPolygonDistance + ", should be < " + EPSILON);
			passedTest = false;
		}
		
		double abovePolygonDistance = Utilities.squaredDistanceToPolytope(new Vector2D(0,2), convexHull);
		
		if (Math.abs(abovePolygonDistance - 1)  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + abovePolygonDistance + ", should be 1");
		}
		else
		{
			System.out.println("Test failed: Distance was " + abovePolygonDistance + ", should be 1");
			passedTest = false;
		}
		
		double inPolygonDistance = Utilities.squaredDistanceToPolytope(new Vector2D(0,0), convexHull);
		
		if (inPolygonDistance  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + inPolygonDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + inPolygonDistance + ", should be < " + EPSILON);
			passedTest = false;
		}
		
		return passedTest;
	}
	
	public static boolean testIntervalConvexHull()
	{
		boolean passedTest = true;
		
		List<DoubleMatrix> points = new ArrayList<DoubleMatrix>();
		points.add(new DoubleMatrix(new double[]{1}));
		points.add(new DoubleMatrix(new double[]{0}));
		points.add(new DoubleMatrix(new double[]{-1}));
		
		QHullWrapper qHull = new QHullWrapper("testinterval",true);
		IntervalsSet convexHull = (IntervalsSet)(qHull.getConvexHull(points));
		
		double onIntervalDistance = Utilities.squaredDistanceToPolytope(new Vector1D(1), convexHull);
		
		
		if (onIntervalDistance < EPSILON)
		{
			System.out.println("Test passed: Distance was " + onIntervalDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + onIntervalDistance + ", should be < " + EPSILON);
			passedTest = false;
		}
		
		double aboveIntervalDistance = Utilities.squaredDistanceToPolytope(new Vector1D(2), convexHull);
		
		if (Math.abs(aboveIntervalDistance - 1)  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + aboveIntervalDistance + ", should be 1");
		}
		else
		{
			System.out.println("Test failed: Distance was " + aboveIntervalDistance + ", should be 1");
			passedTest = false;
		}
		
		double inIntervalDistance = Utilities.squaredDistanceToPolytope(new Vector1D(0), convexHull);
		
		if (inIntervalDistance  < EPSILON)
		{
			System.out.println("Test passed: Distance was " + inIntervalDistance + ", should be < " + EPSILON);
		}
		else
		{
			System.out.println("Test failed: Distance was " + inIntervalDistance + ", should be < " + EPSILON);
			passedTest = false;
		}
		
		return passedTest;
	}
	
	public boolean checkNormals(PolyhedronsSet polyhedron)
	{
		
		return false;
	}
}
