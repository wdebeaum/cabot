package owlground.utilities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.math3.geometry.Space;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.oned.SubOrientedPoint;
import org.apache.commons.math3.geometry.euclidean.oned.Vector1D;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.SubPlane;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Line;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.AbstractSubHyperplane;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.BoundaryAttribute;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.jblas.DoubleMatrix;

public class Utilities {

	public static <T extends Space> Vector<T> vectorFromDoubleMatrix(DoubleMatrix dm)
	{
		if (dm == null)
			return null;
		
		if (dm.length == 1)
			return (Vector<T>)(new Vector1D(dm.get(0)));
		if (dm.length == 2)
			return (Vector<T>)(new Vector2D(dm.data));
		if (dm.length == 3)
			return (Vector<T>)(new Vector3D(dm.data));
			
		return null;
	}
	
	public static 
	<T extends Comparable<? super T>> List<T> asSortedList(Collection<T> c) {
	  List<T> list = new ArrayList<T>(c);
	  java.util.Collections.sort(list);
	  return list;
	}
	
	public static DoubleMatrix doubleMatrixFromVector(Vector v)
	{
		if (v == null)
			return null;
		
		if (v instanceof Vector1D)
		{
			return new DoubleMatrix(new double[]{((Vector1D)v).getX()});
		}
		else if (v instanceof Vector2D)
		{
			return new DoubleMatrix(new double[]{((Vector2D)v).getX(), ((Vector2D)v).getY()});
		}
		else if (v instanceof Vector3D)
		{
			return new DoubleMatrix(new double[]{((Vector3D)v).getX(), ((Vector3D)v).getY(), ((Vector3D)v).getZ()});
		}
		return null;
	}
	
	public static Collection<DoubleMatrix> doubleMatrixFromVector3DCollection(Collection<Vector3D> vectors)
	{
		HashSet<DoubleMatrix> result = new HashSet<DoubleMatrix>();
		
		for (Vector3D v : vectors)
		{
			result.add(doubleMatrixFromVector(v));
		}
		
		return result;
	}
	
	public static <T extends Space> double squaredDistanceToPolytope(DoubleMatrix dm, Region<T> polytope)
	{
		return squaredDistanceToPolytope(Utilities.<T>vectorFromDoubleMatrix(dm), polytope);
	}
	
	public static boolean isThinTriangle(Vector3D a, Vector3D b, Vector3D c, double minANgle)
	{
		Vector3D ab = b.subtract(a);
		Vector3D ac = c.subtract(a);
		double bacAngle = Math.acos(ab.dotProduct(ac) / (ab.getNorm() * ac.getNorm()));
		//System.out.println("BAC angle: " + Math.toDegrees(bacAngle));
		if (bacAngle + minANgle > Math.PI || bacAngle - minANgle < 0)
			return true;
		return false;
	}
	
	public static <T extends Space> double squaredDistanceToPolytope(Vector<T> v, Region<T> polytope)
	{
		if (polytope.checkPoint(v) != Region.Location.OUTSIDE)
			return 0.0;
		
		BSPTree currentCell;
		if (polytope instanceof IntervalsSet)
			currentCell = polytope.getTree(false).getCell(v);
		else
			currentCell = polytope.getTree(true).getCell(v);
		
		double minDistance = Double.POSITIVE_INFINITY;
		
		while (currentCell != null)
		{
			//Polyhedron case
			if (currentCell.getCut() instanceof SubPlane)
			{
				Object attribute = currentCell.getAttribute();
				if (attribute instanceof BoundaryAttribute)
				{
					SubPlane subPlane = (SubPlane)((BoundaryAttribute)attribute).getPlusOutside();
					
//					if (subPlane == null)
//						subPlane = (SubPlane)((BoundaryAttribute)attribute).getPlusInside();
					if (subPlane == null)
					{
						currentCell = currentCell.getParent();
						continue;
					}
					
					double regionSize = subPlane.getSize();
					//System.out.println("Boundary Region size: " + regionSize);
					
					//System.out.println("SubPlane cut");
					
					Vector3D closestPoint = closestPointOn3DPolygon(subPlane, (Vector3D)v);
					double distance = closestPoint.distanceSq((Vector<Euclidean3D>)v);
					
					//System.out.println("Min distance for node: " + distance);
					
					if (distance < minDistance)
						minDistance = distance;
					
				}
//				else
//				{
//					System.out.println("Leaf node");
//				}

			}
			else if (currentCell.getCut() instanceof SubLine)
			{
				Object attribute = currentCell.getAttribute();
				if (attribute instanceof BoundaryAttribute)
				{
					SubLine subLine = (SubLine)((BoundaryAttribute)attribute).getPlusOutside();
					double regionSize = subLine.getSize();
					//System.out.println("Subline cut");
					
					Vector2D start = subLine.getSegments().get(0).getStart();
					Vector2D end = subLine.getSegments().get(0).getEnd();
					double distance = squaredDistanceToLineSegment(start,end,(Vector2D)v);
					
					//System.out.println("Min distance for node: " + distance);
					
					if (distance < minDistance)
						minDistance = distance;
					
				}
//				else
//				{
//					System.out.println("Leaf node");
//				}

			}
			else if (currentCell.getCut() instanceof SubOrientedPoint)
			{
				//System.out.println("SubOriented Point cut");
				SubOrientedPoint subPoint = (SubOrientedPoint)currentCell.getCut();
				double distance = Math.pow(subPoint.getHyperplane().getOffset((Vector<Euclidean1D>)v),2);
				
				//System.out.println("Min distance for node: " + distance);
				
				if (distance < minDistance)
					minDistance = distance;
			}
			currentCell = currentCell.getParent();
		}
		
		//System.out.println("Min distance for path: " + minDistance);
		
		return minDistance;
	}
	
	public static double squaredDistanceToLineSegment(Vector2D start, Vector2D end, Vector2D point)
	{
		
		Vector2D closestPoint = closestPointOnLineSegment(start,end,point);
		return point.distanceSq(closestPoint);
	}
	
	public static Vector3D closestPointOn3DPolygon(SubPlane subPlane, Vector3D point)
	{
		//System.out.println("Region size: " + subPlane.getSize());
		Region<Euclidean2D> polygon = subPlane.getRemainingRegion();
		//System.out.println("Subregion size: " + polygon.getSize());
		Plane plane = (Plane)(subPlane.getHyperplane());
		Vector2D projectedPoint = plane.toSubSpace((Vector<Euclidean3D>)point);
		//System.out.println("Projected point: " + projectedPoint);
		Vector2D closestPoint = closestPointOnPolygon((PolygonsSet)polygon, projectedPoint);
		//System.out.println("Closest point: " + closestPoint);
		Vector3D closestPointIn3D = plane.toSpace(closestPoint);
		//System.out.println("Closest point in 3D: " + closestPointIn3D);
		
		return closestPointIn3D;
		
		
	}
	
	public static Vector2D closestPointOnLineSegment(Vector2D start, Vector2D end, Vector2D point)
	{
		Line line = new Line(start,end);
		SubLine subLine = new SubLine(start,end);
		Vector1D projectedPoint = line.toSubSpace(point);
	
		Region<Euclidean1D> remainingRegion = subLine.getRemainingRegion();
		// Check if point projects onto line
		if (!(remainingRegion.checkPoint(projectedPoint) == Region.Location.OUTSIDE))
		{
			Vector2D linePoint = line.toSpace(projectedPoint);
			return linePoint;
		}
		// Otherwise, the closest points are one of the two endpoints
		else
		{
			if (point.distanceSq(start) < point.distanceSq(end))
				return start;
			else
				return end;
		}
	}	
	
	public static HashSet<Vector3D> getPoints(PolyhedronsSet polyhedron)
	{
		HashSet<Vector3D> points = new HashSet<Vector3D>();
		points.addAll(getPointsRecurse(polyhedron.getTree(true)));
		return points;
	}
	
	/*
	 * Paraphrased from arwillis on Commons Math newsgroup
	 */
	private static HashSet<Vector3D> getPointsRecurse(BSPTree<Euclidean3D> tree)
	{
		HashSet<Vector3D> points = new HashSet<Vector3D>();
		if (!(tree.getAttribute() instanceof BoundaryAttribute))
			return points;
		SubPlane subPlane = (SubPlane) ((BoundaryAttribute) tree.getAttribute()).getPlusOutside();
		
		if (subPlane == null)
			return points;
		
        Region<Euclidean2D> region = ((AbstractSubHyperplane) subPlane).getRemainingRegion();
        PolygonsSet polygonset = (PolygonsSet) region;
        Vector2D[][] vertices = polygonset.getVertices();
        Vector2D curVertex = null;
        Plane plane = (Plane) subPlane.getHyperplane();

        for (int polygonIdx = 0; polygonIdx < vertices.length; polygonIdx++) {
            Vector3D[] vertices3D = new Vector3D[vertices[polygonIdx].length];
            for (int vertexIdx = 0; vertexIdx < vertices3D.length; vertexIdx++) {
                curVertex = vertices[0][vertexIdx];
                if (curVertex != null) {
                	//System.out.println("Found point: " + curVertex);
                    points.add(plane.toSpace(curVertex));
                }
            }
        }
        
        if (tree.getMinus() != null)
        	points.addAll(getPointsRecurse(tree.getMinus()));
        if (tree.getPlus() != null)
        	points.addAll(getPointsRecurse(tree.getPlus()));
        
        return points;
	}
	
	// Does not handle infinite edges correctly but gives an answer
	public static double squaredDistanceToPolygon(PolygonsSet polygons, Vector2D point)
	{
		Vector2D closestPoint = closestPointOnPolygon(polygons, point);
		return point.distanceSq(closestPoint);
	}
	
	// Does not handle infinite edges correctly but gives an answer
	public static Vector2D closestPointOnPolygon(PolygonsSet polygons, Vector2D point)
	{
		if (polygons.checkPoint(point) != Region.Location.OUTSIDE)
			return point;
		
		double minDistance = Double.POSITIVE_INFINITY;
		Vector2D closestPoint = null;
		
		for (Vector2D[] loop : polygons.getVertices())
		{
			boolean openLoop = false;
			for (int i = 0; i < loop.length; i++)
			{
				if (loop[i] == null)
				{
					openLoop = true;
					continue;
				}
				double distance = Double.POSITIVE_INFINITY;
				Vector2D currentPoint = null;
				
				if (i + 1 == loop.length)
				{
					if (openLoop)
					{
						currentPoint = closestPointOnLineSegment(loop[i], loop[1], point);
					}
					else
					{
						currentPoint = closestPointOnLineSegment(loop[i], loop[0], point);
					}
				}
				else
				{
					currentPoint = closestPointOnLineSegment(loop[i], loop[i+1], point);
				}
				
				distance = point.distanceSq(currentPoint);
				
				if (distance < minDistance)
				{
					minDistance = distance;
					closestPoint = currentPoint;
				}
			}
		}
		return closestPoint;
	}
	
	public static double round(double number, double digits)
	{
		long truncation = (long)(number * Math.pow(10, digits));
		return ((double)(truncation)) / Math.pow(10, digits);
	}
	
	public static String quoteEscaped(String input)
	{
		return input.replace("\\","\\\\").replace("\"", "\\\"").replaceAll("[\\p{Z}\\s]", " ").trim();
	}
}
