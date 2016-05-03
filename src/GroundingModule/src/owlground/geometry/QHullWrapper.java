package owlground.geometry;

import java.io.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.apache.commons.math3.exception.MathArithmeticException;
import org.apache.commons.math3.exception.NotFiniteNumberException;
import org.apache.commons.math3.geometry.Vector;
import org.apache.commons.math3.geometry.euclidean.oned.IntervalsSet;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.SubPlane;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.Hyperplane;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.apache.commons.math3.geometry.partitioning.RegionFactory;
import org.apache.commons.math3.geometry.partitioning.SubHyperplane;
import org.jblas.DoubleMatrix;

import owlground.utilities.Utilities;
import quickhull3d.Point3d;
import quickhull3d.QuickHull3D;

public class QHullWrapper {

	private ProcessBuilder qHullProcessBuilder = null;
	private Process qHullProcess = null;
	private final String qHullLocation = "../../src/GroundingModule/qhull/bin";
	private final String qHullName = "./qconvex";
	private BufferedWriter qHullWriter = null;
	private BufferedReader qHullReader = null;
	private int dimension;
	private int numFacets;
	private int numRidges;
	private boolean debug;
	private String identifier;
	private double MIN_DISTANCE = .01;
	
	private final double HYPERPLANE_THICKNESS = .0001;
	
	private double[][] inputPoints;
	private double[][] facetNormalsOffsets;
	private double[][] qHullPoints;
	private int[][] planePoints;
	private Vector centroid;
	
	public QHullWrapper(String identifier)
	{
		this.debug = false;
		this.identifier = identifier;
	}
	
	public QHullWrapper(String identifier, boolean debug)
	{
		this.debug = debug;
		this.identifier = identifier;
	}
	
	public void addPoints(Collection<DoubleMatrix> points)
	{
		if (points.size() < 2)
			throw new IllegalArgumentException("Not enough points for convex hull");
		if (points.size() < 3 && dimension > 1)
			throw new IllegalArgumentException("Not enough points for convex hull");
		if (points.size() < 5 && dimension > 2)
			throw new IllegalArgumentException("Not enough points for convex hull");
		
		for (DoubleMatrix point : points)
		{
			dimension = point.length;
			break;
		}
		
		if (dimension > 3)
			throw new IllegalArgumentException("Dimensions higher than 3 not currently supported");
		
		//checkDuplicatePoints(points);
		
		inputPoints = new double[points.size()][dimension];
		DoubleMatrix[] pointArray = points.toArray(new DoubleMatrix[0]);
		
		//pointArray = reshapePoints(pointArray);
		
		DoubleMatrix average = DoubleMatrix.zeros(dimension);
		for (int i = 0; i < points.size(); i++)
		{
			inputPoints[i] = jogglePoint(pointArray[i].toArray());
			//inputPoints[i] = pointArray[i].toArray();
			//System.out.println(pointArray[i]);
			average.addi(pointArray[i]);
		}
		
		average.divi(pointArray.length);
		centroid = Utilities.vectorFromDoubleMatrix(average);
		
		if (debug)
			System.out.println("Read " + points.size() + " points");
	}

	
	private DoubleMatrix[] reshapePoints(DoubleMatrix[] points)
	{
		DoubleMatrix centroid = getAverage(points);
		for (DoubleMatrix point : points)
		{
			point.subi(centroid);
			point.muli(10);
		}
		
		return points;
	}
	
	private double[] jogglePoint(double[] point)
	{
		double[] result = new double[point.length];
		
		for (int i = 0; i < point.length; i++)
		{
			result[i] = point[i] + ((Math.random() - .5) / 500.0);
		}
		return result;
	}
	

	
	private DoubleMatrix getAverage(DoubleMatrix[] points)
	{
		DoubleMatrix average = DoubleMatrix.zeros(dimension);
		for (DoubleMatrix point : points)
		{
			average.addi(point);
		}
		
		return average.div(points.length);
	}
	
	private void checkDuplicatePoints(Collection<DoubleMatrix> points)
	{
		
		for (DoubleMatrix point : points)
		{
			//System.out.println("Point: " + point);
			double minDistance = Double.POSITIVE_INFINITY;
			for (DoubleMatrix other : points)
			{
				if (point == other)
					continue;
				
				double distance = point.squaredDistance(other);
				if (distance < minDistance)
					minDistance = distance;
				
			}
			//System.out.println("Min distance: " + minDistance);
		}
	}
	
	private void writeInputPointsToStream() throws IOException
	{
		System.out.println("Sending " + inputPoints.length + " points");
		qHullWriter.write(dimension + "\n");
		qHullWriter.write(inputPoints.length + "\n");
		
		for (double[] point : inputPoints)
		{
			for (double element : point)
				qHullWriter.write(element + " ");
		}
		qHullWriter.close();
	}
	
	private void readOutputFromStream()
	{
		String line = null;
		int lineNumber = 0;
		PrintWriter printWriter = null;
		try {
			printWriter = new PrintWriter(identifier, "UTF-8");
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (UnsupportedEncodingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
			while ((line = qHullReader.readLine()) != null)
			{
				//System.out.println("Line: " + line);
				if (lineNumber == 0)
				{
					numFacets = Integer.parseInt(line);
					facetNormalsOffsets = new double[numFacets][dimension + 1];
					planePoints = new int[numFacets][dimension];
					if (debug)
						System.out.println("Number of facets: " + numFacets);
				}
				else if (lineNumber - 1 < numFacets)
				{
					String[] planePointsSplit = line.trim().split("\\s+");
					planePoints[lineNumber-1] = new int[planePointsSplit.length];
					for (int i = 0; i < planePointsSplit.length; i++)
					{
						planePoints[lineNumber-1][i] = Integer.parseInt(planePointsSplit[i]);
					}
				}
				// Skip dimensions and number of faces
				else if (lineNumber > numFacets + 2 && lineNumber < (numFacets * 2) + 3)
				{
					//System.out.println(line);
					int facetNumber = lineNumber - (numFacets + 3);
					String[] normalsSplit = line.trim().split("\\s+");
					//System.out.println("Facet number: " + facetNumber);
					facetNormalsOffsets[facetNumber] = new double[normalsSplit.length];
					
					for (int i = 0; i < normalsSplit.length; i++)
					{
						//System.out.println("Facet normal: " + normalsSplit[i]);
						facetNormalsOffsets[facetNumber][i] = Double.parseDouble(normalsSplit[i]);
						
					}
					//Flip sign on normals
					//facetNormalsOffsets[facetNumber][3] = -facetNormalsOffsets[facetNumber][3];
				}
				else if (lineNumber >= (numFacets * 2) + 3)
				{
					//System.out.println("Writing " + line + " to file");
					printWriter.println(line);
				}
				
				lineNumber++;
			}
			printWriter.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private IntervalsSet generateIntervalsSet()
	{
		double max = Double.NEGATIVE_INFINITY;
		double min = Double.POSITIVE_INFINITY;
		
		for (double[] point : inputPoints)
		{
			if (point[0] < min)
				min = point[0];
			if (point[0] > max)
				max = point[0];
		}
		
		return new IntervalsSet(min,max);
		
	}
	
	private PolyhedronsSet generateQuickHullPolyhedronsSet()
	{
		List<Point3d> points = new ArrayList<Point3d>();
		for (double[] point : inputPoints)
		{
			points.add(new Point3d(point[0],point[1],point[2]));
		}
		
		QuickHull3D hull = new QuickHull3D(points.toArray(new Point3d[0]));
		Collection<Hyperplane<Euclidean3D>> planes = new ArrayList<Hyperplane<Euclidean3D>>();
		for (int[] face : hull.getFaces())
		{
			Vector3D p1 = new Vector3D(inputPoints[face[0]]);
			Vector3D p2 = new Vector3D(inputPoints[face[1]]);
			Vector3D p3 = new Vector3D(inputPoints[face[2]]);
			
			if (p1.distanceSq(p2) < MIN_DISTANCE || p1.distanceSq(p3) < MIN_DISTANCE || p2.distanceSq(p3) < MIN_DISTANCE)
			{
				System.out.println("Coincident points");
				break;
			}
			
			
			if (Utilities.isThinTriangle(p1,p2,p3,.05))
			{
				System.out.println("Thin triangle");
				break;
			}
			
			if (Utilities.isThinTriangle(p1,p3,p2,.05))
			{
				System.out.println("Thin triangle");
				break;
			}
			
/*			if (debug)
			{
				System.out.println("Plane: " + face[0] + " " + face[1] + " " + face[2]);
				System.out.println(p1);
				System.out.println(p2);
				System.out.println(p3);
			}*/
			
			Plane currentPlane = new Plane(p1,p2,p3);
			
			
			planes.add(currentPlane);
		}
		RegionFactory regionFactory = new RegionFactory();
		
		try {
			
			PolyhedronsSet result = (PolyhedronsSet)(regionFactory.buildConvex(10,planes.toArray(new Hyperplane[0])));
			if (result != null)
			{
				System.out.println("Polyhedron center: " + result.getBarycenter());
				System.out.println("Points centroid: " + centroid);
			}
			else
				System.out.println("Null polyhedron");
			
			return result;
		}
		catch (NullPointerException npe)
		{
			npe.printStackTrace();
			System.out.println("NOTE: Polyhedron could not be constructed");
			
			return null;
		}
		catch (MathArithmeticException mae)
		{
			mae.printStackTrace();
			System.out.println("NOTE: Polyhedron could not be constructed");
			
			return null;
		}
	
	}
	
	private PolyhedronsSet generatePolyhedronsSet()
	{
		Collection<Plane> planes = new ArrayList<Plane>();
		System.out.println("Number of planes: " + numFacets);
		int coplanarFaces = 0;
		int thinTriangles = 0;
		int coincidentPoints = 0;
		for (int i = 0; i < numFacets; i++)
		{
			Vector3D p1 = new Vector3D(inputPoints[planePoints[i][0]]);
			Vector3D p2 = new Vector3D(inputPoints[planePoints[i][1]]);
			Vector3D p3 = new Vector3D(inputPoints[planePoints[i][2]]);
			
			if (p1.distanceSq(p2) < MIN_DISTANCE || p1.distanceSq(p3) < MIN_DISTANCE || p2.distanceSq(p3) < MIN_DISTANCE)
			{
				//System.out.println("Coincident points");
				coincidentPoints++;
				continue;
			}
			
			
			if (Utilities.isThinTriangle(p3,p2,p1,.1))
			{
				//System.out.println("Thin triangle");
				thinTriangles++;
				continue;
			}
			
			if (Utilities.isThinTriangle(p3,p1,p2,.1))
			{
				//System.out.println("Thin triangle");
				thinTriangles++;
				continue;
			}
			
			Plane currentPlane = new Plane(p3,p2,p1);
			
			Vector3D normal = currentPlane.getNormal();


			//Plane currentPlane = new Plane(p1, normal);
			
			if (currentPlane.getOffset(centroid) > 0)
			{
				//System.out.print("Swapped normal from " + currentPlane.getNormal());
				currentPlane = new Plane(p1,normal.negate());
				//System.out.println(" to " + currentPlane.getNormal());
			}

			boolean coplanar = false;
			for (Plane p : planes)
			{
				if (normal.dotProduct(p.getNormal()) > .9)
				{
					//System.out.println("Coplanar");
					coplanarFaces++;
					coplanar = true;
					break;
				}
			}
			if (!coplanar)
				planes.add(currentPlane);
			
		}
		System.out.println(thinTriangles + " thin triangles");
		System.out.println(coplanarFaces + " coplanar faces");
		System.out.println(coincidentPoints + " coincident points");
		System.out.println(planes.size() + " planes kept");
		RegionFactory regionFactory = new RegionFactory();
		
		try {
	
			PolyhedronsSet result = (PolyhedronsSet)(regionFactory.buildConvex(10,planes.toArray(new Hyperplane[0])));
			if (result != null)
			{
				System.out.println("Polyhedron center: " + result.getBarycenter());
				System.out.println("Points centroid: " + centroid);
			}
			else
				System.out.println("Null polyhedron");
			
			return result;
		}
		catch (NullPointerException npe)
		{
			npe.printStackTrace();
			System.out.println("NOTE: Polyhedron could not be constructed");
			
			return null;
		}
		catch (MathArithmeticException mae)
		{
			mae.printStackTrace();
			System.out.println("NOTE: Polyhedron could not be constructed");
			
			return null;
		}
		
		
	}
	
/*	private PolyhedronsSet generatePolyhedronsSet()
	{
		Collection<SubHyperplane<Euclidean3D>> subPlanes = new ArrayList<SubHyperplane<Euclidean3D>>();
		for (int i = 0; i < numFacets; i++)
		{
			Vector3D p1 = new Vector3D(inputPoints[planePoints[i][0]]);
			Vector3D p2 = new Vector3D(inputPoints[planePoints[i][1]]);
			Vector3D p3 = new Vector3D(inputPoints[planePoints[i][2]]);
			//Plane currentPlane = new Plane(p1,p2,p3);
			//Plane currentPlane = new Plane(p3,p2,p1);
			PolygonsSet subRegion;
			
			
			if (currentPlane.getOffset(centroid) > 0)
			{
				System.out.print("Swapped normal from " + currentPlane.getNormal());
				currentPlane = new Plane(p1,p2,p3);
				System.out.println(" to " + currentPlane.getNormal());
			}
			Vector3D normal = new Vector3D(facetNormalsOffsets[i][0],
					facetNormalsOffsets[i][1],
					facetNormalsOffsets[i][2]);
			Plane currentPlane = new Plane(p1, normal);
			
			System.out.println("Offset from centroid: " + currentPlane.getOffset(centroid));

			subRegion = new PolygonsSet(HYPERPLANE_THICKNESS, 
									currentPlane.toSubSpace(p3),
									currentPlane.toSubSpace(p2),
									currentPlane.toSubSpace(p1),
									currentPlane.toSubSpace(p3));

			if (Double.isInfinite(subRegion.getSize()))
			{
				subRegion = new PolygonsSet(HYPERPLANE_THICKNESS, 
						currentPlane.toSubSpace(p1),
						currentPlane.toSubSpace(p2),
						currentPlane.toSubSpace(p3),
						currentPlane.toSubSpace(p1));
			}
			System.out.println("Generated subregion of size: " + subRegion.getSize());
			SubPlane subPlane = new SubPlane(currentPlane,subRegion);
			System.out.println("Generated subplane of size: " + subPlane.getSize());
			subPlanes.add(subPlane);
		}
		return new PolyhedronsSet(subPlanes);
	}*/
	
/*	private PolygonsSet createTriangleFromPoints(Vector2D p1, Vector2D p2, Vector2D p3)
	{
		ArrayList<SubHyperplane<Euclidean2D>> edges = new ArrayList<SubHyperplane<Euclidean2D>>();
		edges.add(buildSegment(p1,p2));
		for (int i = 0; i < vertices.length; ++i) {
			int l = vertices[i].length;
			for (int j = 0; j < l; ++j) {
				edges.add(buildSegment(vertices[i][j], vertices[i][(j + 1) % l]));
			}
		}
		return new PolygonsSet(edges);
	}
	
	private SubHyperplane<Euclidean2D> buildSegment(Vector2D start, Vector2D end) {
		Line   line  = new Line(start, end);
		double lower = (line.toSubSpace(start)).getX();
		double upper = (line.toSubSpace(end)).getX();
		return new SubLine(line, new IntervalsSet(lower, upper));
	}*/
	
	private PolygonsSet generatePolygonsSet()
	{
		Collection<SubHyperplane<Euclidean2D>> subLines = new ArrayList<SubHyperplane<Euclidean2D>>();
		for (int i = 0; i < numFacets; i++)
		{
			Vector2D p1 = new Vector2D(inputPoints[planePoints[i][0]]);
			Vector2D p2 = new Vector2D(inputPoints[planePoints[i][1]]);

			
			SubLine subLine = new SubLine(p1,p2);
			subLines.add(subLine);
		}
		return new PolygonsSet(subLines);
	}
	
	public Region generateConvexHull()
	{
		if (dimension == 1)
			return generateIntervalsSet();
		if (dimension == 3)
			return generateQuickHullPolyhedronsSet();
		
		try {
			writeInputPointsToStream();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		readOutputFromStream();
		
		Region result;
		
		if (dimension == 3)
			result = generatePolyhedronsSet();
		else if (dimension == 2)
			result = generatePolygonsSet();
		else
			return null;
		
		return result;
	}
	
	public Region getConvexHull(Collection<DoubleMatrix> points)
	{
		initialize();
		addPoints(points);
		
		System.out.println(identifier);
		
		return generateConvexHull();
	}
	
	public Region getConvexHullVector3D(Collection<Vector3D> points)
	{
		return getConvexHull(Utilities.doubleMatrixFromVector3DCollection(points));
	}
	
	public void initialize()
	{
		qHullProcessBuilder = new ProcessBuilder(qHullName,"Qj","i","n","G","Ga");
		qHullProcessBuilder.directory(new File(qHullLocation));
		System.out.println("Working directory: " + System.getProperty("user.dir"));
		try {
			qHullProcess = qHullProcessBuilder.start();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.out.println("Initializing QHull process failed");
			e.printStackTrace();
		}
		qHullWriter = new BufferedWriter(new OutputStreamWriter(qHullProcess.getOutputStream()));
		qHullReader = new BufferedReader(new InputStreamReader(qHullProcess.getInputStream()));

	}
	
}
