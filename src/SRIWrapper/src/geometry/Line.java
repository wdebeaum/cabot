package geometry;

import java.util.Set;

import org.jblas.DoubleMatrix;

public class Line {

	public DoubleMatrix a, b;
	
	public Line(DoubleMatrix a, DoubleMatrix b) {
		this.a = a;
		this.b = b;
	}

	public double distanceToLine(DoubleMatrix point)
	{
		DoubleMatrix ab = b.sub(a);
		double t = ab.project(point);
		
		DoubleMatrix closestPointOnLine = ab.mul(t);
		
		return closestPointOnLine.distance2(point);
		
	}
	
	public static Line lineOfBestFit(Set<DoubleMatrix> points)
	{
		return null;
	}
}
