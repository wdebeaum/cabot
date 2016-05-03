package owlground.spaces;

import org.apache.commons.math3.exception.MathUnsupportedOperationException;
import org.apache.commons.math3.geometry.Space;
import org.jblas.DoubleMatrix;

import java.lang.Math;

/*
 * Won't be used yet.
 */

public class EuclideanSpace extends FeatureSpace {

	public EuclideanSpace( String name, int dim) {
		super(name, dim);
		// TODO Auto-generated constructor stub
	}

	@Override
	public double distance(DoubleMatrix a, DoubleMatrix b) {
		
		
		return a.distance2(b);
	}

	@Override
	public int getDimension() {
		return dimension;
	}

	@Override
	public Space getSubSpace() {
		FeatureSpace subSpace = new EuclideanSpace(name + "-subspace", dimension - 1);
		subSpace.setSpaceManager(spaceManager);
		return subSpace;
	}
}
