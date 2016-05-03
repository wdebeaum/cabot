package owlground.regions;

import java.util.Collection;

import org.jblas.DoubleMatrix;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;

/*
 * Won't be used yet.
 */

public class BallRegion extends PrimitiveRegion {
	public BallRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		// TODO Auto-generated constructor stub
	}
	DoubleMatrix center;
	double radius;
	
	@Override
	public boolean addAttendedPercept(Percept p) {
		// TODO Auto-generated method stub
		return false;
	}
	@Override
	public DoubleMatrix getCenter() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DataRegion intersection(DataRegion r) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public DataRegion union(DataRegion r) {
		// TODO Auto-generated method stub
		return null;
	}
	@Override
	public DataRegion exclude(Collection<DataRegion> c) {
		// TODO Auto-generated method stub
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
