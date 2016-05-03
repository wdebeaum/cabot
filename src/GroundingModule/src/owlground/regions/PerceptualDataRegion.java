package owlground.regions;

import java.util.Collection;

import owlground.objects.AbstractDescriptor;
import owlground.spaces.FeatureSpace;

public abstract class PerceptualDataRegion extends DataRegion {

	public PerceptualDataRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		// TODO Auto-generated constructor stub
	}

	@Override
	public DataRegion intersection(DataRegion r) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DataRegion union(DataRegion r) {
		
		DataRegion result = super.emptyClone();
		result.addPercepts(r.getPercepts());
		result.addPercepts(this.percepts);
		return result;
	}

	@Override
	public DataRegion exclude(Collection<DataRegion> c) {
		// TODO Auto-generated method stub
		return null;
	}
}
