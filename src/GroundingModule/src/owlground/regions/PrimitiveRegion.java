package owlground.regions;

import java.util.Collection;

import org.jblas.DoubleMatrix;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.spaces.FeatureSpace;

/*
 * Won't be used yet.
 */

public abstract class PrimitiveRegion extends DataRegion {


	public PrimitiveRegion(FeatureSpace s, AbstractDescriptor descriptor) {
		super(s, descriptor);
		// TODO Auto-generated constructor stub
	}

	public abstract boolean addAttendedPercept(Percept p);

	public abstract DoubleMatrix getCenter();
	
	public boolean subsume(DataRegion r)
	{
		boolean modified = false;
		for (Percept p : r.getPercepts())
		{
			if (addAttendedPercept(p))
				modified = true;
		}
		
		return modified;
		
	}

}
