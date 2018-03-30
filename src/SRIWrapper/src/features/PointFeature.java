package features;

import org.jblas.DoubleMatrix;
import geometry.Line;

public class PointFeature extends Feature<DoubleMatrix> {

	private DoubleMatrix value;
	
	public PointFeature(String name) {
		super(name);
		value = new DoubleMatrix();
	}

	@Override
	public DoubleMatrix getValue() {
		// TODO Auto-generated method stub
		return value;
	}

	@Override
	public void setValue(DoubleMatrix newValue) {
		// TODO Auto-generated method stub
		value = newValue;
	}
	
	public PointFeature projectOnto(Line line)
	{
		DoubleMatrix ap = value.sub(line.a);
		DoubleMatrix ab = line.b.sub(line.a);
		DoubleMatrix result = line.a.add(ab.mul(ap.dot(ab) / ab.dot(ab)));
		PointFeature resultPointFeature = new PointFeature("projected");
		resultPointFeature.setValue(result);
		return resultPointFeature;
	}

	public String toString()
	{
		return "Point Feature: " + value.toString();
	}
}
