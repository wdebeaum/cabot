package features;

import java.util.ArrayList;
import java.util.List;

import org.jblas.DoubleMatrix;

public class LineSegmentFeature implements FeatureGroup {

	private PointFeature start;
	private PointFeature end;
	
	public LineSegmentFeature(PointFeature start, PointFeature end) {
		this.start = start;
		this.end = end;
		
	}

	@Override
	public List<Feature> getFeatures() {
		List<Feature> result = new ArrayList<Feature>();
		result.add(start);
		result.add(end);
		return result;
	}
	
	public Line getLine()
	{
		return new Line(start.getValue(),end.getValue());
	}

}
