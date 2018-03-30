package features;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import geometry.Line;

import org.jblas.DoubleMatrix;

public class LineSegmentFeature implements FeatureGroup {

	private PointFeature start;
	private PointFeature end;
	
	public LineSegmentFeature(PointFeature start, PointFeature end) {
		this.start = start;
		this.end = end;
		
	}

	@Override
	public HashMap<String,Feature> getFeatures() {
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.put(FeatureConstants.ORIGIN,start);
		result.put(FeatureConstants.END,end);
		return result;
	}
	
	public Line getLine()
	{
		return new Line(start.getValue(),end.getValue());
	}

}
