package owlground.spaces;

public class FeatureSpaceValueTuple {

	private FeatureSpace featureSpace;
	private double value;
	
	public FeatureSpaceValueTuple(FeatureSpace featureSpace, double value) {
		this.featureSpace = featureSpace;
		this.value = value;
	}

	public FeatureSpace getFeatureSpace() {
		return featureSpace;
	}

	public void setFeatureSpace(FeatureSpace featureSpace) {
		this.featureSpace = featureSpace;
	}

	public double getValue() {
		return value;
	}

	public void setValue(double value) {
		this.value = value;
	}
	
}
