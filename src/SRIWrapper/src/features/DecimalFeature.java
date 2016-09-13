package features;

import java.util.Comparator;

public class DecimalFeature extends Feature<Double> implements Comparable<DecimalFeature> {

	protected double value;
	
	public DecimalFeature(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	@Override
	public Double getValue() {
		// TODO Auto-generated method stub
		return value;
	}

	@Override
	public void setValue(Double newValue) {
		// TODO Auto-generated method stub
		this.value = newValue;
	}

	@Override
	public int compareTo(DecimalFeature other) {
		// TODO Auto-generated method stub
		if (value > other.value)
			return 1;
		if (value < other.value)
			return -1;
		return 0;
	}

}
