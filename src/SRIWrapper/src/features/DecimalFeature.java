package features;

import java.util.Comparator;

public class DecimalFeature extends Feature<Double> implements Comparable<DecimalFeature> {

	protected double value;
	protected double threshold;
	
	public DecimalFeature(String name) {
		super(name);
		threshold = .5;
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
		constant = true;
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
	
	public boolean isTrue()
	{
		return value > threshold;
	}
	
	public DecimalFeature getThreshold()
	{
		DecimalFeature toReturn = new DecimalFeature("threshold");
		toReturn.setValue(threshold);
		
		return toReturn;
	}
	
	public static DecimalFeature getDefaultDecimalMinimum()
	{
		DecimalFeature toReturn = new DecimalFeature("threshold");
		toReturn.setValue(.8);
		
		return toReturn;
	}

}
