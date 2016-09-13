package models.comparators;

import java.util.Comparator;

import features.Feature;

public class ValueComparator implements Comparator<Feature> {

	public ValueComparator() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public int compare(Feature first, Feature second) {

		if (!(first.getValue() instanceof Number))
			throw new ClassCastException("Feature value not of type Number");
		if (!(second.getValue() instanceof Number))
			throw new ClassCastException("Feature value not of type Integer or Double");
		
		double firstValue = ((Number)first.getValue()).doubleValue();
		double secondValue = ((Number)second.getValue()).doubleValue();
		
		if (firstValue < secondValue)
			return -1;
		if (firstValue > secondValue)
			return 1;
		
		return 0;
	}

}
