package models.comparators;

import java.util.Comparator;

import features.Feature;

public class ValueComparator implements Comparator<Feature> {

	public static double EQUALS_EPSILON = .1;
	
	public ValueComparator() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public int compare(Feature first, Feature second) {

		boolean doubleComparison = false;
		if (!(first.getValue() instanceof Number))
			throw new ClassCastException("Feature value not of type Number");
		if (!(second.getValue() instanceof Number))
			throw new ClassCastException("Feature value not of type Integer or Double");
		
		// If one of the values is a double, use an epsilon for equals
		if (first.getValue() instanceof Double || 
				second.getValue() instanceof Double)
			doubleComparison = true;
		
		double firstValue = ((Number)first.getValue()).doubleValue();
		double secondValue = ((Number)second.getValue()).doubleValue();
		
		if (firstValue < secondValue)
			return -1;
		if (firstValue > secondValue)
			return 1;
		
		return 0;
	}

}
