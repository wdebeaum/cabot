package models;

import java.util.*;

import models.comparators.*;
import features.Feature;

public class FeatureConstraint {

	public enum Operator {LESS,GREATER,GEQ,LEQ,EQUAL}
	public enum ComparisonType {VALUE,DISTANCE}
	
	Feature feature;
	Operator operator;
	ComparisonType comparisonType;
	double value;
	Feature comparisonFeature;
	
	
	public FeatureConstraint(Feature feature, Operator operator, 
										ComparisonType comparisonType, double value) {
		// TODO Auto-generated constructor stub
		this.feature = feature;
		this.operator = operator;
		this.value = value;
		this.comparisonType = comparisonType;
		this.comparisonFeature = null;
	}
	
	public FeatureConstraint(Feature feature, Operator operator, 
									ComparisonType comparisonType, Feature comparisonFeature) {
		// TODO Auto-generated constructor stub
		this.feature = feature;
		this.operator = operator;
		this.value = -1;
		this.comparisonFeature = comparisonFeature;
		this.comparisonType = comparisonType;
	}
	
	public static Operator operatorFromTRIPS(String tripsConcept)
	{
		if (tripsConcept == null)
			return null;
		switch (tripsConcept.toUpperCase())
		{
		case "ONT::MORE-VAL":
			return Operator.GREATER;
		case "ONT::MORE":
			return Operator.GREATER;
		case "ONT::LESS-VAL":
			return Operator.LESS;
		case "ONT::LESS":
			return Operator.LESS;
		default:
			return null;
		
		}
	}
	
	public boolean isSatisfied()
	{
		Comparator comparator;
		if (comparisonType.equals(ComparisonType.DISTANCE))
			comparator = new DistanceComparator();
		else
			comparator = new ValueComparator();
		switch (operator)
		{
		case LESS:
			return comparator.compare(feature,comparisonFeature) < 0;
		case GREATER:
			return comparator.compare(feature,comparisonFeature) > 0;
		case EQUAL:
			return comparator.compare(feature,comparisonFeature) == 0;
		case GEQ:
			return comparator.compare(feature,comparisonFeature) >= 0;
		case LEQ:
			return comparator.compare(feature,comparisonFeature) <= 0;
		default:
			return false;
			
		}
	}
	
	public String toString()
	{
		return "Feature Constraint: " + feature.getName() + " " 
									+ operator + " " 
									+ comparisonFeature.getName();
	}
}
