package models;

import java.util.*;

import environment.Scene;
import models.comparators.*;
import features.CountFeature;
import features.DecimalFeature;
import features.Feature;
import features.FeatureConstants;

public class FeatureConstraint implements Constraint {

	public enum Operator {LESS,GREATER,GEQ,LEQ,EQUAL}
	public enum ComparisonType {VALUE,DISTANCE,DEFAULT}
	
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
		this.comparisonFeature = new DecimalFeature("value");
		this.comparisonFeature.setValue(value);
		this.comparisonFeature.setConstant(true);
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
	
	public FeatureConstraint(Feature feature, Operator operator, 
			ComparisonType comparisonType) {
		// TODO Auto-generated constructor stub
		this.feature = feature;
		this.operator = operator;
		this.value = -1;
		this.comparisonFeature = new DecimalFeature("value");
		this.comparisonType = comparisonType;
	}
	
	public static Operator operatorFromTRIPS(String tripsConcept, String lex)
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
		case "ONT::QMODIFIER":
			if (lex.equalsIgnoreCase("W::MIN"))
				return Operator.GEQ;
			if (lex.equalsIgnoreCase("W::MAX"))
				return Operator.LEQ;
		case FeatureConstants.HORIZONTAL:
		case FeatureConstants.VERTICAL:
		case FeatureConstants.LINE:
			return Operator.GREATER;
		case "ONT::HAVE":
			return Operator.EQUAL;
		default:
			return null;
		}
	}
	
	
	public boolean isSatisfied(Feature featureToTest, Scene s)
	{
		if (featureToTest.getSource() != null)
			featureToTest.getSource().evaluate(s);
		if (comparisonFeature.getSource() != null)
			comparisonFeature.getSource().evaluate(s);
		Comparator comparator;
		if (comparisonType.equals(ComparisonType.DISTANCE))
			comparator = new DistanceComparator();
		else
			comparator = new ValueComparator();
		switch (operator)
		{
		case LESS:
			return comparator.compare(featureToTest,comparisonFeature) < 0;
		case GREATER:
			return comparator.compare(featureToTest,comparisonFeature) > 0;
		case EQUAL:
			return comparator.compare(featureToTest,comparisonFeature) == 0;
		case GEQ:
			return comparator.compare(featureToTest,comparisonFeature) >= 0;
		case LEQ:
			return comparator.compare(featureToTest,comparisonFeature) <= 0;
		default:
			return false;
			
		}
	}
	
	public boolean isSatisfied(Scene s)
	{
		return isSatisfied(feature,s);	
	}
	
	public String toString()
	{
		if (comparisonFeature != null)
		{
			if (comparisonFeature.isConstant())
				return "Feature Constraint: " + feature.getName() + " " 
									+ operator + " " 
									+ comparisonFeature.getValue();
			else
				return "Feature Constraint: " + feature.getName() + " " 
									+ operator + " " 
									+ comparisonFeature.getName();
		}
		else
			return "Feature Constraint: " + feature.getName() + " " 
			+ operator + " unresolved value";			
	}
	
	public String reason()
	{
		
		System.out.println("Feature " + feature.getPrettyName() + ": " + feature.getValue());
		StringBuilder sb = new StringBuilder();
		String negationString = "";
		if (!isSatisfied(Scene.currentScene))
			negationString = "not";

		sb.append(getPrettyFeatureName(feature.getName()));
		sb.append(" is ");
		sb.append(negationString);
		sb.append(" ");
		sb.append(getPrettyOperatorName(operator));
		sb.append(" ");
		if (comparisonFeature.isConstant())
			sb.append(comparisonFeature.getValue());
		else if (comparisonFeature.getSource() != null)
		{
			sb.append(" the ");
			sb.append(getPrettyFeatureName(comparisonFeature.getName()));
			sb.append(" of the ");
			sb.append(comparisonFeature.getSource().toString());
		}
		else
			sb.append("the " + getPrettyFeatureName(comparisonFeature.getName()));
		
		return sb.toString();
	}
	
	private String getPrettyFeatureName(String feature)
	{
		String result = new String(feature);
		if (result.contains("::"))
			result = feature.split("::")[1];
		if (result.contains("SCALE"))
			result = result.split("-")[0];
		return result.toLowerCase();
	}
	


	@Override
	public String getDescription() {
		// TODO Auto-generated method stub
		return "the " + feature.getPrettyName();
	}

	@Override
	public Feature getFeature() {
		// TODO Auto-generated method stub
		return feature;
	}
	
	public String getPrettyOperatorName(Operator operator)
	{
		switch (operator)
		{
		case LESS:
			return "less than";
		case GREATER:
			return "greater than";
		case EQUAL:
			return "equal to";
		case GEQ:
			return "greater than or equal to";
		case LEQ:
			return "less than or equal to";
		default:
			return "";
			
		}
	}
	
	public String getFeatureName()
	{
		return getFeature().getPrettyName();
	}
	
	@Override
	public boolean equals(Object other)
	{
		if (!(other instanceof FeatureConstraint))
			return false;
		
		FeatureConstraint otherFc = (FeatureConstraint)other;
		
		return otherFc.getFeatureName().equalsIgnoreCase(getFeatureName()) &&
				otherFc.operator == operator && otherFc.value == otherFc.value &&
				comparisonFeature.getName().equalsIgnoreCase(
						otherFc.comparisonFeature.getName());
	}
	
	@Override
	public int hashCode()
	{
		int result = getFeatureName().hashCode() + operator.hashCode() +
				(int)value;
		if (comparisonFeature != null)
			result += comparisonFeature.hashCode();
		
		return result;
	}
	
	@Override
	public boolean setValue(double value)
	{
		this.value = value;
		
		this.comparisonFeature = new CountFeature("value");
		comparisonFeature.setValue((int)value);
		comparisonFeature.setConstant(true);
		return true;
	}
}
