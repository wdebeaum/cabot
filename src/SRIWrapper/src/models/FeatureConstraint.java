package models;

import java.util.*;

import environment.Scene;
import models.comparators.*;
import features.CountFeature;
import features.DecimalFeature;
import features.Feature;
import features.FeatureConstants;
import features.FeatureGroup;
import features.UnorderedGroupingFeature;

public class FeatureConstraint implements Constraint {

	public enum Operator {LESS,GREATER,GEQ,LEQ,EQUAL,GREATEST,LEAST,NOTEQUAL}
	public enum ComparisonType {VALUE,DISTANCE,INTRA,DEFAULT}
	
	Feature feature;
	Operator operator;
	ComparisonType comparisonType;
	double value;
	Feature comparisonFeature;
	ReferringExpression comparisonSet;
	boolean existential;
	
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
			if (lex.equalsIgnoreCase("W::MORE-THAN") || 
					lex.equalsIgnoreCase("W::MORE"))
				return Operator.GREATER;
			if (lex.equalsIgnoreCase("W::LESS-THAN"))
				return Operator.LESS;
			if (lex.equalsIgnoreCase("W::EXACT"))
				return Operator.EQUAL;
		case FeatureConstants.HORIZONTAL:
		case FeatureConstants.VERTICAL:
		case FeatureConstants.LINE:
			return Operator.GREATER;
		case "ONT::MIN-VAL":
			return Operator.LEAST;
		case "ONT::MAX-VAL":
			return Operator.GREATEST;
		case "ONT::HAVE":
		case "ONT::HAVE-PROPERTY":
		case "ONT::BE":
		case "ONT::OBJECT-COMPARE":
		case "ONT::IDENTITY-VAL":
		case "ONT::AS-MUCH-AS":
		case "ONT::EQUAL":
		case "ONT::ASSOC-WITH":
		case "ONT::SAME":
		case "ONT::CHANGE": //Hack
			return Operator.EQUAL;
		case FeatureConstants.DIFFERENT:
			return Operator.NOTEQUAL;
		default:
			return null;
		}
	}
	
	public boolean isSatisfied(Feature featureToTest, Scene s)
	{
		return isSatisfied(featureToTest, s, s);
	}
	
	public boolean isSatisfied(Feature featureToTest, Scene subjectScene, Scene totalScene)
	{
		if (featureToTest.getSource() != null)
			featureToTest.getSource().evaluate(subjectScene);
		if (comparisonFeature.getSource() != null)
		{
			ReferringExpression source = comparisonFeature.getSource();
			source.evaluate(totalScene);
			if (source.getPseudoInstance() != null && 
					source.getPseudoInstance().hasFeature(comparisonFeature.getName()))
			{
				System.out.println("Pseudoinstance being tested has " + source.getPseudoInstance().blocks.size() + " blocks.");
				System.out.println("Setting source " + source + " feature value " + comparisonFeature + 
						" to " + source.getPseudoInstance().getFeature(comparisonFeature.getName()).getValue());
				comparisonFeature.setValue(source.getPseudoInstance().getFeature(comparisonFeature.getName()).getValue());
				
			}
			
		}
		
		Feature newComparisonFeature = comparisonFeature;
		
		if (isSuperlative())
		{
			if (operator.equals(Operator.GREATEST) && comparisonFeature instanceof UnorderedGroupingFeature)
			{
				newComparisonFeature = ((UnorderedGroupingFeature)comparisonFeature).
												getMaxFeatureValue(featureToTest.getName());
			}
			if (operator.equals(Operator.LEAST) && comparisonFeature instanceof UnorderedGroupingFeature)
			{
				newComparisonFeature = ((UnorderedGroupingFeature)comparisonFeature).
												getMinFeatureValue(featureToTest.getName());
			}
		}
		
		System.out.println(featureToTest);
		Comparator comparator;
		if (comparisonType.equals(ComparisonType.DISTANCE))
			comparator = new DistanceComparator();
		else
			comparator = new ValueComparator();
		
		// In cases like "The height of all columns are equal, check for all items in a group
		if (operator.equals(Operator.EQUAL) && comparisonFeature instanceof UnorderedGroupingFeature)
		{
			System.out.println("Checking equality over grouping");
			UnorderedGroupingFeature grouping = (UnorderedGroupingFeature)comparisonFeature;
			System.out.println("Feature to test value: " + featureToTest.getValue());
			for (FeatureGroup fg : grouping.getValue())
			{
				if (fg.getFeatures().containsKey(featureToTest.getName()))
				{
					newComparisonFeature = fg.getFeatures().get(featureToTest.getName());
					System.out.println("Comparison feature value " + newComparisonFeature.getValue());
					if (!(comparator.compare(featureToTest,newComparisonFeature) == 0))
						return false;
				}
				else
					return false;

			}
			return true;
		}
		
		
		
		switch (operator)
		{
		case LESS:
			return comparator.compare(featureToTest,newComparisonFeature) < 0;
		case GREATER:
			return comparator.compare(featureToTest,newComparisonFeature) > 0;
		case EQUAL:
			return comparator.compare(featureToTest,newComparisonFeature) == 0;
		case GEQ:
			return comparator.compare(featureToTest,newComparisonFeature) >= 0;
		case LEQ:
			return comparator.compare(featureToTest,newComparisonFeature) <= 0;
		case GREATEST:
			return comparator.compare(featureToTest,newComparisonFeature) > 0;
		case LEAST:
			return comparator.compare(featureToTest,newComparisonFeature) < 0;
		default:
			return false;
			
		}
	}
	
	public boolean isSatisfied(Scene subjectScene, Scene totalScene)
	{
		return isSatisfied(feature,subjectScene,totalScene);	
	}
	
	@Override
	public boolean isSatisfied(Scene s)
	{
		return isSatisfied(s,s);
	}
	
	
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("Feature Constraint \n");
		sb.append(feature.getName() + " ");
		sb.append(operator.toString() + " ");
		
		if (comparisonFeature != null)
		{
			if (comparisonFeature.isConstant())
				sb.append(comparisonFeature.getValue());
			else if (comparisonFeature instanceof UnorderedGroupingFeature)
				sb.append("others");
			else
				sb.append(comparisonFeature.getName());
		}
		else
			sb.append("unresolved value");
		
		sb.append("\nDescription: ");
		sb.append(reason(true) + "\n");
		
		return sb.toString();
	}
	
	public String reason()
	{
		return reason(isSatisfied(Scene.currentScene, Scene.currentScene));

	}
	
	public String reason(boolean satisfied)
	{
		System.out.println("Feature " + feature.getPrettyName() + ": " + feature.getValue());
		StringBuilder sb = new StringBuilder();
		String negationString = "";
		if (!satisfied)
			negationString = "not";

		sb.append(getPrettyFeatureName(feature.getName()));
		sb.append(" is ");
		sb.append(negationString);
		sb.append(" ");
		sb.append(getPrettyOperatorName(operator));
		sb.append(" ");
		if (comparisonFeature.isConstant())
			sb.append(comparisonFeature.getValue());
		else if (comparisonFeature.getSource() != null && !isSuperlative())
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
		case LEAST:
			return "less than";
		case GREATER:
		case GREATEST:
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
	
	public boolean isSuperlative()
	{
		return operator.equals(Operator.GREATEST) || operator.equals(Operator.LEAST);
	}
	
	public boolean isInferred()
	{
		return feature.isInferred() || comparisonFeature.isInferred();
	}

	public boolean isExistential() {
		return existential;
	}

	public void setExistential(boolean existential) {
		this.existential = existential;
	}




}
