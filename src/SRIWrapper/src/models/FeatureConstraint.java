package models;

import java.util.*;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.StructureInstance;
import models.comparators.*;
import utilities.KQMLUtilities;
import features.CountFeature;
import features.DecimalFeature;
import features.Feature;
import features.FeatureConstants;

public class FeatureConstraint implements Constraint {

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
	
	public FeatureConstraint(Feature feature, Operator operator, 
			ComparisonType comparisonType) {
		// TODO Auto-generated constructor stub
		this.feature = feature;
		this.operator = operator;
		this.value = -1;
		this.comparisonFeature = null;
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
	
	public String reason()
	{
		
		System.out.println("Feature " + feature.getPrettyName() + ": " + feature.getValue());
		StringBuilder sb = new StringBuilder();
		String negationString = "";
		if (!isSatisfied())
			negationString = "not";

		sb.append(getPrettyFeatureName(feature.getName()));
		sb.append(" is ");
		sb.append(negationString);
		sb.append(" ");
		sb.append(operator.toString().toLowerCase());
		if (operator.equals(Operator.EQUAL))
			sb.append(" to ");
		else
			sb.append(" than ");
		if (comparisonFeature.isConstant())
			sb.append(comparisonFeature.getValue());
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
	
	public static boolean isComparatorInContext(KQMLList context)
	{
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			if (termList.getKeywordArg(":INSTANCE-OF") != null)
			{
				String instance = termList.getKeywordArg(":INSTANCE-OF").stringValue();
				String lex = "";
				if (termList.getKeywordArg(":LEX") != null)
					lex = termList.getKeywordArg(":LEX").stringValue();
				if (FeatureConstraint.operatorFromTRIPS(instance, lex) != null)
					return true;
			}
		}
		
		return false;
	}
	
	public static FeatureConstraint extractFeature(StructureInstance structureInstance, KQMLList term, KQMLList context)
	{

		String scale = "unnamed";
		Feature extractedFeature = null;
		Feature groundFeature = null;
		String ground = null;
		String operatorString = null;
		String descriptorFeatureString = null;
		KQMLList groundTerm = null;
		
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return null;
		
		// Get the scale for naming the feature
		if (term.getKeywordArg(":SCALE") != null)
		{
			scale = term.getKeywordArg(":SCALE").stringValue();
			if (scale.equalsIgnoreCase("ONT::LINEAR-SCALE"))
			{
				scale = term.getKeywordArg(":INSTANCE-OF").stringValue();
			}
			extractedFeature = structureInstance.getFeature(scale);
			System.out.println("Scale: " + scale);
		}
		else if (term.getKeywordArg(":FIGURE") != null)
		{
			String figureSymbol = term.getKeywordArg(":FIGURE").stringValue();
			KQMLList figureTerm = 
					KQMLUtilities.findTermInKQMLList(figureSymbol, context);
			
			descriptorFeatureString = figureTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			extractedFeature = structureInstance.getFeature(descriptorFeatureString);
			System.out.println("Scale: " + descriptorFeatureString);
		}
		
		// Get the comparison object or property
		if (term.getKeywordArg(":GROUND") != null)
		{
			ground = term.getKeywordArg(":GROUND").stringValue();
			groundTerm = KQMLUtilities.findTermInKQMLList(ground, context);
			System.out.println("Ground: " + ground);
		}
		
		// Get the ONT::MORE-VAL or equivalent
		operatorString = term.getKeywordArg(":INSTANCE-OF").stringValue();
		String lex = "";
		if (term.getKeywordArg(":LEX") != null)
			lex = term.getKeywordArg(":LEX").stringValue();
		
		FeatureConstraint.Operator operator = 
				FeatureConstraint.operatorFromTRIPS(operatorString, lex);
		
		// The instance-of is not actually an operator, but a descriptor
		if (operator == null && (operatorString.equalsIgnoreCase(FeatureConstants.HORIZONTAL) ||
								operatorString.equalsIgnoreCase(FeatureConstants.VERTICAL) ||
								operatorString.equalsIgnoreCase(FeatureConstants.LINE)))
		{
			descriptorFeatureString = operatorString;
			if (structureInstance.hasFeature(descriptorFeatureString))
				extractedFeature = structureInstance.
											getFeature(descriptorFeatureString);
			operator = FeatureConstraint.Operator.GREATER;
			groundFeature = DecimalFeature.getDefaultDecimalMinimum();
		}
		
		// Number constraints
		if (operator == null && operatorString.equalsIgnoreCase(FeatureConstants.NUMBER))
		{
			// If there's a comparator, that should take precedence
			if (FeatureConstraint.isComparatorInContext(context))
				return null;
			else if (term.getKeywordArg(":VALUE") != null)
			{
				operator = FeatureConstraint.Operator.EQUAL;
				extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
				groundFeature = new CountFeature(FeatureConstants.NUMBER);
				int value = Integer.parseInt(term.getKeywordArg(":VALUE").stringValue());
				groundFeature.setValue(value);
				groundFeature.setConstant(true);
			}
		}
		
		
		if (groundTerm != null)
		{
			if (groundTerm.getKeywordArg(":INSTANCE-OF") != null)
			{
				String groundScale = groundTerm.getKeywordArg(":INSTANCE-OF").stringValue();
				System.out.println("Ground Scale: " + groundScale);
				groundFeature = structureInstance.getFeature(groundScale);
				
			}
			else if (groundTerm.getKeywordArg(":SCALE") != null)
			{
				String groundScale = groundTerm.getKeywordArg(":SCALE").stringValue();
				System.out.println("Ground Scale: " + groundScale);
				groundFeature = structureInstance.getFeature(groundScale);
				
			}
			else if (groundTerm.getKeywordArg(":VALUE") != null)
			{
				
			}
			else
			{
				KQMLObject groundConcept = groundTerm.get(2);
				if (groundConcept instanceof KQMLList)
				{
					String groundFeatureName = ((KQMLList)groundConcept).get(2).stringValue();
					groundFeature = structureInstance.getFeature(groundFeatureName);
				}
			}
		}
		if (extractedFeature == null || groundFeature == null || operator == null)
			return null;
		System.out.println("Ground Feature: " + groundFeature.getName());
		FeatureConstraint newConstraint = new FeatureConstraint(extractedFeature,
													operator, 
													FeatureConstraint.ComparisonType.VALUE, 
													groundFeature);
		System.out.println("Extracted feature: " + newConstraint);
		return newConstraint;
		
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
}
