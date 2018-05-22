package messages;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.StructureInstance;
import features.CountFeature;
import features.DecimalFeature;
import features.Feature;
import features.FeatureConstants;
import models.Constraint;
import models.FeatureConstraint;
import models.ReferringExpression;
import models.StructureConstraint;
import models.FeatureConstraint.Operator;
import models.PredicateConstraint;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import utilities.KQMLUtilities;

public class FeatureParser {

	// Mapping from term ID to referring expression
	private Map<String,ReferringExpression> referringExpressions;
	private KQMLList eventTerm;
	private KQMLList context;
	private ReferringExpression headReferringExpression;
	private StructureInstance currentStructureInstance;
	
	public FeatureParser(KQMLList eventTerm, KQMLList context, 
			Map<String, ReferringExpression> referringExpressions,
			ReferringExpression headReferringExpression,
			StructureInstance currentStructureInstance) {
		this.referringExpressions = referringExpressions;
		this.eventTerm = eventTerm;
		this.context = context;
		this.headReferringExpression = headReferringExpression;
		this.currentStructureInstance = currentStructureInstance;
	}
	
	public int extractIntValue(KQMLList context) throws NumberFormatException
	{
		
		boolean foundValue = false;
		int value;
				
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			if (termList.getKeywordArg(":INSTANCE-OF").stringValue().contains("NUMBER") && 
					termList.getKeywordArg(":VALUE") != null)
			{
				try
				{
					value = Integer.parseInt(termList.getKeywordArg(":VALUE").stringValue());
					System.out.println("Found int " + value + " in " + term.toString());
					return value;
				}
				catch (NumberFormatException nfe)
				{
					continue;
				}
			}
			else if (termList.getKeywordArg(":NUMBER") != null)
			{
				try
				{
					value = Integer.parseInt(termList.getKeywordArg(":NUMBER").stringValue());
					System.out.println("Found int " + value + " in " + term.toString());
					return value;
				}
				catch (NumberFormatException nfe)
				{
					continue;
				}
			}
		}

		throw new NumberFormatException("No valid number found in context");
		
	}
	
	public static double extractDoubleValue(KQMLList content, KQMLList context) throws NumberFormatException
	{
		
		boolean foundValue = false;
		double value = 0;
		KQMLObject valueObject = content.getKeywordArg(":VALUE");
		String valueVariable = valueObject.stringValue();
		KQMLList valueTerm = KQMLUtilities.findTermInKQMLList(valueVariable, context);
		KQMLObject valueTermValueObject = valueTerm.getKeywordArg(":VALUE");
		
		// TODO: Put some better processing in here 
		if (valueTermValueObject != null)
		{
			try
			{
				value = Double.parseDouble(valueTermValueObject.stringValue());
				return value;
			}
			catch (NumberFormatException nfe)
			{
				
			}
		}
				
		// Then just grab a number otherwise
		if (!foundValue)
		{
			for (KQMLObject term : context)
			{
				KQMLList termList = (KQMLList)term;
				if (termList.getKeywordArg(":INSTANCE-OF").stringValue().contains("NUMBER") && 
						termList.getKeywordArg(":VALUE") != null)
				{
					try
					{
						value = Double.parseDouble(termList.getKeywordArg(":VALUE").stringValue());
						return value;
					}
					catch (NumberFormatException nfe)
					{
						continue;
					}
				}
				else if (termList.getKeywordArg(":NUMBER") != null)
				{
					try
					{
						value = Double.parseDouble(termList.getKeywordArg(":NUMBER").stringValue());
						return value;
					}
					catch (NumberFormatException nfe)
					{
						continue;
					}
				}
			}
		}
		
		throw new NumberFormatException("No valid number found in context");

	}
	
	public Feature extractSubjectFeature(StructureInstance structureInstance, KQMLList term, KQMLList context)
	{
		String scale = "unnamed";
		Feature extractedFeature = null;
		
		
		// Get the scale for naming the feature
		if (term.getKeywordArg(":SCALE") != null)
		{
			scale = term.getKeywordArg(":SCALE").stringValue();
			if (scale.equalsIgnoreCase("ONT::LINEAR-SCALE"))
			{
				scale = term.getKeywordArg(":INSTANCE-OF").stringValue();
			}

			if (structureInstance.hasFeature(scale))
			{
				extractedFeature = structureInstance.getFeature(scale);
				System.out.println("Scale: " + scale);
			}
			
		}
		else if (term.getKeywordArg(":FIGURE") != null)
		{
			String figureSymbol = term.getKeywordArg(":FIGURE").stringValue();
			KQMLList figureTerm = 
					KQMLUtilities.findTermInKQMLList(figureSymbol, context);
			
			String descriptorFeatureString = figureTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			if (structureInstance.hasFeature(descriptorFeatureString))
			{
				extractedFeature = structureInstance.getFeature(descriptorFeatureString);
				System.out.println("Feature Scale: " + descriptorFeatureString);
			}
		}
		else if (isIndirectNumber(term.getKeywordArg(":INSTANCE-OF").stringValue()))
		{
			extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
			System.out.println("Inferred number subject feature");
		}
		
		return extractedFeature;
	}
	
	public Feature extractGroundFeature(StructureInstance structureInstance, KQMLList term, KQMLList context)
	{
		Feature groundFeature = null;
		String ground = null;
		KQMLList groundTerm = null;
		
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return null;
		
		// Get the comparison object or property
		if (term.getKeywordArg(":GROUND") != null)
		{
			ground = term.getKeywordArg(":GROUND").stringValue();
			groundTerm = KQMLUtilities.findTermInKQMLList(ground, context);
			System.out.println("Ground: " + ground);
		}
	
		
		if (groundTerm != null)
		{
			// If the feature is for an object
			if (ReferringExpression.isReferredObject(groundTerm) && 
					term.getKeywordArg(":SCALE") != null)
			{
				String groundScale = term.getKeywordArg(":SCALE").stringValue();
				System.out.println("Ground scale: " + groundScale);
				System.out.println("Ground object: " + groundTerm);
				if (referringExpressions.containsKey(ground))
				{
					
					groundFeature = referringExpressions.get(ground)
							.getPseudoInstance().getFeature(groundScale);
					groundFeature.setSource(referringExpressions.get(ground));
					groundFeature.setConstant(false);
				}
				
			}
			else if (groundTerm.getKeywordArg(":INSTANCE-OF") != null)
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
			if (groundTerm.getKeywordArg(":VALUE") != null && 
					groundTerm.getKeywordArg(":INSTANCE-OF").stringValue()
					.equalsIgnoreCase(FeatureConstants.NUMBER))
			{
				
				groundFeature = new CountFeature(FeatureConstants.NUMBER);
				int value = Integer.parseInt(groundTerm.getKeywordArg(":VALUE").stringValue());
				groundFeature.setValue(value);
				groundFeature.setConstant(true);
			}
		}
		
		if (term.getKeywordArg(":NEUTRAL1") != null)
		{
			
			KQMLList neutralTerm = KQMLUtilities.findTermInKQMLList(
					term.getKeywordArg(":NEUTRAL1").stringValue(), context);
			if (neutralTerm.getKeywordArg(":SIZE") != null)
			{
				KQMLList sizeTerm = KQMLUtilities.findTermInKQMLList(
									neutralTerm.getKeywordArg(":SIZE").stringValue(), context);
				
				if (sizeTerm.getKeywordArg(":VALUE") != null)
				{
					groundFeature = new CountFeature(FeatureConstants.NUMBER);
					int value = Integer.parseInt(sizeTerm.getKeywordArg(":VALUE").stringValue());
					groundFeature.setValue(value);
					groundFeature.setConstant(true);
					return groundFeature;
				}
			}
		}
		
		if (term.getKeywordArg(":SIZE") != null)
		{
			KQMLList sizeTerm = KQMLUtilities.findTermInKQMLList(
								term.getKeywordArg(":SIZE").stringValue(), context);
			groundFeature = new CountFeature(FeatureConstants.NUMBER);
			if (sizeTerm.getKeywordArg(":VALUE") != null)
			{
				int value = Integer.parseInt(sizeTerm.getKeywordArg(":VALUE").stringValue());
				groundFeature.setValue(value);
				groundFeature.setConstant(true);
				return groundFeature;
			}
		}

		return groundFeature;
		
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
	
	public static Operator getOperatorInContext(KQMLList context)
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
				Operator o = FeatureConstraint.operatorFromTRIPS(instance, lex);
				if (o != null)
					return o;
			}
		}
		
		return null;
	}
	
	private static boolean isDefaultComparator(String operatorString)
	{
		return (operatorString.equalsIgnoreCase(FeatureConstants.HORIZONTAL) ||
				operatorString.equalsIgnoreCase(FeatureConstants.VERTICAL) ||
				operatorString.equalsIgnoreCase(FeatureConstants.LINE));
	}
	
	private static boolean isIndirectNumber(String subjectInstanceOf)
	{
		return (subjectInstanceOf.equalsIgnoreCase("ONT::HAVE") ||
				subjectInstanceOf.equalsIgnoreCase("ONT::CONTAINMENT") ||
				subjectInstanceOf.equalsIgnoreCase("ONT::BE"));
	}
	
	public Set<Constraint> extractFeatures()
	{
		Set<Constraint> constraintsFound = new HashSet<Constraint>();
		for (KQMLObject term: context)
		{
			KQMLList termAsList = (KQMLList)term;
			if (referringExpressions.isEmpty())
			{
				FeatureConstraint fc = extractFeature(currentStructureInstance,
													termAsList,context);
				if (fc != null)
				{
					constraintsFound.add(fc);
					
				}
			}
			else // TODO : Get all ref exps, not just first one
			{
				
				FeatureConstraint fc = extractFeature(
							headReferringExpression.getPseudoInstance(),
							termAsList,context);
				if (fc != null)
				{
					StructureConstraint sc = new StructureConstraint(headReferringExpression,fc);
					constraintsFound.add(sc);
					
				}
			}
		}
		return constraintsFound;
	}
	
	public FeatureConstraint extractFeature(StructureInstance structureInstance, KQMLList term, KQMLList context)
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
		
		System.out.println("Checking term: " + term);
		extractedFeature = extractSubjectFeature(structureInstance, term, context);
		
		groundFeature = extractGroundFeature(structureInstance, term, context);
		
		// Trying to get all operators instead of one in the term
		// Get the ONT::MORE-VAL or equivalent
		operatorString = term.getKeywordArg(":INSTANCE-OF").stringValue();
		String lex = "";
		if (term.getKeywordArg(":LEX") != null)
			lex = term.getKeywordArg(":LEX").stringValue();
		
		FeatureConstraint.Operator operator = 
				FeatureConstraint.operatorFromTRIPS(operatorString, lex);
		
		// The instance-of is not actually an operator, but a descriptor
		if (isDefaultComparator(operatorString))
		{
			System.out.println("Invoking default comparator");
			descriptorFeatureString = operatorString;
			if (structureInstance.hasFeature(descriptorFeatureString))
				extractedFeature = structureInstance.
											getFeature(descriptorFeatureString);
			groundFeature = DecimalFeature.getDefaultDecimalMinimum();
		}
		// Number constraints
		else if (operatorString.equalsIgnoreCase(FeatureConstants.NUMBER))
		{
			// If there's a comparator, that should take precedence
			if (isComparatorInContext(context))
				return null;
			else if (term.getKeywordArg(":VALUE") != null)
			{
				System.out.println("No comparators found, choosing default number feature");
				extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
			}
		}
		else if (groundFeature != null && 
				groundFeature instanceof CountFeature && operator == null)
		{
			//extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
			operator = getOperatorInContext(context);
			
			if (operator == null)
			{
				System.out.println("Inferring set size");
				operator = Operator.EQUAL;
			}
		}
		
		// Not currently working
//		if (extractedFeature != null && operator != null && groundFeature == null)
//		{
//			if (isIndirectNumber(term.getKeywordArg(":INSTANCE-OF").stringValue()))
//			{
//				try {
//					int value = extractIntValue(context);
//					groundFeature = new CountFeature(FeatureConstants.NUMBER);
//					groundFeature.setValue(value);
//					groundFeature.setConstant(true);
//				} catch (NumberFormatException nfe) {};
//			}
//		}

		
		if (extractedFeature == null || groundFeature == null || operator == null)
			return null;
		System.out.println("Ground Feature: " + groundFeature.getName());
		if (groundFeature.isConstant())
			System.out.println("Ground value: " + groundFeature.getValue());
		FeatureConstraint newConstraint = new FeatureConstraint(extractedFeature,
													operator, 
													FeatureConstraint.ComparisonType.VALUE, 
													groundFeature);
		System.out.println("Extracted feature: " + newConstraint);
		return newConstraint;
		
	}
}
