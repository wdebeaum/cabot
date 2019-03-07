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
import features.DistanceFeature;
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
	private ArrayList<String> scales;
	
	public FeatureParser(KQMLList eventTerm, KQMLList context, 
			Map<String, ReferringExpression> referringExpressions,
			ReferringExpression headReferringExpression,
			StructureInstance currentStructureInstance) {
		this.referringExpressions = referringExpressions;
		this.eventTerm = eventTerm;
		this.context = context;
		this.headReferringExpression = headReferringExpression;
		this.currentStructureInstance = currentStructureInstance;
		this.scales = new ArrayList<String>();
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
		if (valueObject != null)
		{
			String valueVariable = valueObject.stringValue();
			KQMLList valueTerm = KQMLUtilities.findTermInKQMLList(valueVariable, context);
			KQMLObject valueTermValueObject = valueTerm.getKeywordArg(":VALUE");
			KQMLObject valueTermNumberObject = valueTerm.getKeywordArg(":NUMBER");
			
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
			else if (valueTermNumberObject != null)
			{
				try
				{
					value = Double.parseDouble(valueTermNumberObject.stringValue());
					return value;
				}
				catch (NumberFormatException nfe)
				{
					
				}			
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
						value = Double.parseDouble(KQMLUtilities.cleanOnt(
									termList.getKeywordArg(":NUMBER").stringValue()));
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
		else if (term.getKeywordArg(":NEUTRAL") != null && term.getKeywordArg(":NEUTRAL1") != null)
		{
			String figureSymbol = term.getKeywordArg(":NEUTRAL").stringValue();
			KQMLList figureTerm = 
					KQMLUtilities.findTermInKQMLList(figureSymbol, context);
			KQMLList neutralOneTerm = 
					KQMLUtilities.findTermInKQMLList(term.getKeywordArg(":NEUTRAL1").stringValue(), context);
			String descriptorFeatureString = figureTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			String neutralOneDescriptorFeatureString = neutralOneTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			if (structureInstance.hasFeature(descriptorFeatureString))
			{
				extractedFeature = structureInstance.getFeature(descriptorFeatureString);
				System.out.println("Feature Scale: " + descriptorFeatureString);
			}
			else if (structureInstance.hasFeature(neutralOneDescriptorFeatureString))
			{
				extractedFeature = structureInstance.getFeature(neutralOneDescriptorFeatureString);
				System.out.println("Feature Scale: " + neutralOneDescriptorFeatureString);
			}
			else if (neutralOneTerm.getKeywordArg(":SIZE") != null)
			{
				extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
				System.out.println("Inferred number subject feature");
			}
			
		}
//		// Possessive scales e.g. "The column's height"
//		else if (term.getKeywordArg(":ASSOC-POS") != null)
//		{
//			String figureSymbol = term.getKeywordArg(":ASSOC-POS").stringValue();
//			KQMLList figureTerm = 
//					KQMLUtilities.findTermInKQMLList(figureSymbol, context);
//		}
		else if (isIndirectNumber(term.getKeywordArg(":INSTANCE-OF").stringValue()))
		{
			extractedFeature = structureInstance.getFeature(FeatureConstants.NUMBER);
			System.out.println("Inferred number subject feature");
		}
		
		// If there's a scale detected somewhere, replace a NUMBER feature with the scale
		if (extractedFeature != null &&
				extractedFeature.getName() == FeatureConstants.NUMBER &&
				!scales.isEmpty())
		{
			for (String replacementScale : scales)
			{
				 if (structureInstance.hasFeature(replacementScale))
				 {
					 System.out.println("Replacing NUMBER with scale: " + replacementScale);
					 extractedFeature = structureInstance.getFeature(replacementScale);
				 }
			}
			
		}
		
		if (headReferringExpression != null)
			System.out.println("Feature refexp: " + headReferringExpression.toString());
			
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
		else if (term.getKeywordArg(":NEUTRAL1") != null) // I think this is better than the below
														// neutral1 option but maybe not
		{
			ground = term.getKeywordArg(":NEUTRAL1").stringValue();
			groundTerm = KQMLUtilities.findTermInKQMLList(ground, context);
			System.out.println("Ground (neutral1): " + ground);
		}
		else if (term.getKeywordArg(":COMPAR") != null)
		{
			ground = term.getKeywordArg(":COMPAR").stringValue();
			groundTerm = KQMLUtilities.findTermInKQMLList(ground, context);
			System.out.println("Ground (compar): " + ground);
		}
		
		if (groundTerm != null)
		{
			// If the feature is for an object
			// e.g. "...taller than the column..."
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
			// If the feature is a scale of an object
			// e.g. "...equal to the height of the column"
			else if (groundTerm.getKeywordArg(":SCALE") != null) 
			{
				String groundScale = groundTerm.getKeywordArg(":SCALE").stringValue();
				
				System.out.println("Ground Scale: " + groundScale);
				
				if (groundTerm.getKeywordArg(":FIGURE") != null)
				{
					String groundObject = groundTerm.getKeywordArg(":FIGURE").stringValue();
					if (referringExpressions.containsKey(groundObject))
					{
						System.out.println("Ground object: " + groundObject);
						groundFeature = referringExpressions.get(groundObject)
								.getPseudoInstance().getFeature(groundScale);
						groundFeature.setSource(referringExpressions.get(groundObject));
						groundFeature.setConstant(false);
					}
				}
				//groundFeature = structureInstance.getFeature(groundScale);
				
			}
			else if (groundTerm.getKeywordArg(":INSTANCE-OF") != null)
			{
				
				String groundScale = groundTerm.getKeywordArg(":INSTANCE-OF").stringValue();
				if (structureInstance.getFeature(groundScale) != null && 
						!groundScale.equalsIgnoreCase(FeatureConstants.NUMBER))
				{
					System.out.println("Ground Scale: " + groundScale);
					groundFeature = structureInstance.getFeature(groundScale);
				}
				
			}

			if (groundTerm.getKeywordArg(":VALUE") != null && 
					groundTerm.getKeywordArg(":INSTANCE-OF").stringValue()
					.equalsIgnoreCase(FeatureConstants.NUMBER))
			{
				
				groundFeature = new CountFeature(FeatureConstants.NUMBER);
				int value = Integer.parseInt(groundTerm.getKeywordArg(":VALUE").stringValue());
				groundFeature.setValue(value);
				groundFeature.setConstant(true);
				System.out.println("Ground value: " + value);
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
					System.out.println("Number constraint: " + value);
					groundFeature.setValue(value);
					groundFeature.setConstant(true);
					return groundFeature;
				}
			}
			else if (neutralTerm.getKeywordArg(":SCALE") != null)
			{
				
			}
		}
		
		if (term.getKeywordArg(":FORMAL") != null)
		{
			
			KQMLList neutralTerm = KQMLUtilities.findTermInKQMLList(
					term.getKeywordArg(":FORMAL").stringValue(), context);
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
			else if (neutralTerm.getKeywordArg(":SCALE") != null)
			{
				
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
			
			if (termAsList.getKeywordArg(":SCALE") != null)
			{
				scales.add(termAsList.getKeywordArg(":SCALE").stringValue());
			}
		}
		
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
				FeatureConstraint fc = null;
				if (headReferringExpression != null)
				{
					fc = extractFeature(
							headReferringExpression.getPseudoInstance(),
							termAsList,context);
				}
				if (fc != null)
				{
					StructureConstraint sc = new StructureConstraint(headReferringExpression,fc);
					constraintsFound.add(sc);
				}
				else // If there is no head expression, just pick one
				{
//					for (ReferringExpression re : referringExpressions.values())
//					{
//						fc = extractFeature(
//								re.getPseudoInstance(),
//								termAsList,context);
//						if (fc != null)
//						{
//							StructureConstraint sc = new StructureConstraint(re,fc);
//							constraintsFound.add(sc);
//							break;
//						}
//					}
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
		boolean inferred = false;
		
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
			
			groundFeature.setInferred();
		}
		
		
		if (extractedFeature != null && operator != null && groundFeature == null)
		{
			try {
				double value = extractDoubleValue(term, context);
				groundFeature = new DistanceFeature(FeatureConstants.NUMBER);
				groundFeature.setValue(value);
				groundFeature.setConstant(true);
				groundFeature.setInferred();
				System.out.println("Inferred number value of " + value);
			} catch (NumberFormatException nfe) {};
//			if (isIndirectNumber(term.getKeywordArg(":INSTANCE-OF").stringValue()))
//			{
//				try {
//					int value = extractIntValue(context);
//					groundFeature = new CountFeature(FeatureConstants.NUMBER);
//					groundFeature.setValue(value);
//					groundFeature.setConstant(true);
//				} catch (NumberFormatException nfe) {};
//			}
		}
		


		// Compare against the rest of the scene
		if (extractedFeature != null && groundFeature == null && operator != null &&
				(operator.equals(Operator.GREATEST) || operator.equals(Operator.LEAST)))
		{
			System.out.println("Calculating greatest or least feature");
			ReferringExpression refExp = extractedFeature.getSource();
			if (refExp == null)
				refExp = headReferringExpression;
			if (refExp == null && !referringExpressions.isEmpty())
			{
				for (ReferringExpression re : referringExpressions.values())
				{
					refExp = re;
					break;
				}
			}
			if (refExp == null)
				return null;
			
			if (operator.equals(Operator.GREATEST))
			{
					
			}
		}
		
		if (extractedFeature == null)
		{
			System.out.println("No extracted feature");
			return null;
		}
		if (groundFeature == null)
		{
			System.out.println("No ground feature");
			return null;
		}
		if (operator == null)
		{
			System.out.println("No operator");
			return null;
		}

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
