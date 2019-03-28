package models;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;
import features.FeatureConstants;
import utilities.KQMLUtilities;

public class ReferringExpressionParser {

	private HashMap<String,ReferringExpression> referringExpressions;
	private static String lastObjectType;
	private KQMLList eventTerm;
	private KQMLList context;
	private ReferringExpression headReferringExpression;
	
	public ReferringExpressionParser(KQMLList eventTerm, KQMLList context) {
		referringExpressions = new HashMap<String, ReferringExpression>();
		this.eventTerm = eventTerm;
		this.context = context;
		headReferringExpression = null;
		lastObjectType = null;
	}

	
	public Map<String, ReferringExpression> extractReferringExpressions()
	{
		String subjectVariable = null;
		if (eventTerm.getKeywordArg(":NEUTRAL") != null)
		{
			KQMLList subjectTerm = KQMLUtilities.findTermInKQMLList(
					eventTerm.getKeywordArg(":NEUTRAL").stringValue(), context);
			// This only picks a head referring expression if it's directly mentioning
			// an object in the scene
			if (ReferringExpression.isReferredObject(subjectTerm) )
			{
				subjectVariable = subjectTerm.get(KQMLUtilities.VARIABLE).stringValue();
				ReferringExpression subject = new ReferringExpression(subjectTerm,context);
				headReferringExpression = subject;
				referringExpressions.put(subjectVariable,subject);
				
				// Don't store underspecified referents
				if (!ReferringExpression.isUnderspecifiedReferredObject(subjectTerm))
					lastObjectType = subject.getObjectTypeString();
			}
				 
			else if (ReferringExpression.modifiesReferredObject(subjectTerm, context))
			{
				subjectVariable = subjectTerm.get(KQMLUtilities.VARIABLE).stringValue();
				ReferringExpression subject = new ReferringExpression(subjectTerm,context);

					
				referringExpressions.put(subjectVariable.trim(),subject);
				
				// Don't store underspecified referents
				if (!ReferringExpression.isUnderspecifiedReferredObject(subjectTerm))
					lastObjectType = subject.getObjectTypeString();
			}
		}
//			if (isReferredObject(neutralTerm))
//			{
//				ReferringExpression primaryRef = new ReferringExpression(neutralTerm,context);
//				expressions.add(primaryRef);
//			}
		// Add back in for multiple predicates
		for (KQMLList headTerm : getDefiniteHeadTerms(context))
		{
			if (ReferringExpression.isReferredObject(headTerm) && 
					!headTerm.get(KQMLUtilities.VARIABLE).stringValue()
					.equalsIgnoreCase(subjectVariable))
			{
				String variable = headTerm.get(KQMLUtilities.VARIABLE).stringValue();
				ReferringExpression newRef = new ReferringExpression(headTerm,context);
				referringExpressions.put(variable.trim(), newRef);
				if (headReferringExpression == null)
					headReferringExpression = newRef;
			}
		}
		
		for (KQMLList headTerm : getIndefiniteHeadTerms(context))
		{
			if (ReferringExpression.isReferredObject(headTerm)  && 
					!headTerm.get(KQMLUtilities.VARIABLE).stringValue()
					.equalsIgnoreCase(subjectVariable))
			{
				String variable = headTerm.get(KQMLUtilities.VARIABLE).stringValue();
				ReferringExpression newRef = new ReferringExpression(headTerm,context);
				referringExpressions.put(variable.trim(), newRef);
				
				// Remove refsets as the indef-set will then have the important info
				if (headTerm.getKeywordArg(":REFSET") != null)
				{
					String refset = headTerm.getKeywordArg(":REFSET").stringValue();
					referringExpressions.remove(refset.trim());
				}
			}
		}
		/*
		// Remove "structure" if there is a legit referring expression
		if (referringExpressions.size() > 1 )
		{
			boolean resetHeadReferringExpression = false;
			for (Entry<String,ReferringExpression> re : referringExpressions.entrySet())
			{
				System.out.println("RefExp: |" + re.getValue().toString().toLowerCase() + "|");
				if (re.toString().toLowerCase().contains("structure"))
				{
					System.out.println("RE: " + re.getValue());
					System.out.println("Head RE: " + headReferringExpression);
					if (referringExpressions.get(re.getValue().getVariableName()) == headReferringExpression)
						resetHeadReferringExpression = true;
					referringExpressions.remove(re.getKey());
				}
				
				
			}
			if (resetHeadReferringExpression)
			{
				headReferringExpression = null;
				for (ReferringExpression re : referringExpressions.values())
				{
					// Prioritize definite referring expressions for the head term
					if (re.isDefinite())
					{
						headReferringExpression = re;
						break;
					}
				}
				if (headReferringExpression == null)
				{
					for (ReferringExpression re : referringExpressions.values())
					{
							headReferringExpression = re;
							break;
					}
				}
			}
		}*/
		
		System.out.println("Found referring expressions: ");
		for (ReferringExpression re : referringExpressions.values())
			System.out.println(re);
		
//		if (referringExpressions.size() > 0 && headReferringExpression == null)
//		{
//			for (ReferringExpression re : referringExpressions.values())
//			{
//				headReferringExpression = re;
//				break;
//			}
//		}
		
		resolveAnaphora();

		return referringExpressions;
	}
	
	/**
	 * Go through referring expressions and fill in any referring expressions that are 
	 * underspecified, like "the other ones" or "these". If it's an "other" referring 
	 * expression, it will use a contrast set during evaluation.
	 */
	public void resolveAnaphora()
	{
		String inferredObjectType = FeatureConstants.BLOCK;
		
		// First try to find unresolved referring expressions in the current assertion
		HashSet<ReferringExpression> unresolved = new HashSet<ReferringExpression>();
		for (ReferringExpression re : referringExpressions.values())
		{
			if (re.isUnderspecified())
				unresolved.add(re);
		}
		
		if (unresolved.isEmpty())
			return;
		
		ReferringExpression referent = null;
		// Then find appropriate object type
		for (ReferringExpression re : referringExpressions.values())
		{
			if (!re.isUnderspecified()) {
				inferredObjectType = re.getInstanceOf();
				referent = re;
			}

		}
		
		if (referent != null)
		{
			for (ReferringExpression ure : unresolved)
			{
				boolean contrasted = ure.isContrasted();
				ure.headTerm = KQMLUtilities.setKeywordArgTo(ure.headTerm, 
										":INSTANCE-OF", 
										new KQMLToken(referent.getInstanceOf()));
				System.out.println("Inferred refexp as " + referent.getObjectTypeString());
				if (contrasted)
					ure.setContrastSet(referent);
				
			}
			return;
		}
		
		// Try the last mentioned object
		if (lastObjectType != null)
		{
			inferredObjectType = lastObjectType;
			for (ReferringExpression ure : unresolved)
			{
				ure.headTerm = KQMLUtilities.setKeywordArgTo(ure.headTerm, 
						":INSTANCE-OF", 
						new KQMLToken(inferredObjectType));
				System.out.println("Inferred refexp as " + inferredObjectType);
			referringExpressions.put(ure.getVariableName(), ure);
			}
			return;
		}
		
	}
	
	public static List<String> getDefiniteHeadTermSymbols(KQMLList tree)
	{
		List<KQMLList> headTerms = getDefiniteHeadTerms(tree);
		List<String> headTermSymbols = new ArrayList<String>();
		for (KQMLList headTerm : headTerms)
			headTermSymbols.add(headTerm.get(KQMLUtilities.VARIABLE).stringValue());
		return headTermSymbols;
	}
	
	public static List<KQMLList> getDefiniteHeadTerms(KQMLList tree)
	{
		List<KQMLList> toReturn = new ArrayList<KQMLList>();
		for (KQMLObject term : tree)
		{
			if (term instanceof KQMLList)
			{
				KQMLList termList = (KQMLList)term;
				for (String indicator : ReferringExpression.definiteHeadTermIndicators)
				{
					if (termList.get(KQMLUtilities.ONT_REF).stringValue()
							.equalsIgnoreCase(indicator) || 
						(termList.getKeywordArg(":SPEC") != null &&
						 termList.getKeywordArg(":SPEC").stringValue()
						 .equalsIgnoreCase(indicator)))
					{
						toReturn.add(termList);
					}
				}
			}
		}
		return toReturn;
	}
	
	public static List<KQMLList> getIndefiniteHeadTerms(KQMLList tree)
	{
		List<KQMLList> toReturn = new ArrayList<KQMLList>();
		for (KQMLObject term : tree)
		{
			if (term instanceof KQMLList)
			{
				KQMLList termList = (KQMLList)term;
				for (String indicator : ReferringExpression.indefiniteHeadTermIndicators)
				{
					if (termList.get(KQMLUtilities.ONT_REF).stringValue()
							.equalsIgnoreCase(indicator) || 
						(termList.getKeywordArg(":SPEC") != null &&
						 termList.getKeywordArg(":SPEC").stringValue()
						 .equalsIgnoreCase(indicator)))
					{
						toReturn.add(termList);
					}
				}
			}
		}
		return toReturn;
	}
	
	public ReferringExpression getHeadReferringExpression()
	{
		return headReferringExpression;
	}

}
