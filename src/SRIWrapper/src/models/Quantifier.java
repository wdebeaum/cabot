package models;

import TRIPS.KQML.KQMLList;
import features.FeatureConstants;
import utilities.KQMLUtilities;

public class Quantifier {

	public enum QuantifierType {RESTRICTED, UNIVERSAL, GENERIC};
	private QuantifierType type;
	private int minimumCount;
	private int maximumCount;
	ReferringExpression referringExpression;
	
	public Quantifier(ReferringExpression re)
	{
		this.referringExpression = referringExpression;
		type = QuantifierType.GENERIC;
		minimumCount = 1;
		maximumCount = 100;
	}
	
	public Quantifier()
	{
		type = QuantifierType.GENERIC;
		minimumCount = 1;
		maximumCount = 100;
	}
	
	public static Quantifier extractQuantifier(KQMLList head, KQMLList context)
	{
		Quantifier q = new Quantifier();
		String sizeVariable;
		if (head == null)
			return null;
		
		if (head.getKeywordArg(":SIZE") != null)
		{
			sizeVariable = head.getKeywordArg(":SIZE").stringValue();
		
			if (sizeVariable.equalsIgnoreCase(FeatureConstants.UNIVERSAL))
				q.type = QuantifierType.UNIVERSAL;
			else if (sizeVariable.equalsIgnoreCase(FeatureConstants.SOME))
			{
				
			}
			else
			{
			
				KQMLList sizeTerm = KQMLUtilities.findTermInKQMLList(sizeVariable, context);
				if (sizeTerm == null)
				{
					System.out.println("Quantifier min: " + q.minimumCount);
					System.out.println("Quantifier max: " + q.maximumCount);
					System.out.println("Quantifier type: " + q.getType());
					return q;
				}
				if (sizeTerm.getKeywordArg(":VALUE") != null)
				{
					int value = Integer.parseInt(sizeTerm.getKeywordArg(":VALUE").stringValue());
					q.minimumCount = value;
					q.maximumCount = value;
				}
				
				if (sizeTerm.getKeywordArg(":MIN") != null)
				{
					q.minimumCount = Integer.parseInt(KQMLUtilities.cleanOnt(sizeTerm.getKeywordArg(":MIN").stringValue()));
				}
				
				if (sizeTerm.getKeywordArg(":MAX") != null)
				{
					q.maximumCount = Integer.parseInt(KQMLUtilities.cleanOnt(sizeTerm.getKeywordArg(":MAX").stringValue()));
				}
				
				KQMLList modTerm = null;
				if (sizeTerm.getKeywordArg(":MODS") != null)
				{
					if (((KQMLList)sizeTerm.getKeywordArg(":MODS")).size() > 0)
					{
						String modVariable = ((KQMLList)sizeTerm.getKeywordArg(":MODS")).get(0).stringValue();
						modTerm = KQMLUtilities.findTermInKQMLList(modVariable, context);
						
					}
				}
				else if (sizeTerm.getKeywordArg(":MOD") != null)
				{
	
					String modVariable = sizeTerm.getKeywordArg(":MOD").stringValue();
					modTerm = KQMLUtilities.findTermInKQMLList(modVariable, context);
				}
			
			
				if (modTerm != null && modTerm.getKeywordArg(":GROUND") != null)
				{
					String groundVariable = modTerm.getKeywordArg(":GROUND").stringValue();
					KQMLList groundTerm = KQMLUtilities.findTermInKQMLList(groundVariable, context);
					
					if (groundTerm.getKeywordArg(":VALUE") == null && groundTerm.getKeywordArg(":NUMBER") == null)
						return q;
					
					int value = Integer.parseInt(KQMLUtilities.cleanOnt(groundTerm.getKeywordArg(":VALUE").stringValue()));
					String lex = modTerm.getKeywordArg(":LEX").stringValue();
					switch (lex)
					{
					case FeatureConstants.MIN:
						q.minimumCount = value;
						break;
					case FeatureConstants.MAX:
						q.maximumCount = value;
						break;
					case FeatureConstants.MORETHAN:
						q.minimumCount = value + 1;
						break;
					case FeatureConstants.LESSTHAN:
						q.maximumCount = value - 1;
						break;
					}
				}
			}
				
		}
		//if (head.getKeywordArg(":QUAN") != null)
		
		// "The columns" - no specific number, therefore likely to be a universal constraint
		if (head.getKeywordArg(":SPEC") != null && 
				head.getKeywordArg(":SPEC").stringValue().equalsIgnoreCase("ONT::THE-SET") && 
				q.maximumCount == 100 && q.minimumCount == 1)
			q.type = QuantifierType.UNIVERSAL;
		
		System.out.println("Quantifier min: " + q.minimumCount);
		System.out.println("Quantifier max: " + q.maximumCount);
		System.out.println("Quantifier type: " + q.getType());
		
		return q;
	}
	
	boolean validSatisfactionCount(int satisfactions, int totalInstances)
	{
		System.out.println("Satisfactions: " + satisfactions);
		System.out.println("Total instances: " + totalInstances);
		System.out.println("Maximum count: " + maximumCount);
		System.out.println("Minimum count: " + minimumCount);
		if (satisfactions > maximumCount || satisfactions < minimumCount)
			return false;
		
		if (type == QuantifierType.UNIVERSAL && (satisfactions != totalInstances))
			return false;
		
		return true;
	}
	
	public QuantifierType getType()
	{
		return type;
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("Quantifier: " + type.toString());
		sb.append("\nMaximum count: " + maximumCount);
		sb.append("\nMinimum count: " + minimumCount);
		return sb.toString();
	}

	public int getMinimumCount() {
		return minimumCount;
	}

	public int getMaximumCount() {
		return maximumCount;
	}
}
