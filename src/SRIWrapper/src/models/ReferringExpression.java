package models;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.Block;
import environment.Scene;
import environment.StructureInstance;
import features.BlockFeatureGroup;
import features.ColumnFeature;
import features.FeatureConstants;
import features.FeatureGroup;
import features.UnorderedGroupingFeature;
import features.UnorderedRowFeature;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import utilities.KQMLUtilities;

public class ReferringExpression {

	KQMLList tree;
	KQMLList headTerm;
	List<KQMLList> modifiers;
	List<Predicate> predicates;
	List<Predicate> unaryPredicates;
	List<Predicate> binaryPredicates;
	private StructureInstance pseudoInstance; // Use to attach features
	static String[] definiteHeadTermIndicators = {"ONT::THE", "ONT::THE-SET"};
	static String[] indefiniteHeadTermIndicators = {"ONT::A", "ONT::INDEF-SET"};
	
	public ReferringExpression(String headSymbol, KQMLList tree) {
		this(KQMLUtilities.findTermInKQMLList(headSymbol, tree), tree);
	}
	
	public ReferringExpression(KQMLList headTerm, KQMLList tree) {
		this.tree = tree;
		this.headTerm = headTerm;
		modifiers = new ArrayList<KQMLList>();
		predicates = new ArrayList<Predicate>();
		unaryPredicates = new ArrayList<Predicate>();
		binaryPredicates = new ArrayList<Predicate>();
		pseudoInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		
		findModifiers();
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
				for (String indicator : definiteHeadTermIndicators)
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
				for (String indicator : indefiniteHeadTermIndicators)
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
	
	public static boolean isReferredObject(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") != null)
		{
			String instanceOf = term.getKeywordArg(":INSTANCE-OF").stringValue();
			return instanceOf.equalsIgnoreCase(FeatureConstants.BLOCK) ||
					instanceOf.equalsIgnoreCase(FeatureConstants.COLUMN) ||
					instanceOf.equalsIgnoreCase(FeatureConstants.ROW);
		}
		
		return false;
	}
	
	public static List<ReferringExpression> getReferringExpressions(KQMLList neutralTerm, 
			KQMLList context) 
	{
		List<ReferringExpression> expressions = new ArrayList<ReferringExpression>();
		
		if (isReferredObject(neutralTerm))
		{
			ReferringExpression primaryRef = new ReferringExpression(neutralTerm,context);
			expressions.add(primaryRef);
		}
		// Add back in for multiple predicates
		for (KQMLList headTerm : getDefiniteHeadTerms(context))
		{
			if (isReferredObject(headTerm))
			{
				ReferringExpression newRef = new ReferringExpression(headTerm,context);
				expressions.add(newRef);
			}
		}
		
		for (KQMLList headTerm : getIndefiniteHeadTerms(context))
		{
			if (isReferredObject(headTerm))
			{
				ReferringExpression newRef = new ReferringExpression(headTerm,context);
				expressions.add(newRef);
			}
		}
		
		System.out.println("Found referring expressions: ");
		for (ReferringExpression re : expressions)
			System.out.println(re);
		
		return expressions;
	}
	
	private void findModifiers()
	{
		if (headTerm.getKeywordArg(":MOD") != null)
		{
			String symbol = headTerm.getKeywordArg(":MOD").stringValue();
			KQMLList modTerm = KQMLUtilities.findTermInKQMLList(symbol, tree);
			
			if (modTerm != null)
				modifiers.add(modTerm);
		}
		KQMLObject modList = headTerm.getKeywordArg(":MODS");
		if (modList!= null && modList instanceof KQMLList)
		{
			for (KQMLObject symbol : (KQMLList)(modList))
			{
				KQMLList modTerm = KQMLUtilities.findTermInKQMLList(
												symbol.stringValue(), tree);
				
				if (modTerm != null)
					modifiers.add(modTerm);
			}
		}
		
		for (KQMLList modifier : modifiers)
		{
			String ontType = "";
			String lex = "*";
			if (modifier.getKeywordArg(":INSTANCE-OF") != null)
				ontType = modifier.getKeywordArg(":INSTANCE-OF").stringValue();
			if (modifier.getKeywordArg(":LEX") != null)
				lex = modifier.getKeywordArg(":LEX").stringValue();
			
			PredicateType result = PredicateType.fromString(ontType, lex);
			
			if (result != null)
				predicates.add(new Predicate(result));
		}
	}
	
	public String getInstanceOf()
	{
		if (headTerm.getKeywordArg(":INSTANCE-OF") != null)
			return headTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		return null;
	}
	
	public boolean isRow()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.ROW);
	}
	
	public boolean isColumn()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.COLUMN);
	}
	
	public boolean isBlock()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.BLOCK);
	}
	
	public UnorderedGroupingFeature evaluate(Scene s)
	{
		Set<UnorderedGroupingFeature> matches = new HashSet<UnorderedGroupingFeature>();
		if (isRow())
		{
			matches.addAll(UnorderedRowFeature.rowsFromBlocks(
												s.integerBlockMapping.values()));
		}
		else if (isColumn())
		{
			matches.addAll(ColumnFeature.columnsFromBlocks(
					s.integerBlockMapping.values()));
		}
		else if (isBlock())
		{
			for (Block b : s.integerBlockMapping.values())
			{
				UnorderedGroupingFeature ugf = new UnorderedGroupingFeature("block");
				ugf.add(new BlockFeatureGroup(b));
				matches.add(ugf);
			}
		}
		
		List<UnorderedGroupingFeature> results = filterByUnaryPredicates(matches);
		
		if (results.size() > 0)
		{
			UnorderedGroupingFeature result = results.get(0);
			System.out.println("Blocks in result: " + result.getBlocks().size());
			pseudoInstance.setBlocks(result.getBlocks());
			return results.get(0);
		}
		
		
		return null;
	}
	
	public StructureInstance getPseudoInstance()
	{
		return pseudoInstance;
	}
	
	public String getObjectTypeString()
	{
		if (isRow())
			return "row";
		if (isColumn())
			return "column";
		if (isBlock())
			return "block";
		
		return "structure";
	}
	
	private List<UnorderedGroupingFeature> filterByUnaryPredicates(
								Set<UnorderedGroupingFeature> matches)
	{
		List<UnorderedGroupingFeature> results = 
				new ArrayList<UnorderedGroupingFeature>();
		
		for (UnorderedGroupingFeature element : matches)
		{
			if (element.isEmpty())
				continue;
			
			for (Predicate predicate : predicates)
			{
				if (predicate.evaluate(element))
					results.add(element);
			}
		}
		
		return results;
		
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		//sb.append("the ");
		for (Predicate p : predicates)
			sb.append(p.toString());
		sb.append(" " + getObjectTypeString());
		if (headTerm.getKeywordArg(":SPEC") != null &&
				headTerm.getKeywordArg(":SPEC").stringValue().equals("ONT::THE-SET"))
			sb.append("s");
		return sb.toString();
	}
	

}
