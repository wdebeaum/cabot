package models;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jblas.DoubleMatrix;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.Block;
import environment.Scene;
import environment.Space;
import environment.StructureInstance;
import features.BlockFeatureGroup;
import features.ColumnFeature;
import features.Feature;
import features.FeatureConstants;
import features.FeatureGroup;
import features.UnorderedGroupingFeature;
import features.UnorderedRowFeature;
import geometry.AxisAlignedBoundingBox;
import goals.GoalStateHandler;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import utilities.KQMLUtilities;

public class ReferringExpression {

	KQMLList tree;
	KQMLList headTerm;
	List<KQMLList> modifiers;
	List<String> subSelections;
	List<Predicate> predicates;
	List<Predicate> unaryPredicates;
	List<Predicate> binaryPredicates;
	
	UnorderedGroupingFeature ground;
	private StructureInstance pseudoInstance; // Use to attach features
	private StructureInstance inverseInstance;
	UnorderedGroupingFeature inverseGroupingFeature;
	public static String[] definiteHeadTermIndicators = {"ONT::THE", "ONT::THE-SET"};
	public static String[] indefiniteHeadTermIndicators = {"ONT::A", "ONT::INDEF-SET"};
	static String lastReferredObjectType = null;
	boolean restricted = false;
	private ReferringExpression contrastSet;
	
	
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
		subSelections = new ArrayList<String>();
		pseudoInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		inverseInstance = new StructureInstance("inverse", new ArrayList<Block>());
		if (isReferredObject(headTerm))
			lastReferredObjectType = headTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		contrastSet = null;
		ground = null;
		inverseGroupingFeature = null;
		findModifiers();
		findLocation();
		findSubSelection();
	}
	
	public ReferringExpression(Predicate predicate, String quantifier, String instanceOf)
	{
		this(new KQMLList(), new KQMLList());
		
		headTerm.add(quantifier);
		headTerm.add("R" + String.format("%05d", GoalStateHandler.getNextId()));
		headTerm.add(":INSTANCE-OF");
		headTerm.add(instanceOf);
		
		predicates.add(predicate);
		unaryPredicates.add(predicate);
		
		
	}
	
	public boolean isPlural()
	{
		if (headTerm.size() < 1)
			return false;
		if (headTerm.get(0).stringValue().contains("-SET"))
			return true;
		return false;
		
	}
	

	
	public static boolean isReferredObject(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return false;

		String instanceOf = term.getKeywordArg(":INSTANCE-OF").stringValue();
		if (instanceOf.equalsIgnoreCase(FeatureConstants.BLOCK) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.COLUMN) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.ROW) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.GAP) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.SPACE) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.OTHER) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.REF_SEM) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.SET))
			return true;
		
//		// For subselections, like "ends of the row"
//		if (term.getKeywordArg(":FIGURE") != null)
//		{
//			String figureVariable = term.getKeywordArg(":FIGURE").stringValue();
//			KQMLList figure = KQMLUtilities.findTermInKQMLList(figureVariable, context);
//			if (figure != null)
//				return isReferredObject(figure,context);
//		}
		
		return false;
	}
	
	public static boolean isUnderspecifiedReferredObject(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return false;

		String instanceOf = term.getKeywordArg(":INSTANCE-OF").stringValue();
		if (instanceOf.equalsIgnoreCase(FeatureConstants.OTHER) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.REF_SEM) ||
				instanceOf.equalsIgnoreCase(FeatureConstants.SET))
			return true;

		
		return false;
	}
	
	public boolean isUnderspecified()
	{
		return isContrasted() || isUnderspecifiedReferredObject(headTerm);
	}
	
	public boolean isContrasted()
	{
		for (KQMLList modifier : modifiers)
		{
			if (modifier.getKeywordArg(":INSTANCE-OF")
					.stringValue().equalsIgnoreCase(FeatureConstants.OTHER))
				return true;
		}
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.OTHER);
	}

	
	private PredicateType findLocation()
	{
		if (headTerm.getKeywordArg(":LOCATION") != null)
		{
			String locationVariable = headTerm.getKeywordArg(":LOCATION").stringValue();
			KQMLList locationTerm = KQMLUtilities.findTermInKQMLList(locationVariable, tree);
			String ontType = "";
			String lex = "";
			if (locationTerm.getKeywordArg(":INSTANCE-OF") != null)
				ontType = locationTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			if (locationTerm.getKeywordArg(":LEX") != null)
				lex = locationTerm.getKeywordArg(":LEX").stringValue();
			
			PredicateType result = PredicateType.fromString(ontType, lex);
			if (result != null)
				predicates.add(new Predicate(result));
			return result;
		}
		
		return null;
	}
	
	private void findSubSelection()
	{
		for (KQMLObject term : tree)
		{
			KQMLList termList = (KQMLList)term;
			String ontType = "";
			
			if (termList.getKeywordArg(":INSTANCE-OF") != null)
				ontType = termList.getKeywordArg(":INSTANCE-OF").stringValue();
			
			if (termList.getKeywordArg(":ASSOC-POS") != null)
			{
				if (termList.getKeywordArg(":ASSOC-POS").stringValue()
						.equalsIgnoreCase(getVariableName()))
				{
					subSelections.add(ontType);
				}
			}
			
			if (termList.getKeywordArg(":FIGURE") != null)
			{
				if (termList.getKeywordArg(":FIGURE").stringValue()
						.equalsIgnoreCase(getVariableName()))
				{
					subSelections.add(ontType);
				}
			}
		}
	}
	
	private void findModifiers()
	{
		if (headTerm.getKeywordArg(":QUAN") != null &&
				headTerm.getKeywordArg(":QUAN").stringValue()
					.equalsIgnoreCase("ONT::ONLY"))
			restricted = true;
		
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
	
	public boolean isSpace()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.SPACE) || 
				getInstanceOf().equalsIgnoreCase(FeatureConstants.GAP) ;
	}
	
	public UnorderedGroupingFeature evaluate(Scene s)
	{
		Set<UnorderedGroupingFeature> matches = new HashSet<UnorderedGroupingFeature>();
		
		if (contrastSet != null)
		{
			// Avoid referent loops
			if (contrastSet.getContrastSet() == this)
			{
				System.out.println("Self referential reference loop");
				return null;
			}
				
			contrastSet.evaluate(s);
			return contrastSet.inverseGroupingFeature;
			
		}
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
		// TODO: Add spaces at ground level between blocks
		else if (isSpace())
		{
			for (ColumnFeature column:
					ColumnFeature.columnsFromBlocks(s.integerBlockMapping.values()))
			{
				AxisAlignedBoundingBox aabb = 
						AxisAlignedBoundingBox.fromBlockFeatureGroups(column.getBlockFeatureGroups());
				double newZ = aabb.maxZ + (Block.BLOCK_WIDTH / 2);
				DoubleMatrix newSpaceCenter = aabb.getCenter();
				newSpaceCenter.put(2, newZ);
				UnorderedGroupingFeature ugf = new UnorderedGroupingFeature("space");
				ugf.add(new BlockFeatureGroup(new Space(newSpaceCenter)));
				matches.add(ugf);
				
			}
		}
		
		
		List<UnorderedGroupingFeature> results = filterByUnaryPredicates(matches);
		// Start generating the inverse set (objects which don't match)
		if (isPlural())
			matches.removeAll(results);
		else if (results.size() > 0)
			matches.remove(results.get(0));
		
		inverseGroupingFeature = new UnorderedGroupingFeature("inverse");
		for (UnorderedGroupingFeature inverseMatch : matches)
		{
			inverseGroupingFeature.add(inverseMatch);
		}
		inverseInstance.setBlocks(inverseGroupingFeature.getBlocks());

		if (results.size() > 0)
		{
			if (isPlural())
			{
				UnorderedGroupingFeature combinedResult = new UnorderedGroupingFeature("result");
				for (UnorderedGroupingFeature result : results)
				{
					combinedResult.add(result);
				}
				pseudoInstance.setBlocks(combinedResult.getBlocks());
				return combinedResult;
			}
			else
			{
				UnorderedGroupingFeature result = results.get(0);
				System.out.println("Blocks in result: " + result.getBlocks().size());
				pseudoInstance.setBlocks(result.getBlocks());
				return results.get(0);
			}
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
		if (isSpace())
			return "space";
		
		
		return "structure";
	}
	
	
	private List<UnorderedGroupingFeature> filterByUnaryPredicates(
								Set<UnorderedGroupingFeature> matches)
	{
		List<UnorderedGroupingFeature> results = 
				new ArrayList<UnorderedGroupingFeature>();
		
		if (predicates.isEmpty())
		{
			for (UnorderedGroupingFeature element: matches)
			{
				results.add(getSubselection(element));
			}
			
			return results;
		}
		
		for (UnorderedGroupingFeature element : matches)
		{
			if (element.isEmpty())
				continue;
			
			for (Predicate predicate : predicates)
			{
				if (predicate.evaluate(element))
					results.add(getSubselection(element));
			}
		}
		
		return results;
		
	}
	
	private UnorderedGroupingFeature getSubselection(UnorderedGroupingFeature ugf)
	{
		for (String selection : subSelections)
		{
			if (ugf.getFeatures().get(selection) != null)
			{
				Feature f = ugf.getFeatures().get(selection);
				if (f instanceof UnorderedGroupingFeature)
					return (UnorderedGroupingFeature)f;
			}
		}
		return ugf;
	}
	
	@Override
	public boolean equals(Object other)
	{
		if (!(other instanceof ReferringExpression))
			return false;
		
		ReferringExpression refExpOther = (ReferringExpression)other;
		
		return refExpOther.toString().equalsIgnoreCase(toString());
	}
	
	@Override
	public int hashCode()
	{
		return toString().hashCode();
	}
	
	public String getVariableName()
	{
		return headTerm.get(KQMLUtilities.VARIABLE).stringValue();
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		//sb.append("the ");
		if (contrastSet != null)
			sb.append(" other ");
		
		for (Predicate p : predicates)
			sb.append(p.toString());

		sb.append(" " + getObjectTypeString());
		if ((headTerm.getKeywordArg(":SPEC") != null &&
				headTerm.getKeywordArg(":SPEC").stringValue().equals("ONT::THE-SET")) ||
				isPlural())
			sb.append("s");
		return sb.toString();
	}

	public boolean isRestricted() {
		return restricted;
	}

	public void setRestricted(boolean restricted) {
		this.restricted = restricted;
	}
	
	public static String getLastReferredObjectType()
	{
		return new String(lastReferredObjectType);
	}
	
	public void setContrastSet(ReferringExpression re)
	{
		this.contrastSet = re;
	}
	
	public ReferringExpression getContrastSet()
	{
		return contrastSet;
	}

}
