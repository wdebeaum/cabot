package models;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
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
import features.TemporalSequenceFeature;
import features.UnorderedGroupingFeature;
import features.UnorderedRowFeature;
import geometry.AxisAlignedBoundingBox;
import goals.GoalStateHandler;
import messages.LanguageGeneration;
import models.comparators.ValueComparator;
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
	List<UnorderedGroupingFeature> subselectionInverse;
	public static String[] definiteHeadTermIndicators = {"ONT::THE", "ONT::THE-SET", "ONT::QUANTIFIER", "ONT::BARE"};
	public static String[] indefiniteHeadTermIndicators = {"ONT::A", "ONT::INDEF-SET"};
	static String lastReferredObjectType = null;
	boolean restricted = false;
	boolean universal = false;
	boolean ordinal = false;
	private ReferringExpression contrastSet;
	private Quantifier quantifier;
	
	
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
		
		quantifier = Quantifier.extractQuantifier(headTerm, tree);
		contrastSet = null;
		ground = null;
		inverseGroupingFeature = null;
		subselectionInverse = null;
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
	
	public ReferringExpression(ReferringExpression toCopy)
	{
		this(toCopy.headTerm,toCopy.tree);
	}
	
	public boolean isPlural()
	{
		if (headTerm.size() < 1)
			return false;
		if (headTerm.get(0).stringValue().contains("-SET"))
			return true;
		if (headTerm.get(0).stringValue().equalsIgnoreCase("ONT::QUANTIFIER"))
			return true;
		if (getInstanceOf().equals("ONT::SET"))
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
	
	public static boolean modifiesReferredObject(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":ASSOC-POS") != null)
		{
			String posTerm = term.getKeywordArg(":ASSOC-POS").stringValue();
			if (isReferredObject(KQMLUtilities.findTermInKQMLList(posTerm, context)))
				return true;
		}
		if (term.getKeywordArg(":FIGURE") != null)
		{
			String figTerm = term.getKeywordArg(":FIGURE").stringValue();
			if (isReferredObject(KQMLUtilities.findTermInKQMLList(figTerm, context)))
				return true;			
		}
		
		return false;
	}
	
	public static String getReferredObject(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":ASSOC-POS") != null)
		{
			String posTerm = term.getKeywordArg(":ASSOC-POS").stringValue();
			if (isReferredObject(KQMLUtilities.findTermInKQMLList(posTerm, context)))
				return posTerm;
		}
		if (term.getKeywordArg(":FIGURE") != null)
		{
			String figTerm = term.getKeywordArg(":FIGURE").stringValue();
			if (isReferredObject(KQMLUtilities.findTermInKQMLList(figTerm, context)))
				return figTerm;			
		}
		
		return null;
	}
	
	public static boolean isUnderspecifiedReferredObject(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return false;
		
		if (term.getKeywordArg(":ELEMENT-TYPE") != null)
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
			if (result != null && !PredicateType.isBinary(result))
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
					// Don't grab scales like "height of the column"
					if (termList.getKeywordArg(":SCALE") == null)
						subSelections.add(ontType);
				}
			}
			
			if (termList.getKeywordArg(":ORDINAL") != null)
			{
				System.out.println("Found ordinal subselection");
				subSelections.add(termList.getKeywordArg(":ORDINAL").stringValue());
				ordinal = true;
			}
		}
		
		KQMLObject modList = headTerm.getKeywordArg(":MODS");
		if (modList != null && modList instanceof KQMLList)
		{
			for (KQMLObject symbol : (KQMLList)(modList))
			{
				KQMLList modTerm = KQMLUtilities.findTermInKQMLList(
												symbol.stringValue(), tree);
				
				if (modTerm != null && modTerm.getKeywordArg(":INSTANCE-OF") != null)
				{
					if (modTerm.getKeywordArg(":INSTANCE-OF").stringValue()
							.equalsIgnoreCase(FeatureConstants.LAST))
						subSelections.add(modTerm.getKeywordArg(":INSTANCE-OF").stringValue());
				}
			}
		}
	}
	
	private void findModifiers()
	{
		if (headTerm.getKeywordArg(":QUAN") != null)
		{
			if (headTerm.getKeywordArg(":QUAN").stringValue()
					.equalsIgnoreCase("ONT::ONLY"))
				restricted = true;
			else if (headTerm.getKeywordArg(":QUAN").stringValue()
					.equalsIgnoreCase("ONT::EVERY"))
				universal = true;
		}
		
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
	
	public Feature getMaxFeatureValue(String featureName, Scene s)
	{
		UnorderedGroupingFeature result = evaluate(s);
		Feature maxFeature = null;
		
		if (isPlural())
		{
			for (FeatureGroup element : result.getValue())
			{
				if (element instanceof UnorderedGroupingFeature && 
						element.getFeatures().containsKey(featureName))
				{
					Feature currentFeature = element.getFeatures().get(featureName);
					if (maxFeature == null)
						maxFeature = currentFeature;
					Comparator comparator = new ValueComparator();
					if (comparator.compare(currentFeature, maxFeature) >= 0)
						maxFeature = currentFeature;
				}
			}
			
			return maxFeature;
		}
		else
		{
			evaluate(s);
			return pseudoInstance.getFeature(featureName);
		}
	}
	
	public Feature getMinFeatureValue(String featureName, Scene s)
	{
		UnorderedGroupingFeature result = evaluate(s);
		Feature minFeature = null;
		
		if (isPlural())
		{
			for (FeatureGroup element : result.getValue())
			{
				if (element instanceof UnorderedGroupingFeature && 
						element.getFeatures().containsKey(featureName))
				{
					Feature currentFeature = element.getFeatures().get(featureName);
					if (minFeature == null)
						minFeature = currentFeature;
					Comparator comparator = new ValueComparator();
					if (comparator.compare(currentFeature, minFeature) <= 0)
						minFeature = currentFeature;
				}
			}
			
			return minFeature;
		}
		else
		{
			evaluate(s);
			return pseudoInstance.getFeature(featureName);
		}
	}
	
	
	
	public String getInstanceOf()
	{
		if (headTerm.getKeywordArg(":INSTANCE-OF") != null)
			return headTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		return null;
	}
	
	public String getElementType()
	{
		if (headTerm.getKeywordArg(":ELEMENT-TYPE") != null)
			return headTerm.getKeywordArg(":ELEMENT-TYPE").stringValue();
		else
			return getInstanceOf();
	}
	
	public boolean isRow()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.ROW) || 
				getElementType().equalsIgnoreCase(FeatureConstants.ROW);
	}
	
	public boolean isColumn()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.COLUMN) || 
				getElementType().equalsIgnoreCase(FeatureConstants.COLUMN);
	}
	
	public boolean isBlock()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.BLOCK) || 
				getElementType().equalsIgnoreCase(FeatureConstants.BLOCK);
	}
	
	public boolean isSpace()
	{
		return getInstanceOf().equalsIgnoreCase(FeatureConstants.SPACE) || 
				getInstanceOf().equalsIgnoreCase(FeatureConstants.GAP) ;
	}
	
	public Quantifier getQuantifier()
	{
		return quantifier;
	}
	
	public UnorderedGroupingFeature evaluate(Scene s)
	{
		System.out.println("Scene has " + s.integerBlockMapping.size() + " blocks.");
		System.out.println("Evaluating " + toString());
		if (pseudoInstance != null)
			System.out.println("Pseudoinstance has " + pseudoInstance.blocks.size() + " blocks.");
		Set<UnorderedGroupingFeature> objectTypeMatches = new HashSet<UnorderedGroupingFeature>();
		
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
			// If we will refer to this using ordinals, put it into a temporal sequence
			// feature to be chosen with subselection
			if (ordinal)
			{
				objectTypeMatches.add(TemporalSequenceFeature.temporalSequenceVerticalFromBlocks(
						s.integerBlockMapping.values()));
			}
			else
				objectTypeMatches.addAll(UnorderedRowFeature.rowsFromBlocks(
												s.integerBlockMapping.values()));
		}
		else if (isColumn())
		{
			// If we will refer to this using ordinals, put it into a temporal sequence
			// feature to be chosen with subselection
			if (ordinal)
			{
				TemporalSequenceFeature result = TemporalSequenceFeature.temporalSequenceFromBlocks(
						s.integerBlockMapping.values());
				int i = 1;
				for (Object columnObject : result.getValue())
				{
					UnorderedGroupingFeature column = (UnorderedGroupingFeature)columnObject;
					System.out.println("Column " + i + " has " + column.getBlocks().size() + " blocks");
					i++;
				}
				for (Entry<String,Feature> entry : result.getFeatures().entrySet())
				{
					System.out.println("Feature: " + entry.getKey());
					System.out.println("Value: " + entry.getValue());
				}
				objectTypeMatches.add(TemporalSequenceFeature.temporalSequenceFromBlocks(
												s.integerBlockMapping.values()));
				
			}
			else
				objectTypeMatches.addAll(ColumnFeature.columnsFromBlocks(
					s.integerBlockMapping.values()));
		}
		else if (isBlock())
		{
			for (Block b : s.integerBlockMapping.values())
			{
				UnorderedGroupingFeature ugf = new UnorderedGroupingFeature("block");
				ugf.add(new BlockFeatureGroup(b));
				objectTypeMatches.add(ugf);
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
				objectTypeMatches.add(ugf);
				
			}
		}
		
		
		List<UnorderedGroupingFeature> results = filterByUnaryPredicates(objectTypeMatches,s);
		// Start generating the inverse set (objects which don't match)
		if (isPlural() || universal)
			objectTypeMatches.removeAll(results);
		else if (!ordinal && results.size() > 0)
			objectTypeMatches.remove(results.get(0));
		else if (ordinal && !objectTypeMatches.isEmpty())
		{
			UnorderedGroupingFeature originalTemporalResult = null;
			for (UnorderedGroupingFeature originalResult : objectTypeMatches)
				originalTemporalResult = originalResult;
			objectTypeMatches.clear();
			for (FeatureGroup fg: originalTemporalResult.getValue())
			{
				if (!results.contains(fg) && fg instanceof UnorderedGroupingFeature)
					objectTypeMatches.add((UnorderedGroupingFeature)fg);
			}
		}
		
		inverseGroupingFeature = new UnorderedGroupingFeature("inverse");
		

		for (UnorderedGroupingFeature inverseMatch : objectTypeMatches)
		{
			inverseGroupingFeature.add(inverseMatch);
		}

		inverseInstance.setBlocks(inverseGroupingFeature.getBlocks());
		

		if (results.size() > 0)
		{
			if (isPlural() || universal)
			{
				UnorderedGroupingFeature combinedResult = new UnorderedGroupingFeature("result");
				for (UnorderedGroupingFeature result : results)
				{
					combinedResult.add(result);
				}
				pseudoInstance.setBlocks(combinedResult.getBlocks());
				System.out.println("Pseudoinstance now has " + pseudoInstance.blocks.size() + " blocks");
				
				return combinedResult;
			}
			else
			{
				UnorderedGroupingFeature result = results.get(0);
				System.out.println("Blocks in result: " + result.getBlocks().size());
				pseudoInstance.setBlocks(result.getBlocks());
				System.out.println("Pseudoinstance now has " + pseudoInstance.blocks + " blocks");
				
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
	
	
	
	// Return a list of UnorderedGroupingFeatures that are filtered by
	// Unary predicates and subselections
	private List<UnorderedGroupingFeature> filterByUnaryPredicates(
								Set<UnorderedGroupingFeature> matches, Scene s)
	{
		List<UnorderedGroupingFeature> results = 
				new ArrayList<UnorderedGroupingFeature>();
		
		if (predicates.isEmpty() )
		{
			for (UnorderedGroupingFeature element: matches)
			{
				UnorderedGroupingFeature result = getSubselection(element);
				if (result != null)
				{
					results.add(result);
				}

			}
			
			return results;
		}
		
		for (UnorderedGroupingFeature element : matches)
		{
			if (element.isEmpty())
				continue;
			
			for (Predicate predicate : predicates)
			{
				if (predicate.evaluate(element,s))
					results.add(getSubselection(element));
			}
		}
		
		return results;
		
	}
	
	private UnorderedGroupingFeature getSubselection(UnorderedGroupingFeature ugf)
	{
		// There are no subselections, so use the whole set
		if (subSelections.isEmpty())
			return ugf;
		
		for (String selection : subSelections)
		{
			if (ugf.getFeatures().get(selection) != null)
			{
				Feature f = ugf.getFeatures().get(selection);
				System.out.println("Pulling subselection of " + selection);
				if (f instanceof UnorderedGroupingFeature)
					return (UnorderedGroupingFeature)f;
				else
				{
					UnorderedGroupingFeature newUGF = new UnorderedGroupingFeature("subselection");
					newUGF.add(f);
					return newUGF;
				}
			}
			
		}
		
		
		return null;
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
		
		for (String subselection : subSelections)
		{
			int ordinal = -1;
			try {
				ordinal = Integer.parseInt(subselection);
			}
			catch (NumberFormatException nfe)
			{
				continue;
			}
			if (ordinal != -1)
				sb.append(LanguageGeneration.ordinalString(ordinal));
		}
		
		if (universal)
			sb.append(" every ");

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
	
	public boolean isDefinite()
	{
		for (String indicator : definiteHeadTermIndicators)
		{
			System.out.println("Indicator: |" + indicator + "|");
			System.out.println("Indicator: |" + headTerm.get(KQMLUtilities.ONT_REF).stringValue() + "|");
			if (headTerm.get(KQMLUtilities.ONT_REF).stringValue().equalsIgnoreCase(indicator))
				return true;
		}
		
		return false;
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
