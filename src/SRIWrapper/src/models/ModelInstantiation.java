package models;

import java.util.*;

import utilities.KQMLUtilities;
import environment.*;
import TRIPS.KQML.*;
import features.*;
import messages.FeatureParser;
import models.FeatureConstraint.ComparisonType;
import models.FeatureConstraint.Operator;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;


public class ModelInstantiation {

	private StructureInstance currentStructureInstance;
	private HashMap<String,FeatureGroup> components;
	public Set<Constraint> constraints;
	private List<StructureInstance> positiveExamples;
	private Set<ReferringExpression> referringExpressions;
	String[] generalFeaturesToAsk = {FeatureConstants.HEIGHT, FeatureConstants.WIDTH};
	String[] subFeaturesToAsk = {FeatureConstants.NUMBER};
	private boolean introduced;
	
	public ModelInstantiation() {
		currentStructureInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		components = new HashMap<String,FeatureGroup>();
		constraints = new HashSet<Constraint>();
		positiveExamples = new ArrayList<StructureInstance>();
		introduced = false;
	}

	public void addKQMLTerm(KQMLList term, KQMLList context)
	{
		String variableName = term.get(1).stringValue();
		KQMLObject concept = term.get(2);
		KQMLList conceptList = null;
		if (concept instanceof KQMLList)
		{
			conceptList = (KQMLList)concept;
			String ontologyName = conceptList.get(1).stringValue();
			
			if (ontologyName.equalsIgnoreCase("ONT::GENERAL-STRUCTURE"))
			{
				components.put(variableName, currentStructureInstance);
				currentStructureInstance.setName(conceptList.stringValue());
			}
		}
	}
	
	public Set<Constraint> getConstraints()
	{
		Set<Constraint> toReturn = new HashSet<Constraint>();
		toReturn.addAll(constraints);
		return toReturn;
	}
	
	public Set<Feature> getConstrainedGeneralFeatures()
	{
		HashSet<Feature> features = new HashSet<Feature>();
		for (FeatureConstraint fc : getFeatureConstraints())
		{
			features.add(fc.feature);
		}
		
		return features;
	}
	
	public Set<Feature> getConstrainedSubFeatures()
	{
		HashSet<Feature> features = new HashSet<Feature>();
		for (StructureConstraint sc : getStructureConstraints())
		{
			features.add(sc.featureConstraint.feature);
		}
		
		return features;
	}
	
	public Set<String> getConstrainedGeneralFeatureNames()
	{
		HashSet<String> features = new HashSet<String>();
		for (FeatureConstraint fc : getFeatureConstraints())
		{
			features.add(fc.feature.getName());
		}
		
		return features;
	}
	
	public Set<String> getConstrainedSubFeatureNames()
	{
		HashSet<String> features = new HashSet<String>();
		for (StructureConstraint sc : getStructureConstraints())
		{
			features.add(sc.featureConstraint.feature.getName());
		}
		
		return features;
	}
	
	public Set<FeatureConstraint> getFeatureConstraints()
	{
		Set<FeatureConstraint> result = new HashSet<FeatureConstraint>();
		for (Constraint c : constraints)
			if (c instanceof FeatureConstraint)
				result.add((FeatureConstraint)c);
		return result;
	}
	
	public Set<StructureConstraint> getStructureConstraints()
	{
		Set<StructureConstraint> result = new HashSet<StructureConstraint>();
		for (Constraint c : constraints)
			if (c instanceof StructureConstraint)
				result.add((StructureConstraint)c);
		return result;
	}
	
	public Set<PredicateConstraint> getPredicateConstraints()
	{
		Set<PredicateConstraint> result = new HashSet<PredicateConstraint>();
		for (Constraint c : constraints)
			if (c instanceof PredicateConstraint)
				result.add((PredicateConstraint)c);
		return result;
	}
	
	public Constraint getConstraintToAsk()
	{
		Set<String> constrainedGeneralFeatureNames = getConstrainedGeneralFeatureNames();
		Set<String> constrainedSubFeatureNames = getConstrainedSubFeatureNames();
		
		System.out.println("Constrained general features: ");
		for (String s : constrainedGeneralFeatureNames)
			System.out.println(s);
		System.out.println("Constrained structure features: ");
		for (String s : constrainedSubFeatureNames)
			System.out.println(s);
		Random rand = new Random();
		int questionType = rand.nextInt(3);
		int index = rand.nextInt(255);
		int steps = 0;
		String featureName = "";
		
		// Loop through features to ask about until we find one not described
		// General Feature
		if (questionType == 0)
		{
			do {
				featureName = generalFeaturesToAsk[index % generalFeaturesToAsk.length];
				index++;
				steps++;
				if (steps > generalFeaturesToAsk.length)
					return null;
				
			}
			while (constrainedGeneralFeatureNames.contains(featureName));
			
			
			Feature feature = currentStructureInstance.getFeature(featureName);
			FeatureConstraint fc = new FeatureConstraint(feature, Operator.LEQ,
								ComparisonType.VALUE);
			return fc;
		}
		
		else if (questionType == 1 && getPredicateConstraints().isEmpty())
		{
			// Ask about placement of top blocks
			List<UnorderedRowFeature> rows = 
					UnorderedRowFeature.rowsFromBlocks(Scene.currentScene.integerBlockMapping.values());
			
			if (rows.size() > 1)
			{
				
				for (UnorderedRowFeature row : rows)
				{
					Predicate p = new Predicate(PredicateType.TOP);
					// Get the top row
					if (p.evaluate(row))
					{
						System.out.println("Found top row");
						ReferringExpression refExp;
						// Ask where the top block can be
						if (row.getBlocks().size() == 1)
							refExp = new ReferringExpression(p,"ONT::THE","ONT::BLOCK");
						else
							refExp = new ReferringExpression(p,"ONT::THE-SET","ONT::BLOCK");
						PredicateConstraint pc = new PredicateConstraint(refExp);
						return pc;
						// Ask how many blocks can be in the top row
						// If there is no row, skip down below
					}
				}
			}
		}
		
		for (StructureConstraint sc : getStructureConstraints())
		{
			boolean foundConstraint = true;
			do {
				featureName = subFeaturesToAsk[index % subFeaturesToAsk.length];
				index++;
				steps++;
				if (steps > generalFeaturesToAsk.length)
				{
					steps = 0;
					foundConstraint = false;
					break;
				}
			}
			while (constrainedSubFeatureNames.contains(featureName));
			
			if (foundConstraint)
			{
				Feature feature = currentStructureInstance.getFeature(featureName);
				FeatureConstraint fc = new FeatureConstraint(feature, Operator.LEQ,
						ComparisonType.VALUE);
				ReferringExpression refExp = sc.getSubject();
				StructureConstraint toReturn = new StructureConstraint(refExp,fc);
				return toReturn;
			}
			
		}
		return null;
		
	}
	
	public int getConstraintsFromKQML(KQMLList eventTerm, KQMLList context)
	{
		int constraintsFound = 0;
		introduced = true;
		
		ReferringExpressionParser refExpParser = 
				new ReferringExpressionParser(eventTerm, context);
		
		Map<String, ReferringExpression> refExps = 
				refExpParser.extractReferringExpressions();
		
		
		if (refExps.isEmpty())
			System.out.println("No referring expressions found");
		else
		{
			System.out.println("Referring expressions found:");
			for (ReferringExpression refExp : refExps.values())
			{
				System.out.println("Term: " + refExp.headTerm);
				System.out.println("Predicates: ");
				for (Predicate p : refExp.predicates)
					System.out.println(p.toString());
			}
		}
		
		FeatureParser featureParser = new FeatureParser(eventTerm, context,
												refExps, refExpParser.getHeadReferringExpression(),
												currentStructureInstance);
		
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			
			// Cases of "only" as in "only the left column has a height greater than one"
			if (termList.getKeywordArg(":INSTANCE-OF").stringValue().
					equalsIgnoreCase("ONT::RESTRICTION") && 
					refExpParser.getHeadReferringExpression() != null)
			{
				refExpParser.getHeadReferringExpression().setRestricted(true);
			}
		}
		
		Set<Constraint> featureConstraintsFound = featureParser.extractFeatures();
		
		constraintsFound += featureConstraintsFound.size();
		constraints.addAll(featureConstraintsFound);

		PredicateParser predicateParser = new PredicateParser(eventTerm, context,
				refExpParser.getHeadReferringExpression());
		Set<PredicateConstraint> predicateConstraintsFound = 
					predicateParser.extractPredicateConstraints();
		constraintsFound += predicateConstraintsFound.size();
		constraints.addAll(predicateConstraintsFound);
		
		if (constraintsFound == 0)
		{
			System.out.println("Adding predicated refexps to constraints");
			for (ReferringExpression refExp : refExps.values())
			{
				System.out.println("Term: " + refExp.headTerm);
				System.out.println("Predicates: ");
				for (Predicate p : refExp.predicates)
				{
					constraintsFound++;
					constraints.add(new PredicateConstraint(refExp,p));
				}
			}
		}
		
		System.out.println("Found " + constraintsFound + " constraints");
		
		return constraintsFound;
	}
	
	
	public boolean testModelOnStructureInstance(Collection<Block> newBlocks, boolean debug)
	{
		currentStructureInstance.setBlocks(new HashSet<Block>(newBlocks));
		currentStructureInstance.generateFeatures();
		for (FeatureConstraint constraint : getFeatureConstraints())
		{
			if (debug)
			{
				System.out.println("Constraint:");
				System.out.println(constraint);
			}
			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			if (satisfied)
				if (debug)
					System.out.println(" is satisfied");
			else
			{
				if (debug)
					System.out.println(" is not satisfied");
				return false;
			}
		}
		
		for (StructureConstraint constraint : getStructureConstraints())
		{
			if (debug) {
				System.out.println("Constraint:");
				System.out.println(constraint);
			}
			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			
			if (satisfied)
				if (debug)
					System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
				
		}
		
		for (PredicateConstraint constraint : getPredicateConstraints())
		{
			if (debug)
			{
				System.out.println("Constraint:");
				System.out.println(constraint);
			}
			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			
			if (satisfied)
				if (debug)
					System.out.println(" is satisfied");
			else
			{
				if (debug)
					System.out.println(" is not satisfied");
				return false;
			}
							
		}
		return true;
	}
	
	public boolean testModelOnParticularStructureInstanceNoDebug(Collection<Block> newBlocks)
	{
		currentStructureInstance.setBlocks(new HashSet<Block>(newBlocks));
		currentStructureInstance.generateFeatures();
		Scene currentScene = new Scene();
		currentScene.addBlocks(newBlocks);
		
		for (FeatureConstraint constraint : getFeatureConstraints())
		{

			boolean satisfied = constraint.isSatisfied(currentScene);
			if (!satisfied)
				return false;
		}
		
		for (StructureConstraint constraint : getStructureConstraints())
		{
			boolean satisfied = constraint.isSatisfied(currentScene);
			
			if (!satisfied)
				return false;
			
				
		}
		
		for (PredicateConstraint constraint : getPredicateConstraints())
		{
			boolean satisfied = constraint.isSatisfied(currentScene);
			
			if (!satisfied)
				return false;	
		}
		return true;
	}
	
	public boolean testModelOnStructureInstance(Collection<Block> newBlocks)
	{
		currentStructureInstance.setBlocks(new HashSet<Block>(newBlocks));
		currentStructureInstance.generateFeatures();
		for (FeatureConstraint constraint : getFeatureConstraints())
		{

			System.out.println("Constraint:");
			System.out.println(constraint);
			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			if (satisfied)
				System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
		}
		
		for (StructureConstraint constraint : getStructureConstraints())
		{
			System.out.println("Constraint:");
			System.out.println(constraint);

			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			
			if (satisfied)
				System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
			

				
		}
		
		for (PredicateConstraint constraint : getPredicateConstraints())
		{
			System.out.println("Constraint:");
			System.out.println(constraint);

			boolean satisfied = constraint.isSatisfied(Scene.currentScene);
			
			if (satisfied)
				System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
							
		}
		return true;
	}
	
	public void addPositiveExample(StructureInstance positiveExample)
	{
		positiveExamples.add(positiveExample);
	}

	public List<StructureInstance> getPositiveExamples() {
		return positiveExamples;
	}

	public boolean isIntroduced() {
		return introduced;
	}
	
	public void addConstraint(Constraint c)
	{
		constraints.add(c);
	}
	
	
	
}
