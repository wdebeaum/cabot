package models;

import java.util.*;
import java.util.Map.Entry;

import utilities.ConstraintLogger;
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
	public List<Constraint> constraints;
	public ReferringExpression lastUnresolvedObjectReference;
	private List<StructureInstance> positiveExamples;
	private Set<ReferringExpression> referringExpressions;
	String[] generalFeaturesToAsk = {FeatureConstants.HEIGHT, FeatureConstants.WIDTH};
	String[] subFeaturesToAsk = {FeatureConstants.NUMBER};
	private String name;
	public Constraint lastConstraint;
	private boolean introduced;
	
	public ModelInstantiation() {

		this("unnamed");
	}
	
	public ModelInstantiation(String name)
	{
		currentStructureInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		components = new HashMap<String,FeatureGroup>();
		constraints = new ArrayList<Constraint>();
		positiveExamples = new ArrayList<StructureInstance>();
		introduced = false;
		lastConstraint = null;
		this.name = name;
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
		int questionType = 0;
		if (lastUnresolvedObjectReference != null)
			questionType = 2;
		questionType = rand.nextInt(3);
		int index = rand.nextInt(255);
		int steps = 0;
		String featureName = "";
		
		if (Scene.currentScene == null || Scene.currentScene.integerBlockMapping == null)
		{
			System.out.println("No blocks seen");
			return null;
		}
		
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
						{
							refExp = new ReferringExpression(p,"ONT::THE",FeatureConstants.BLOCK);
							PredicateConstraint pc = new PredicateConstraint(refExp);
							return pc;
						}
						else // Ask how many blocks can be in the top row
						{
							refExp = new ReferringExpression(p,"ONT::THE", FeatureConstants.ROW);
							Feature feature = refExp.getPseudoInstance().getFeature(FeatureConstants.NUMBER);
							FeatureConstraint fc = new FeatureConstraint(feature, Operator.LEQ,
									ComparisonType.VALUE);	
							StructureConstraint toReturn = new StructureConstraint(refExp,fc);
							return toReturn;
							
						}
						
						
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
				ReferringExpression refExp;
				
				if (lastUnresolvedObjectReference != null)
					refExp = lastUnresolvedObjectReference;
				else
					refExp = new ReferringExpression(sc.getSubject());
				Feature feature = refExp.getPseudoInstance().getFeature(featureName);
				FeatureConstraint fc = new FeatureConstraint(feature, Operator.LEQ,
						ComparisonType.VALUE);
				
				StructureConstraint toReturn = new StructureConstraint(refExp,fc);
				return toReturn;
			}
			
		}
		return null;
		
	}
	
	public ConstraintBundle getConstraintsFromKQML(KQMLList eventTerm, KQMLList context)
	{
		
		introduced = true;
		
		ReferringExpressionParser refExpParser = 
				new ReferringExpressionParser(eventTerm, context);
		
		Map<String, ReferringExpression> refExps = 
				refExpParser.extractReferringExpressions();
		
		ConstraintBundle cb = new ConstraintBundle();
		
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
			if (isRestricted(termList.getKeywordArg(":INSTANCE-OF").stringValue()) && 
					refExpParser.getHeadReferringExpression() != null)
			{
				refExpParser.getHeadReferringExpression().setRestricted(true);
			}
		}
		
		Set<Constraint> constraintsFound = featureParser.extractFeatures();
		
		// Remove inferred constraints if we found others
		if (constraintsFound.size() > 1)
		{
			for (Constraint c : constraintsFound)
			{
				if (c instanceof FeatureConstraint && !((FeatureConstraint)c).isInferred())
					cb.add(c);
				if (c instanceof StructureConstraint && !((StructureConstraint)c).isInferred())
					cb.add(c);
			}
			// No uninferred constraints, keep the existential ones
			if (cb.size() == 0)
			{
				for (Constraint c : constraintsFound)
				{
					if (c instanceof FeatureConstraint && ((FeatureConstraint)c).isExistential())
						cb.add(c);
				}
			}
		}
		else
			cb.addAll(constraintsFound);
		
		//cb.addAll(featureConstraintsFound);

		PredicateParser predicateParser = new PredicateParser(eventTerm, context,
				refExpParser.getHeadReferringExpression());
		Set<Constraint> predicateConstraintsFound = 
					predicateParser.extractPredicateConstraints();
		
		cb.addAll(predicateConstraintsFound);
		
		if (cb.size() == 0)
		{
			System.out.println("Adding predicated refexps to constraints");
			for (ReferringExpression refExp : refExps.values())
			{
				System.out.println("Term: " + refExp.headTerm);
				System.out.println("Predicates: ");
				for (Predicate p : refExp.predicates)
				{
					
					PredicateConstraint pc = new PredicateConstraint(refExp,p);
					cb.add(pc);
					
				}
			}
		}
		
		// Keep this for generation
		for (Constraint c : cb.getConstraints())
			lastConstraint = c;
		
		System.out.println("Found " + cb.getConstraints().size() + " constraints");
		
		constraints.addAll(cb.getConstraints());
		
		for (Constraint c : cb.getConstraints())
			ConstraintLogger.writeNewConstraint(c);
		
		return cb;
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
			//System.out.println("Feature constraints:");
			System.out.println(constraint);
			boolean satisfied = constraint.isSatisfied(currentScene);
			//System.out.println("Feature value: " + constraint.getFeature().getValue());
			if (!satisfied)
			{
				//System.out.println("Not satisfied");
				return false;
			}
		}
		
		for (StructureConstraint constraint : getStructureConstraints())
		{
			boolean satisfied = constraint.isSatisfied(currentScene);
			
			if (!satisfied)
			{
				//System.out.println("Constraint" + constraint + " not satisfied");
				return false;
			}

			//System.out.println("Constraint" + constraint + " satisfied");

			
				
		}
		
		for (PredicateConstraint constraint : getPredicateConstraints())
		{
			boolean satisfied = constraint.isSatisfied(currentScene);
			
			if (!satisfied)
			{
				//System.out.println("Constraint" + constraint + " not satisfied");
				return false;
			}

			System.out.println("Constraint" + constraint + " satisfied");
		}
		return true;
	}
	
	public boolean testModelOnStructureInstance(Collection<Block> newBlocks)
	{
		currentStructureInstance.setBlocks(new HashSet<Block>(newBlocks));
		currentStructureInstance.generateFeatures();
		
		System.out.println("Structure Instance features tested:");
		for (Entry<String, Feature> f: currentStructureInstance.getFeatures().entrySet())
		{
			System.out.println(f.getKey() + " : " + f.getValue().hashCode());
		}
		for (FeatureConstraint constraint : getFeatureConstraints())
		{

			System.out.println("Feature Constraint:");
			System.out.println(constraint);
			System.out.println("Feature hash:");
			System.out.println(constraint.getFeature().hashCode());
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
			System.out.println("Structure Constraint:");
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
			System.out.println("Predicate Constraint:");
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
	
	public void forgetLastConstraint()
	{
		if (lastConstraint != null && constraints.contains(lastConstraint))
			constraints.remove(lastConstraint);
	}
	
	public static boolean isRestricted(String instanceType)
	{
		return (instanceType.equalsIgnoreCase("ONT::RESTRICTION") ||
				instanceType.equalsIgnoreCase("ONT::EVAL-WRT-EXPECTATION"));
	}
	
}
