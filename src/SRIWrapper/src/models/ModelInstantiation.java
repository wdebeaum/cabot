package models;

import java.util.*;

import utilities.KQMLUtilities;
import environment.*;
import TRIPS.KQML.*;
import features.*;
import models.FeatureConstraint.ComparisonType;
import models.FeatureConstraint.Operator;
import spatialreasoning.Predicate;


public class ModelInstantiation {

	private StructureInstance currentStructureInstance;
	private HashMap<String,FeatureGroup> components;
	public List<FeatureConstraint> featureConstraints;
	public List<StructureConstraint> structureConstraints;
	private List<StructureInstance> positiveExamples;
	private Set<ReferringExpression> referringExpressions;
	String[] generalFeaturesToAsk = {FeatureConstants.HEIGHT, FeatureConstants.WIDTH};
	String[] subFeaturesToAsk = {FeatureConstants.NUMBER};
	private boolean introduced;
	
	public ModelInstantiation() {
		currentStructureInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		components = new HashMap<String,FeatureGroup>();
		featureConstraints = new ArrayList<FeatureConstraint>();
		structureConstraints = new ArrayList<StructureConstraint>();
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
		toReturn.addAll(featureConstraints);
		toReturn.addAll(structureConstraints);
		return toReturn;
	}
	
	public Set<Feature> getConstrainedGeneralFeatures()
	{
		HashSet<Feature> features = new HashSet<Feature>();
		for (FeatureConstraint fc : featureConstraints)
		{
			features.add(fc.feature);
		}
		
		return features;
	}
	
	public Set<Feature> getConstrainedSubFeatures()
	{
		HashSet<Feature> features = new HashSet<Feature>();
		for (StructureConstraint sc : structureConstraints)
		{
			features.add(sc.featureConstraint.feature);
		}
		
		return features;
	}
	
	public Set<String> getConstrainedGeneralFeatureNames()
	{
		HashSet<String> features = new HashSet<String>();
		for (FeatureConstraint fc : featureConstraints)
		{
			features.add(fc.feature.getName());
		}
		
		return features;
	}
	
	public Set<String> getConstrainedSubFeatureNames()
	{
		HashSet<String> features = new HashSet<String>();
		for (StructureConstraint sc : structureConstraints)
		{
			features.add(sc.featureConstraint.feature.getName());
		}
		
		return features;
	}
	
	
	public Constraint getConstraintToAsk()
	{
		Set<String> constrainedGeneralFeatureNames = getConstrainedGeneralFeatureNames();
		Set<String> constrainedSubFeatureNames = getConstrainedSubFeatureNames();
		Random rand = new Random();
		//boolean generalQuestion = rand.nextBoolean();
		boolean generalQuestion = true;
		
		int index = rand.nextInt(255);
		int steps = 0;
		String featureName = "";
		
		// Loop through features to ask about until we find one not described
		if (generalQuestion)
		{
			do {
				featureName = generalFeaturesToAsk[index % generalFeaturesToAsk.length];
				index++;
				steps++;
				if (steps > generalFeaturesToAsk.length)
					break;
				
			}
			while (!constrainedGeneralFeatureNames.contains(featureName));
			Feature feature = currentStructureInstance.getFeature(featureName);
			FeatureConstraint fc = new FeatureConstraint(feature, Operator.LEQ,
								ComparisonType.VALUE);
			return fc;
		}
		else
		{
			StructureConstraint structureConstraintToAsk = null;
			for (StructureConstraint sc : structureConstraints)
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
				while (!constrainedSubFeatureNames.contains(featureName));
				
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
	}
	
	public void getConstraintsFromKQML(KQMLList neutralTerm, KQMLList context)
	{
		int constraintsFound = 0;
		introduced = true;
		List<ReferringExpression> refExps = 
				ReferringExpression.getReferringExpressions(neutralTerm, context);
		
		if (refExps.isEmpty())
			System.out.println("No referring expressions found");
		else
		{
			System.out.println("Referring expressions found:");
			for (ReferringExpression refExp : refExps)
			{
				System.out.println("Term: " + refExp.headTerm);
				System.out.println("Predicates: ");
				for (Predicate p : refExp.predicates)
					System.out.println(p.toString());
			}
		}
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			FeatureConstraint fc;
			if (refExps.isEmpty())
			{
				fc = FeatureConstraint.extractFeature(currentStructureInstance,
													termList,context);
				if (fc != null)
				{
					featureConstraints.add(fc);
					constraintsFound++;
				}
			}
			else // TODO : Get all ref exps, not just first one
			{
				ReferringExpression first = refExps.get(0);
				fc = FeatureConstraint.extractFeature(first.getPseudoInstance(),
						termList,context);
				if (fc != null)
				{
					StructureConstraint sc = new StructureConstraint(first,fc);
					structureConstraints.add(sc);
					constraintsFound++;
				}
			}

		}
		
		System.out.println("Found " + constraintsFound + " constraints");
	}
	
	
	public boolean testModelOnStructureInstance(Collection<Block> newBlocks)
	{
		currentStructureInstance.setBlocks(new HashSet<Block>(newBlocks));
		currentStructureInstance.generateFeatures();
		for (FeatureConstraint constraint : featureConstraints)
		{
			System.out.println("Constraint:");
			System.out.println(constraint);
			boolean satisfied = constraint.isSatisfied();
			if (satisfied)
				System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
		}
		
		for (StructureConstraint constraint : structureConstraints)
		{
			
			System.out.println("Constraint:");
			System.out.println(constraint);
			boolean satisfied = constraint.isSatisfied();
			
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
	
	
	
}
