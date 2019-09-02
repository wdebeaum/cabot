package models;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import TRIPS.KQML.KQMLList;
import environment.Scene;
import environment.StructureInstance;
import features.Feature;
import features.FeatureConstants;
import features.FeatureGroup;
import features.UnorderedGroupingFeature;
import spatialreasoning.Predicate;

public class StructureConstraint implements Constraint {

	ReferringExpression subject;
	FeatureConstraint featureConstraint;
	List<ReferringExpression> objects;
	
	public StructureConstraint(ReferringExpression subject, 
			FeatureConstraint featureConstraint) {
		this.subject = subject;
		this.featureConstraint = featureConstraint;
		objects = new ArrayList<ReferringExpression>();
	}
	
	public StructureConstraint(ReferringExpression subject, 
			Predicate predicateConstraint) {
		this.subject = subject;
		this.featureConstraint = null;
		objects = new ArrayList<ReferringExpression>();
	}
	
	public static StructureConstraint extractStructureConstraint(KQMLList neutralTerm, 
														KQMLList tree)
	{
		ReferringExpressionParser.getDefiniteHeadTermSymbols(tree);
		return null;
	}
	
	public boolean isSatisfied(Scene subjectScene, Scene totalScene)
	{
		return isSatisfied(subjectScene,subject);
		
	}
	
	public boolean isSatisfied(Scene s, ReferringExpression newSubject)
	{
		UnorderedGroupingFeature referredBlocks = newSubject.evaluate(s);
		
		System.out.println(this);
		System.out.println("has " + 
				newSubject.getPseudoInstance().getFeature(FeatureConstants.NUMBER) + 
				" blocks");
	
		if (referredBlocks == null)
			return false;
		
		for (ReferringExpression object : objects)
			object.evaluate(s);
		
		// TODO: Plural handling here?
		// TODO: Universal handling?
		
		List<FeatureGroup> individualInstances = referredBlocks.getValue();
		
		// int numberOfReferredInstances = 1;
		int numberOfReferredInstances = referredBlocks.getValue().size();
		// Check if this UGF is holding the right types of objects to check quantifier constraints
		// Note this just checks for at least one matching element
		boolean subStructures = false;
		for (FeatureGroup instance : individualInstances)
		{
			if (instance instanceof Feature)
			{
				if (((Feature)instance).getName().equalsIgnoreCase(newSubject.getInstanceOf()) ||
						((Feature)instance).getName().equalsIgnoreCase(newSubject.getElementType()))
				{
					numberOfReferredInstances = individualInstances.size();
					subStructures = true;
					System.out.println("RefExp has substructures");
					break;
				}
						
			}
		}
		int satisfactions = 0;
		// Check here for errors
		if (subStructures) {
			for (FeatureGroup instance : individualInstances)
			{
				if (!(instance instanceof UnorderedGroupingFeature))
					continue;
				System.out.println("Substructure is an UnorderedGroupingFeature");
				UnorderedGroupingFeature instanceUGF = (UnorderedGroupingFeature)instance;
				Scene subjectScene = new Scene();
				subjectScene.addBlocks(instanceUGF.getBlocks());
				
				if (instanceUGF.getFeatures().containsKey(featureConstraint.getFeature().getName()))
				{
					Feature featureToTest = instanceUGF.getFeatures().get(featureConstraint.getFeature().getName());
					System.out.println("Found matching feature " + featureToTest.getName());
					if (featureConstraint != null && featureConstraint.isSatisfied(featureToTest, subjectScene, s))
						satisfactions++;
				}
			}
			
			if (!newSubject.getQuantifier().validSatisfactionCount(satisfactions, numberOfReferredInstances))
				return false;
			
			
		}
		else
		{
			Scene subjectScene = new Scene();
			subjectScene.addBlocks(referredBlocks.getBlocks());
			
			//if (featureConstraint != null && !featureConstraint.isSatisfied(s))
			if (featureConstraint != null && !featureConstraint.isSatisfied(subjectScene, s))
				return false;
		}
		
		// If restricted, check all other referring expressions of the same type
		// and make sure they don't fit
		if (newSubject.isRestricted())
		{
			UnorderedGroupingFeature inverse = newSubject.inverseGroupingFeature;
			System.out.println("Testing other instances for restriction of feature: " +
								featureConstraint.feature.getName());
			for (FeatureGroup other: inverse.getValue())
			{
				System.out.println("Inverse Other features:");
				for (Entry<String,Feature> entry : other.getFeatures().entrySet())
				{
					System.out.println("Feature: " + entry.getKey());
					System.out.println("Value: " + entry.getValue());
				}
				String featureNameToTest = featureConstraint.feature.getName();
				if (other.getFeatures().containsKey(featureNameToTest))
				{
					Feature otherFeature = other.getFeatures().get(featureNameToTest);
					System.out.println("Other grouping has " + featureNameToTest + " of "
							+ otherFeature.getValue());
					if (featureConstraint.isSatisfied(otherFeature, s, s))
					{
						System.out.println("Another matching object type satisfied the"
								+ " restricted feature constraint");
						return false;
					}
				}
			}
		}
		
		
		return true;
	}
	
	public boolean isSatisfied()
	{
		return isSatisfied(Scene.currentScene, Scene.currentScene);
	}
	
	@Override
	public boolean isSatisfied(Scene s)
	{
		return isSatisfied(s,s);
	}
	
	public boolean isInferred()
	{
		return featureConstraint.isInferred();
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("StructureConstraint subject:\n" + subject.toString(true) + "\n");
		sb.append("---------\n");
		sb.append("Restricted (i.e. 'only'): " + subject.isRestricted() + "\n");
		sb.append("Comparison objects: \n" );
		for (ReferringExpression object : objects)
			sb.append(object.toString());
		
		if (featureConstraint != null)
			sb.append("Feature: \n" + featureConstraint.toString() + "\n");
		return sb.toString();
	}
	
	public String reason()
	{
		return subject.getDescription() + "'s " + featureConstraint.reason();
	}
	
	public String reason(boolean satisfied)
	{
		return subject.getDescription() + "'s " + featureConstraint.reason(satisfied);
	}
	

	@Override
	public String getDescription() {
		// TODO Auto-generated method stub
		return featureConstraint.getDescription() + " of the " + subject;
	}

	@Override
	public Feature getFeature() {
		// TODO Auto-generated method stub
		return featureConstraint.getFeature();
	}

	public ReferringExpression getSubject() {
		return subject;
	}
	
	public String getFeatureName()
	{
		return getFeature().getPrettyName();
	}
	
	@Override
	public boolean setValue(double value)
	{
		if (featureConstraint == null)
			return false;
		
		featureConstraint.setValue(value);
		return true;
	}

}
