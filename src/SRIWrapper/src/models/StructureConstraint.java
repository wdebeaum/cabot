package models;

import java.util.ArrayList;
import java.util.List;

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
	
	public boolean isSatisfied(Scene s)
	{
		return isSatisfied(s,subject);
		
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
		
		Scene subjectScene = new Scene();
		subjectScene.addBlocks(referredBlocks.getBlocks());
		
		//if (featureConstraint != null && !featureConstraint.isSatisfied(s))
		if (featureConstraint != null && !featureConstraint.isSatisfied(subjectScene))
			return false;
		
		// If restricted, check all other referring expressions of the same type
		// and make sure they don't fit
		if (newSubject.isRestricted())
		{
			UnorderedGroupingFeature inverse = newSubject.inverseGroupingFeature;
			System.out.println("Testing other instances for restriction:");
			for (FeatureGroup other: inverse.getValue())
			{
				String featureNameToTest = featureConstraint.feature.getName();
				if (other.getFeatures().containsKey(featureNameToTest))
				{
					Feature otherFeature = other.getFeatures().get(featureNameToTest);
					System.out.println("Other grouping has " + featureNameToTest + " of "
							+ otherFeature.getValue());
					if (featureConstraint.isSatisfied(otherFeature, s))
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
		return isSatisfied(Scene.currentScene);
	}
	
	public boolean isInferred()
	{
		return featureConstraint.isInferred();
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("StructureConstraint: " + subject + "\n");
		if (featureConstraint != null)
			sb.append(featureConstraint.toString() + "\n");
		return sb.toString();
	}
	
	public String reason()
	{
		return subject + "'s " + featureConstraint.reason();
	}
	
	public String reason(boolean satisfied)
	{
		return subject + "'s " + featureConstraint.reason(satisfied);
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
