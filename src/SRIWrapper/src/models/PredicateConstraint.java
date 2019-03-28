package models;

import java.util.ArrayList;
import java.util.List;

import environment.Scene;
import environment.StructureInstance;
import features.Feature;
import features.FeatureConstants;
import features.UnorderedGroupingFeature;
import spatialreasoning.Predicate;

public class PredicateConstraint implements Constraint {

	ReferringExpression subject;
	Predicate predicate;
	List<ReferringExpression> objects;
	
	public PredicateConstraint(ReferringExpression subject) {
		this(subject,null);
	}
	
	public PredicateConstraint(ReferringExpression subject, 
			Predicate predicateConstraint) {
		this.subject = subject;
		this.predicate = predicateConstraint;
		objects = new ArrayList<ReferringExpression>();
	}

	@Override
	public String getDescription() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Feature getFeature() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String reason() {
		return reason(isSatisfied(Scene.currentScene));
	}
	
	@Override
	public String reason(boolean satisfied) {
		StringBuilder sb = new StringBuilder();
		sb.append("the " + subject + " is ");
		if (!satisfied)
			sb.append("not ");
		sb.append(predicate.prettyString());
		if (!objects.isEmpty())
			sb.append(" the " + objects.get(0));
		
		return sb.toString();
		
	}
	
	public void setPredicate(Predicate p)
	{
		this.predicate = p;
	}
	
	public Predicate getPredicate()
	{
		return predicate;
	}
	
	public boolean isSatisfied(Scene subjectScene, Scene totalScene)
	{
		return isSatisfied(totalScene);
	}

	public boolean isSatisfied(Scene s)
	{
		UnorderedGroupingFeature subjectResult = subject.evaluate(s);
		
		System.out.println(this);
		System.out.println("has " + 
				subject.getPseudoInstance().getFeature(FeatureConstants.NUMBER) + " blocks");
		
		for (ReferringExpression object : objects)
			object.evaluate(s);
		
		if (subjectResult == null || subjectResult.getBlocks().isEmpty())
			return false;
		
		if (predicate != null)
		{
			StructureInstance si = subject.getPseudoInstance();
			UnorderedGroupingFeature subjectUGF = 
					(UnorderedGroupingFeature)si.getFeature(FeatureConstants.GROUPING);
			if (objects.size() == 0)
			{
				return predicate.evaluate(subjectUGF,s);
			}
			else if (objects.size() == 1)
			{
				objects.get(0).evaluate(s);
				StructureInstance other = objects.get(0).getPseudoInstance();
				
				UnorderedGroupingFeature otherUGF = 
						(UnorderedGroupingFeature)other.getFeature(FeatureConstants.GROUPING);
				return predicate.evaluate(subjectUGF,otherUGF);
			}
			else // Merge all objects together
			{
				UnorderedGroupingFeature otherUGF = new UnorderedGroupingFeature("objects");
				for (ReferringExpression object : objects)
				{
					object.evaluate(s);
					otherUGF.add(object.getPseudoInstance()
							.getFeature(FeatureConstants.GROUPING));
				}
				
				return predicate.evaluate(subjectUGF,otherUGF);
			}
		}
		
		return true;
			
	}
	
	public ReferringExpression getSubject() {
		return subject;
	}
	
	public void addObject(ReferringExpression object)
	{
		objects.add(object);
	}
	

	@Override
	public boolean setValue(double value) {
		// TODO Auto-generated method stub
		return false;
	}
	
	public String getFeatureName()
	{
		return "location";
	}

	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("PredicateConstraint: " + subject + "\n");
		if (predicate != null)
			sb.append(predicate.toString() + "\n");
		
		return sb.toString();
	}
	
}
