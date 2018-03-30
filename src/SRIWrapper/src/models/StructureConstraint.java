package models;

import java.util.ArrayList;
import java.util.List;

import TRIPS.KQML.KQMLList;
import environment.Scene;
import environment.StructureInstance;
import features.Feature;
import features.FeatureConstants;
import features.UnorderedGroupingFeature;
import spatialreasoning.Predicate;

public class StructureConstraint implements Constraint {

	ReferringExpression subject;
	FeatureConstraint featureConstraint;
	Predicate predicateConstraint;
	List<ReferringExpression> objects;
	
	public StructureConstraint(ReferringExpression subject, 
			FeatureConstraint featureConstraint) {
		this.subject = subject;
		this.featureConstraint = featureConstraint;
		this.predicateConstraint = null;
		objects = new ArrayList<ReferringExpression>();
	}
	
	public StructureConstraint(ReferringExpression subject, 
			Predicate predicateConstraint) {
		this.subject = subject;
		this.featureConstraint = null;
		this.predicateConstraint = predicateConstraint;
		objects = new ArrayList<ReferringExpression>();
	}
	
	public static StructureConstraint extractStructureConstraint(KQMLList neutralTerm, 
														KQMLList tree)
	{
		ReferringExpression.getDefiniteHeadTermSymbols(tree);
		return null;
	}
	
	public boolean isSatisfied(Scene s)
	{
		subject.evaluate(s);
		
		System.out.println(this);
		System.out.println("has " + subject.getPseudoInstance().getFeature(FeatureConstants.NUMBER) + " blocks");
	
		
		for (ReferringExpression object : objects)
			object.evaluate(s);
		
		if (featureConstraint != null && !featureConstraint.isSatisfied())
			return false;
		
		// TODO
		if (predicateConstraint != null)
		{
			StructureInstance si = subject.getPseudoInstance();
			predicateConstraint.evaluate(
					(UnorderedGroupingFeature)si.getFeature(FeatureConstants.GROUPING));
		}
		
		return true;
			
	}
	
	public boolean isSatisfied()
	{
		return isSatisfied(Scene.currentScene);
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("StructureConstraint: " + subject + "\n");
		if (featureConstraint != null)
			sb.append(featureConstraint.toString() + "\n");
		if (predicateConstraint != null)
			sb.append(predicateConstraint.toString() + "\n");
		return sb.toString();
	}
	
	public String reason()
	{
		return subject + "'s " + featureConstraint.reason();
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

}
