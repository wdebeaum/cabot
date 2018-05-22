package models;

import environment.Scene;
import features.Feature;
import spatialreasoning.Predicate;

public class ExistentialConstraint extends StructureConstraint {
	
	public ExistentialConstraint(ReferringExpression subject,
								FeatureConstraint featureConstraint) {
		super(subject, featureConstraint);
	}
	
	public ExistentialConstraint(ReferringExpression subject,
			Predicate predicateConstraint) {
		super(subject, predicateConstraint);
}


}
