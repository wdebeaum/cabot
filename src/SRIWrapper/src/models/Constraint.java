package models;

import features.Feature;

public interface Constraint {

	public String getDescription();
	public Feature getFeature();
	public String reason();
	public boolean isSatisfied();
}
