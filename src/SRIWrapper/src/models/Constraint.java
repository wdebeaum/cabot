package models;

import environment.Scene;
import features.Feature;

public interface Constraint {

	public String getDescription();
	public Feature getFeature();
	public String getFeatureName();
	public String reason();
	public String reason(boolean satisfied);
	public boolean isSatisfied(Scene subjectScene, Scene totalScene);
	public boolean isSatisfied(Scene s);
	public boolean setValue(double value);
}
