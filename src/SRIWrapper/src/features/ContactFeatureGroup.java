package features;

import java.util.HashMap;
import java.util.List;

import models.FeatureConstraint;
import TRIPS.KQML.KQMLList;

public class ContactFeatureGroup implements FeatureGroup{

	DistanceFeature averageSpacing;
	DecimalFeature spacingVariance;
	
	protected static String[] indicatorClasses = {"ONT::ADJACENT"};
	public static String[] indicatorGlosses = {};
	public static String[] indicatorScales = {"ONT::NEAR-RELN","ONT::REMOTE"};
	
	public ContactFeatureGroup() {
		averageSpacing = new DistanceFeature("averageSpacing");
		spacingVariance = new DecimalFeature("spacingVariance");
	}

	@Override
	public HashMap<String,Feature> getFeatures() {
		// TODO Auto-generated method stub
		return null;
	}


}
