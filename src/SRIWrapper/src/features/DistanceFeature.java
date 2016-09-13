package features;

import TRIPS.KQML.*;
import models.FeatureConstraint;
import environment.*;

public class DistanceFeature extends DecimalFeature {

	public static String[] indicatorGlosses = {"(:* ONT::QUANTITY W::DIAMETER)", "(:* ONT::QUANTITY W::RADIUS)",
									"(:* ONT::LETTER-SYMBOL W::SPACE)"};
	public static String[] indicatorClasses = {};
	public static String[] indicatorScales = {"ONT::HEIGHT-SCALE","ONT::LINEAR-SCALE"};
	
	public DistanceFeature(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}
	
	// Moved to ModelInstantiation, leaving old version just in case
	
//	public static FeatureConstraint generateConstraintFromKQML(StructureInstance instance,
//															KQMLList term, KQMLList context)
//	{
//		KQMLObject concept = term.get(2);
//		KQMLList conceptList = null;
//		String scale = "unnamed";
//		String ground = null;
//		String operator = null;
//		KQMLList groundTerm = null;
//		if (concept instanceof KQMLList)
//		{
//			conceptList = (KQMLList)concept;
//			// Get the scale for naming the feature
//			if (term.getKeywordArg(":SCALE") != null)
//				scale = term.getKeywordArg(":SCALE").stringValue();
//			
//			// Get the comparison object or property
//			if (term.getKeywordArg(":GROUND") != null)
//			{
//				ground = term.getKeywordArg(":GROUND").stringValue();
//				for (KQMLObject contextTerm : context)
//				{
//					if (contextTerm instanceof KQMLList)
//					{
//						KQMLList contextTermList = (KQMLList)contextTerm;
//						if (contextTermList.get(1).stringValue().equalsIgnoreCase(ground))
//							groundTerm = contextTermList;
//					}
//				}
//			}
//			// Get the ONT::MORE-VAL or equivalent
//			operator = conceptList.get(1).stringValue();
//			DistanceFeature newDistanceFeature = new DistanceFeature(scale);
//			
//			
//			DistanceFeature groundDistanceFeature = null;
//			if (groundTerm != null)
//			{
//				groundDistanceFeature = generateFeatureFromKQML(groundTerm);
//			}
//			return new FeatureConstraint(newDistanceFeature,
//					FeatureConstraint.operatorFromTRIPS(operator),groundDistanceFeature);
//		}
//		
//		return null;
//	}


}
