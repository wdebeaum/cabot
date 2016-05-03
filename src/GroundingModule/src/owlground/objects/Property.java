package owlground.objects;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;
/*
 * A Property of an object - consists of the ontology string as the name, 
 * the domain, and the PerceptClusters that correspond to the times it was
 * mentioned.
 */
public class Property extends AbstractDescriptor {
	private FeatureSpace featureSpace;
	private String domain;
	private FeatureSpace representativeFeature;
	private String parentName;
	
	public static final String[] falseWords = {"ONT::ASSOC-WITH", "(:* ONT::ASSOC-WITH W::OF)"};
	
	public static Map<String,String> representativeFeatureMap = new HashMap<String,String>() {{
			put("(:* ONT::RED W::RED)", "rgb");
			put("(:* ONT::BLUE W::BLUE)","rgb");
			put("(:* ONT::MODIFIER W::SQUARE)", "shaperatio");
			put("(:* ONT::EVENT-DURATION-MODIFIER W::SHORT)", "shaperatio");
			put("(:* ONT::BROAD W::SHORT)", "shaperatio");
			put("(:* ONT::MODIFIER W::TALL)", "shaperatio");
			put("(:* ONT::MODIFIER W::YELLOW)", "rgb");
			put("(:* ONT::REFERENTIAL-SEM W::YELLOW)","rgb");
			put("(:* ONT::GREEN W::GREEN)","rgb");
			put("(:* ONT::REFERENTIAL-SEM W::SQUARE)","shaperatio");
			put("(:* ONT::WHITE W::WHITE)","rgb");
			put("(:* ONT::BLACK W::BLACK)","rgb");
			put("(:* ONT::ORANGE W::ORANGE)","rgb");
			put("(:* ONT::LARGE W::LARGE)","size");
			put("(:* ONT::LARGE W::BIG)","size");
			put("(:* ONT::SMALL W::SMALL)","size");
			put("(:* ONT::MODIFIER W::CIRCULAR)","zernike");
			put("(:* ONT::MODIFIER W::SEMICIRCULAR)","zernike");
			put("(:* ONT::SHAPE-VAL W::ROUND)","zernike");
			put("(:* ONT::MODIFIER W::RECTANGULAR)","shaperatio");
			put("(:* ONT::MODIFIER W::CARDBOARD)","rgb");
	}};
	
	public static Map<String, FeatureSpace> getRepresentativeFeatureSpaceMap(FeatureManager session)
	{
		HashMap<String, FeatureSpace> result = new HashMap<String, FeatureSpace>();
		for (Entry<String,String> entry : representativeFeatureMap.entrySet())
		{
			result.put(entry.getKey(), session.getSpaceManager().getFeatureSpaceByName(entry.getValue()));
		}
		
		return result;
	}

	public Property(String name)
	{
		super(name);
		
		parentName = name.trim().split("\\s+")[1];
		representativeFeature = null;
	}


	public void setRepresentativeFeature(FeatureSpace representativeFeature) {
		this.representativeFeature = representativeFeature;
	}
	
	public FeatureSpace getRepresentativeFeature() {
		return representativeFeature;
	}
	
	public String toString()
	{
		return super.toString() + "\nRep. feature: " + representativeFeature;
	}

	public boolean isComparative()
	{
		return parentName.equals("ONT::MORE-VAL") || parentName.equals("ONT::LESS-VAL");
	}
	
	public boolean isSuperlative()
	{
		return parentName.equals("ONT::MAX-VAL") || parentName.equals("ONT::MIN-VAL");
	}
	
	public Set<PerceptCluster> getPerceptClustersInClassContext(ObjectClass oc)
	{
		Set<PerceptCluster> classContextPerceptClusters = new HashSet<PerceptCluster>();
		classContextPerceptClusters.addAll(oc.getPerceptClusters().values());
		classContextPerceptClusters.retainAll(perceptClusters.values());
		return classContextPerceptClusters;
		
	}
	
	public static boolean isFalseWord(String property)
	{
		for (String word : falseWords)
		{
			if (word.equals(property.trim()))
				return true;
		}
		return false;
	}
	
	public static boolean isFalseWord(Property property)
	{
		for (String word : falseWords)
		{
			if (word.equals(property.getName().trim()))
				return true;
		}
		return false;
	}

}
