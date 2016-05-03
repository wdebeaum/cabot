package owlground.objects;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.spaces.FeatureSpace;

public class ObjectClass extends AbstractDescriptor {

	private HashSet<FeatureSpace> representativeFeatures;
	private List<ObjectModel> objectModels;
	
	public static final String defaultClass = "(:* ONT::PHYS-OBJECT W::OBJECT)";
	public static final String nullClass = "(:* null null)";
	public static final String[] falseWords = {"(:* ONT::REFERENTIAL-SEM W::ONE)"};
	public static Map<String,String> representativeFeatureMap = new HashMap<String,String>() {{
		put("(:* ONT::PHYS-OBJECT W::TRUCK)","hu");
		put("(:* ONT::REFERENTIAL-SEM W::TRUCK)","hu");
		put("(:* ONT::BOX W::BOX)","hu");
		put("(:* ONT::REFERENTIAL-SEM W::BLOCK)","hu");
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

	public ObjectClass(String name) {
		super(name);
		this.objectModels = new ArrayList<ObjectModel>();
		this.representativeFeatures = new HashSet<FeatureSpace>();
	}

	public HashSet<FeatureSpace> getRepresentativeFeatures() {
		return representativeFeatures;
	}
	
	public void addObjectModel(ObjectModel om)
	{
		objectModels.add(om);
	}
	
	public List<ObjectModel> getObjectModels()
	{
		return objectModels;
	}
	
	public static boolean isFalseWord(String className)
	{
		for (String word : falseWords)
		{
			if (word.equals(className.trim()))
				return true;
		}
		return false;
	}
	
	public static boolean isFalseWord(ObjectClass objectClass)
	{
		for (String word : falseWords)
		{
			if (word.equals(objectClass.getName().trim()))
				return true;
		}
		return false;
	}

}
