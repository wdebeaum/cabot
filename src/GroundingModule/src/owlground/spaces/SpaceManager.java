package owlground.spaces;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

/*
 * Probably won't be used yet.
 */

public class SpaceManager {
	private HashMap<String,FeatureSpace> featureSpaces;

	public SpaceManager()
	{
		featureSpaces = new HashMap<String, FeatureSpace>();
		
	}

	public boolean addFeatureSpace(FeatureSpace s)
	{
		String name = s.getName();
		s.setSpaceManager(this);
		if (featureSpaces.containsKey(name))
			return false;
			
		featureSpaces.put(name, s);
		
		return true;
	}
	
	public FeatureSpace getFeatureSpaceByName(String spaceName)
	{
		if (!featureSpaces.containsKey(spaceName))
			throw new IllegalArgumentException("No space named " + spaceName + " in SpaceManager.");
		return featureSpaces.get(spaceName);
	}
	
	public HashMap<String, FeatureSpace> getFeatureSpaces()
	{
		return new HashMap<String,FeatureSpace>(featureSpaces);
	}
	
	public void updateFeatureSpaces()
	{
		for (FeatureSpace fs : featureSpaces.values())
		{
			fs.updateRegions();
		}
	}
	
	
}
