package owlground.language;

import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import owlground.Classifiers;
import owlground.WorldManager;
import owlground.language.Demonstration.ImplicationType;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.objects.WorldObjectGroup;

public class Query extends Demonstration{

	public enum QueryType { IDENTIFY, PROPERTY, PROPERTY_YN, POSITION, EXISTENCE }
	private QueryType type;
	private String content;
	private String replyWith;
	private String responseString;

	public Query(Utterance u, QueryType type, String replyWith, String content) {
		super(u,DemonstrationType.QUERY);
		this.type = type;
		this.content = content;
		this.replyWith = replyWith;
	}

	
	public List<WorldObjectGroup> answer()
	{
		WorldManager worldManager = getWorldManager();
		//resolveWorldObjects();
		
		List<String> response = new ArrayList<String>();
		
		switch (type)
		{
		case PROPERTY:
			String featureName = "";
			if (content.equals("(:* ONT::COLOR W::COLOR)"))
				featureName = "rgb";
			else if (content.equals("(:* ONT::SHAPE W::SHAPE)"))
				featureName = "zernike";
			else
				featureName = "rgb";
			
			for (WorldObjectGroup wog : worldObjectGroups)
			{
				for (String property: wog.getCommonPerceivedProperties())
				{
					if (Property.representativeFeatureMap.get(property).equals(featureName))
					{
						responseString = property.split(" ")[2].split("::")[1].replaceAll("\\)", "");
						break;
					}
				}
			}
			break;
		case PROPERTY_YN:
			if (content.equals("color"))
				featureName = "rgb";
			else if (content.equals("shape"))
				featureName = "zernike";
			else
				featureName = "rgb";
			
			for (WorldObjectGroup wog : worldObjectGroups)
			{
				for (String property: wog.getCommonPerceivedProperties())
				{
					if (Property.representativeFeatureMap.get(property).equals(featureName))
					{
						responseString = "true";
						break;
					}
				}
				if (responseString == null)
					responseString = "false";
			}
			break;			
		case IDENTIFY:
			for (WorldObjectGroup wog : worldObjectGroups)
			{
				for (WorldObject wo : wog.getWorldObjects())
					Classifiers.generatePerceivedPropertiesWithKNearestPrechosen(
						worldManager,wo,worldManager.getPropertyK(),worldManager.getMinUtterances());
			}
			break;
		default:
			break;
		}

		return worldObjectGroups;
	}


	public String getReplyWith() {
		return replyWith;
	}
}
