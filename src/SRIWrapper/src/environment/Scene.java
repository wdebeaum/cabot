package environment;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;

import org.json.simple.JSONObject;
import org.json.simple.JSONArray;

import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import TRIPS.KQML.*;

public class Scene {

	public HashMap<Integer, Block> integerBlockMapping;
	public boolean isStable;
	public String timestamp;
	public static volatile Scene currentScene = null;
	public Block pointedTo;

	
	public Scene(JSONObject blocksJson, JSONObject blocksMetadataJson)
	{
		isStable = (boolean)blocksJson.get("IsStable");
		timestamp = (String)blocksJson.get("Timestamp");
		integerBlockMapping = new HashMap<Integer,Block>();
		JSONArray blocks = (JSONArray)(blocksJson.get("BlockStates"));
		for (Object blockJson : blocks)
		{
			Block b = new Block((JSONObject)blockJson);
			if (integerBlockMapping.containsKey(b.id))
			{
				int newId = b.id + 1;
				while (integerBlockMapping.containsKey(newId))
					newId++;
				
				if (newId <= 20)
					integerBlockMapping.put(newId, b);
			}
			else
				integerBlockMapping.put(b.id, b);
		}
		
		
		JSONArray blocksMeta = (JSONArray)(blocksMetadataJson.get("Blocks"));
		
		for (Object blockMetaJson : blocksMeta)
		{
			int id = ((Long)((JSONObject)blockMetaJson).get("ID")).intValue();
			if (integerBlockMapping.containsKey(id))
				integerBlockMapping.get(id).label = (String)((JSONObject)blockMetaJson).get("Name");
		}
		
		pointedTo = null;
		currentScene = this;
	}
	
	public String describeScene()
	{
		Predicate above = new Predicate(PredicateType.ABOVE);
		Predicate below = new Predicate(PredicateType.BELOW);
		Predicate onGround = new Predicate(PredicateType.ONGROUND);
		Predicate nextTo = new Predicate(PredicateType.NEXTTO);
		Predicate touching = new Predicate(PredicateType.TOUCHING);
		
		StringBuilder resultString = new StringBuilder();
		
		for (Block b : integerBlockMapping.values())
		{
			if (onGround.evaluate(b))
				resultString.append("Block " + b.label + " is on the table.\n");
		}
		
		List<Block> blockList = new ArrayList<Block>(integerBlockMapping.values());
		for (int i = 0; i < blockList.size(); i++)
		{
			for (int j = i + 1; j < blockList.size(); j++)
			{
				Block b1 = blockList.get(i);
				Block b2 = blockList.get(j);
				if (above.evaluate(b1,b2))
					resultString.append("Block " + b1.label + " is above the " + b2.label + " block.\n");
				if (below.evaluate(b1,b2))
					resultString.append("Block " + b1.label + " is below the " + b2.label + " block.\n");
				if (nextTo.evaluate(b1,b2))
					resultString.append("Block " + b1.label + " is next to the " + b2.label + " block.\n");
				if (touching.evaluate(b1,b2))
					resultString.append("Block " + b1.label + " is touching the " + b2.label + " block.\n");
				
			}
		}
		
		return resultString.toString();
	}
	
	public boolean checkPredicate(Predicate p)
	{
		List<Block> blockList = new ArrayList<Block>(integerBlockMapping.values());
		for (int i = 0; i < blockList.size(); i++)
		{
			for (int j = 0; j < blockList.size(); j++)
			{
				if (i == j)
					continue;
				Block b1 = blockList.get(i);
				Block b2 = blockList.get(j);
				if (p.evaluate(b1,b2))
					return true;
			}
		}
		
		return false;
	}
	
	public boolean checkPredicates(Collection<Predicate> predicates)
	{
		List<Block> blockList = new ArrayList<Block>(integerBlockMapping.values());
		for (int i = 0; i < blockList.size(); i++)
		{
			for (int j = 0; j < blockList.size(); j++)
			{
				if (i == j)
					continue;
				Block b1 = blockList.get(i);
				Block b2 = blockList.get(j);
				boolean satisfied = true;
				for (Predicate p : predicates)
				{
					if (!p.evaluate(b1,b2))
						satisfied = false;
				}
				if (satisfied)
					return true;
			}
		}
		
		return false;
	}
	
	public boolean isSimilarTo(Scene s)
	{
		if (!s.integerBlockMapping.keySet().equals(integerBlockMapping.keySet()))
			return false;
		
		for (int blockId : integerBlockMapping.keySet())
		{
			Block a = integerBlockMapping.get(blockId);
			Block b = s.integerBlockMapping.get(blockId);
			if (a == null || b == null)
				return false;
			if (!a.isSimilarTo(b))
				return false;
		}
		
		return true;
	}
	
	public KQMLList getKQMLRepresentation()
	{

		KQMLList blocks = new KQMLList();
		KQMLList sceneDescription = new KQMLList();
		
		for (Block b : integerBlockMapping.values())
		{
			blocks.add(b.getKQMLRepresentation());
		}
		
		sceneDescription.add("SCENE-DESCRIPTION");
		sceneDescription.add(":TIMESTAMP");
		sceneDescription.add("\"" + timestamp + "\"");
		sceneDescription.add(":IS-STABLE");
		sceneDescription.add("T");
		sceneDescription.add(":OBJECTS");
		sceneDescription.add(blocks);
		
		return sceneDescription;
	}
}
