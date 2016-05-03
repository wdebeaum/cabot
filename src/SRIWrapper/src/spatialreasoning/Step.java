package spatialreasoning;

import java.util.ArrayList;
import java.util.Map;

import environment.Block;
import environment.Scene;
import utilities.TextToSpeech;

public class Step {

	
	String action;
	String[] arguments;
	
	public Step(String action, String[] arguments)
	{
		this.action = action;
		this.arguments = arguments;
	}
	
	public Step(String action, String argument)
	{
		this.action = action;
		this.arguments = new String[]{argument};
	}
	
	public boolean execute()
	{
		Scene currentScene = Scene.currentScene;
		Map<Integer, Block> blockMapping = currentScene.integerBlockMapping;
		switch (action)
		{
		case "say":
			TextToSpeech.say(arguments[0]);
			return true;
		case "checkblock":
			Predicate p1 = new Predicate(PredicateType.valueOf(arguments[0]));

			if (!blockMapping.containsKey(Integer.parseInt(arguments[1])))
				return false;
			Block b1 = blockMapping.get(Integer.parseInt(arguments[1]));
			if (arguments.length <= 2)
				return p1.evaluate(b1);
			
			if (!blockMapping.containsKey(Integer.parseInt(arguments[2])))
				return false;
			
			Block b2 = blockMapping.get(Integer.parseInt(arguments[2]));
			
			return p1.evaluate(b1,b2);
		case "checkpredicate":
			Predicate p2 = new Predicate(PredicateType.valueOf(arguments[0]));
			int number = Integer.parseInt(arguments[1]);
			int count = 0;
			for (Block b : blockMapping.values())
			{
				if (p2.evaluate(b))
					count++;
			}
			return (count == number);

		case "checkpredicates":
			ArrayList<Predicate> predicateList = new ArrayList<Predicate>();
			predicateList.add(new Predicate(PredicateType.valueOf(arguments[0])));
			predicateList.add(new Predicate(PredicateType.valueOf(arguments[1])));
			return currentScene.checkPredicates(predicateList);
		default:
			return false;
				
		}
	}
}
