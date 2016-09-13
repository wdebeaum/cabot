package spatialreasoning;

import java.io.IOException;
import java.util.*;

import org.jblas.DoubleMatrix;

import TRIPS.KQML.KQMLList;
import messages.BlockMessageSender;
import environment.Block;
import environment.Scene;
import environment.StructureInstance;
import features.*;
import utilities.FormatConversion;
import utilities.KQMLUtilities;
import utilities.TextToSpeech;

public class Step {

	
	String action;
	String[] arguments;
	String modelDescription;
	String lastUtterance;
	KQMLList lastKQMLUtterance;
	int lastUtteranceNumber;
	boolean sentCommand;
	Block sentBlock;
	StructureInstance lastStructureInstance;
	
	public Step(String action, String[] arguments)
	{
		this.action = action;
		this.arguments = arguments;
		lastUtteranceNumber = 0;
		lastUtterance = null;
		sentCommand = false;
		sentBlock = null;
		lastStructureInstance = null;
	}
	
	public Step(String action, String argument)
	{
		this.action = action;
		this.arguments = new String[]{argument};
		lastUtteranceNumber = 0;
		lastUtterance = null;
		sentCommand = false;
		sentBlock = null;
	}
	
	public static Step fromKQML(String goalName, KQMLList context)
	{
		KQMLList term = KQMLUtilities.findTermInKQMLList(goalName, context);
		return null;
	}
	
	// Execute a step, returning true if it's completed
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
		case "querymodeldefinition":
			TextToSpeech.say("What is a " + arguments[0] + "?");
			return true;
		case "checkmodel":
			return true;
		case "getresponse":
			if (lastUtterance == null)
				return false;
			return true;
		case "placeblock":
			if (arguments[0].equals("absolute"))
				sentBlock = new Block(arguments[1]);
			else if (arguments[0].equals("relative"))
			{
				StructureInstance si = Scene.currentScene.getWholeStructureInstance("test");
				DoubleMatrix positionChange = FormatConversion.stringToDoubleMatrix(arguments[1]);
				DoubleMatrix newPosition = si.getAveragePosition().add(positionChange);
				sentBlock = new Block(newPosition);
			}
			else if (arguments[0].equalsIgnoreCase("linear"))
			{
				StructureInstance currentInstance = Scene.currentScene.getWholeStructureInstance("row");
				TemporalSequenceFeature tsf = (TemporalSequenceFeature) currentInstance.getFeature("sequence");
				DirectionFeature xDirection = new DirectionFeature("xdirection");
				xDirection.setValue(new DoubleMatrix(new double[]{1,0,0}));
				TemporalSequenceFeature sortedTsf = new TemporalSequenceFeature("sortedsequence");
				sortedTsf.setSequence(new ArrayList<FeatureGroup>(tsf.sortedPositions(xDirection)));
				FeatureGroup nextFeatureGroup = sortedTsf.getNextInDirection();
				BlockFeatureGroup bfg = (BlockFeatureGroup) nextFeatureGroup;
				sentBlock = bfg.block;
			}
			else if (arguments[0].equalsIgnoreCase("linearY"))
			{
				
				TemporalSequenceFeature tsf = (TemporalSequenceFeature) lastStructureInstance.getFeature("sequence");
				DirectionFeature xDirection = new DirectionFeature("xdirection");
				xDirection.setValue(new DoubleMatrix(new double[]{1,0,0}));
				TemporalSequenceFeature sortedTsf = new TemporalSequenceFeature("sortedsequence");
				sortedTsf.setSequence(new ArrayList<FeatureGroup>(tsf.sortedPositions(xDirection)));
				
				DirectionFeature zDirection = new DirectionFeature("zdirection");
				xDirection.setValue(new DoubleMatrix(new double[]{0,0,1}));
				TemporalSequenceFeature verticalTsf = sortedTsf.projectOnto(xDirection, zDirection);
				FeatureGroup nextFeatureGroup = (FeatureGroup) verticalTsf.getValue().get(Integer.parseInt(arguments[1]));
				BlockFeatureGroup bfg = (BlockFeatureGroup) nextFeatureGroup;
				sentBlock = bfg.block;
			}
			// Check if a block is in that place, then complete the placement step if it is
			for (Block b : currentScene.integerBlockMapping.values())
			{
				if (b.hasSimilarPosition(sentBlock))
					return true;
			}
			
			if (!sentCommand)
			{
				System.out.println("Haven't sent command");
				//BlockMessageSender bms = new BlockMessageSender();
				sentCommand = true;
				try {
					BlockMessageSender.sendPostRequest(sentBlock);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				currentScene.setProxyBlock(sentBlock);
				
			}

			return false;
		default:
			return false;
				
		}
	}
	
	public void setUtterance(String utterance)
	{
		this.lastUtterance = utterance;
	}

}
