package models;

import features.CountFeature;
import features.DistanceFeature;
import features.Feature;
import goals.Goal;
import goals.GoalStateHandler.SystemState;
import TRIPS.KQML.*;

import java.util.*;

import utilities.TextToSpeech;

public class ModelBuilder {

	public StructureModel currentModel;
	private Map<String,ModelInstantiation> modelInstantiations;
	String[] numbers = {"zero","one","two","three","four","five","six"};
	int numBlocks = -1;
	
	public ModelBuilder() {
		currentModel = null;
		modelInstantiations = new HashMap<String,ModelInstantiation>();
	}
	
	public String answerQuery(KQMLList query)
	{
		return null;
	}
	
	public String askAboutModel()
	{
		return null;
	}

	public Goal whatNext(SystemState state)
	{
		if (state == SystemState.LEARNING_DEMONSTRATION)
		{
			TextToSpeech.say("Show me an example.");
			return new Goal("SHOW-EXAMPLE");
		}
		
		if (state == SystemState.LEARNING_CONSTRAINTS)
		{
			return new Goal("ONT::TEACH-TRAIN");
		}
		
		return null;
	}
	
	public void processUtterance(String goal, KQMLList definition)
	{
		if (currentModel == null)
			currentModel = new StructureModel("unnamed");
		
		ModelInstantiation mi = new ModelInstantiation();
		
		modelInstantiations.put(goal, mi);
	}
	
	public void processAssertion(KQMLList content, KQMLList context)
	{
		
	}
	
	public String processNewModel(String modelName)
	{
		currentModel = new StructureModel(modelName);
		ModelInstantiation mi = new ModelInstantiation();
		
		modelInstantiations.put(modelName, mi);
		return null;
		
	}
	
	public boolean hasModel(String modelName)
	{
		return modelInstantiations.containsKey(modelName);
	}
	
	public ModelInstantiation getModelInstantiation(String modelName)
	{
		return modelInstantiations.get(modelName);
	}

}
