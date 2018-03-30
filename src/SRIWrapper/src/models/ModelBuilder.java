package models;

import features.CountFeature;
import features.DistanceFeature;
import features.Feature;
import features.FeatureConstants;
import goals.Goal;
import goals.GoalMessages;
import goals.GoalStateHandler;
import goals.GoalStateHandler.SystemState;
import TRIPS.KQML.*;

import java.util.*;

import utilities.KQMLUtilities;
import utilities.TextToSpeech;

public class ModelBuilder {

	public StructureModel currentModel;
	private String lastModelName;
	private Map<String,ModelInstantiation> modelInstantiations;
	private Feature lastFeatureAsked;
	boolean receivedAssertion = false;
	boolean askedFeature = false;
	String[] numbers = {"zero","one","two","three","four","five","six"};

	
	int numBlocks = -1;
	
	public ModelBuilder() {
		currentModel = null;
		modelInstantiations = new HashMap<String,ModelInstantiation>();
		lastModelName = null;
		lastFeatureAsked = null;
	}
	
	public String answerQuery(KQMLList query)
	{
		return null;
	}
	
	public String getLastModelName()
	{
		return lastModelName;
	}
	
	
	
	public String askAboutModel(Constraint constraintToAsk)
	{
		
		StringBuilder sb = new StringBuilder();
		String featureName = constraintToAsk.getFeature().getPrettyName();
		sb.append("What is the greatest the ");
		sb.append(featureName);
		
		if (constraintToAsk instanceof StructureConstraint)
		{
			ReferringExpression re = ((StructureConstraint)constraintToAsk).getSubject();
			sb.append(" of the ");
			sb.append(re);
		}
	
		sb.append(" can be?");
		return sb.toString();
	}
	
	

	public Goal whatNext(SystemState state, Goal currentGoal)
	{
		if (state == SystemState.LEARNING_DEMONSTRATION)
		{
			TextToSpeech.say("Show me an example.");
			return new Goal("SHOW-EXAMPLE");
		}
		
		if (state == SystemState.LEARNING_CONSTRAINTS)
		{
			if (!getLastModelInstantiation().isIntroduced())
			{
				TextToSpeech.say("Can you describe the structure?");
				return new Goal("ONT::DESCRIBE");
			}
			
			if (!askedFeature)
			{
				Constraint constraintToAsk = getLastModelInstantiation().getConstraintToAsk();
				TextToSpeech.say(askAboutModel(constraintToAsk));
				askedFeature = true;
				return getGoalToAskAboutModel(currentGoal);			
			}
			
			TextToSpeech.say("Can you tell me anything else about it?");
			askedFeature = false;
			return new Goal("ONT::DESCRIBE");
		}
		
		return null;
	}
	
	private Goal getGoalToAskAboutModel(Goal currentGoal)
	{
		
		KQMLList currentTerm = currentGoal.getTerm();
		String questionVariable = "WH" + GoalStateHandler.getNextId();
		String queryVariable = "Q" + GoalStateHandler.getNextId();
		String queryId = "QI" + GoalStateHandler.getNextId();
		KQMLList newTerm = new KQMLList();
		newTerm.addAll(currentTerm);
		
		
		if (newTerm.getKeywordArg(":AFFECTED-RESULT") != null)
			newTerm.removeKeywordArg(":AFFECTED-RESULT");
		newTerm.add(":AFFECTED-RESULT");
		newTerm.add(questionVariable);
		
		Goal newQuery = new Goal(queryVariable,queryId,newTerm);
		newQuery.updateTerm();
		//newQuery.addContext(queryId);
//		addGoal(newQuery);
//		newContext.addAll(context);
//		newContext.add(newQuery.getTerm());
		
		return newQuery;
		
//		KQMLList askWhContent = GoalMessages.askWhAdoptContent(queryId, 
//				questionVariable, queryVariable, currentGoal.getId());
//		
//		return GoalMessages.propose(askWhContent,newContext);
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
	
	public String processNewModel(KQMLList term, KQMLList context)
	{
		String variable = term.get(KQMLUtilities.VARIABLE).stringValue();
		String modelBaseName = "structure";
		if (term.getKeywordArg(":LEX") != null)
			modelBaseName = KQMLUtilities.cleanLex(
					term.getKeywordArg(":LEX").stringValue());
		
		String modifierLex = "";
		if (term.getKeywordArg(":MOD") != null)
		{
			KQMLList modifier = KQMLUtilities.findTermInKQMLList(variable, context);
			if (modifier != null && modifier.getKeywordArg(":LEX") != null)
				modifierLex = KQMLUtilities.cleanLex(
									modifier.getKeywordArg(":LEX").stringValue()) + "-";
				
		}
		String newModelName = modifierLex + modelBaseName;
		return processNewModel(newModelName);
	}
	
	public String processNewModel(String modelName)
	{
		currentModel = new StructureModel(modelName);
		ModelInstantiation mi = new ModelInstantiation();
		lastModelName = modelName;
		modelInstantiations.put(modelName, mi);
		return modelName;
		
	}
	
	public ModelInstantiation getLastModelInstantiation()
	{
		return getModelInstantiation(lastModelName);
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
