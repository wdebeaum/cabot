package models;

import features.CountFeature;
import features.DistanceFeature;
import features.Feature;
import features.FeatureConstants;
import goals.Goal;
import goals.GoalMessages;
import goals.GoalStateHandler;
import goals.GoalStateHandler.SystemState;
import goals.Query;
import messages.BlockMessageSender;
import messages.EvaluateHandler;
import messages.FeatureParser;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import TRIPS.KQML.*;
import environment.Block;

import java.io.IOException;
import java.util.*;

import utilities.KQMLUtilities;
import utilities.TextToSpeech;

public class ModelBuilder {

	public StructureModel currentModel;
	private String lastModelName;
	private Map<String,ModelInstantiation> modelInstantiations;
	private Feature lastFeatureAsked;
	private Constraint lastConstraintAsked;
	private Goal lastGoal;
	boolean receivedAssertion = false;
	boolean askedFeature = false;
	boolean showedExample = false;
	boolean showedUserExample = false;
	int constraintsReceived = 0;
	String[] numbers = {"zero","one","two","three","four","five","six"};

	
	int numBlocks = -1;
	
	public ModelBuilder() {
		currentModel = null;
		modelInstantiations = new HashMap<String,ModelInstantiation>();
		lastModelName = null;
		lastFeatureAsked = null;
		lastGoal = null;
		lastConstraintAsked = null;
	}
	
	public String answerQuery(KQMLList query)
	{
		return null;
	}
	
	/**
	 * After the system has asked a question, this function interprets the answer using
	 * the context (the form of the constraint) of the original question
	 * @param content
	 * @param context
	 * @return
	 */
	public KQMLList answerCurrentConstraint(KQMLList content, KQMLList context)
	{
		if (lastConstraintAsked == null)
			return EvaluateHandler.unacceptableContent("CANNOT-PROCESS", "ANSWER", 
					content.getKeywordArg(":TO").stringValue(), "NIL", content.getKeywordArg(":AS"));
		
		if (lastConstraintAsked instanceof PredicateConstraint)
		{
			List<Predicate> predicates = PredicateParser.extractPredicatesFromAnswer(context);
			
			if (predicates.isEmpty())
			{
				TextToSpeech.say("Hmm, I don't quite understand.");
				return EvaluateHandler.unacceptableContent("CANNOT-PROCESS", "ANSWER", 
						content.getKeywordArg(":TO").stringValue(), "NIL", content.getKeywordArg(":AS"));
			}
			
			// TODO: Improve to take multiple predicates
			((PredicateConstraint)lastConstraintAsked).setPredicate(predicates.get(0));
			getLastModelInstantiation().addConstraint(lastConstraintAsked);
			
			TextToSpeech.say("Okay, got it.");
			return EvaluateHandler.acceptableEffectContent(content, new KQMLList(), context);
		}
		
		KQMLObject valueObject = content.getKeywordArg(":VALUE");
		if (valueObject == null)
			return EvaluateHandler.unacceptableContent("CANNOT-PROCESS", "ANSWER", 
					content.getKeywordArg(":ID").stringValue(), "NIL", content.getKeywordArg(":AS"));

		double value;
			
		
		try 
		{
			value = FeatureParser.extractDoubleValue(content, context);
		}
		catch(NumberFormatException nfe)
		{
			KQMLList valueList = KQMLUtilities.findTermInKQMLList(valueObject.stringValue(),
																context);
			if (valueList != null && valueList.getKeywordArg(":QUAN") != null)
			{
				value = 10;
				
			}
			else
			{
				TextToSpeech.say("Hmm, I don't quite understand.");
				return EvaluateHandler.unacceptableAnswerContent(content);
			}
		}
		System.out.println("Value from response: " + value);
		lastConstraintAsked.setValue(value);
		
		if (getLastModelInstantiation() == null)
			return EvaluateHandler.unacceptableAnswerContent(content);	
		
		getLastModelInstantiation().addConstraint(lastConstraintAsked);
		
		System.out.println("Filled constraint: ");
		System.out.println(lastConstraintAsked);
		
		lastConstraintAsked = null;
		TextToSpeech.say("Okay, got it.");
		constraintsReceived++;
		
		return EvaluateHandler.acceptableEffectContent(content, new KQMLList(), context);
		
	}
	
	public String getLastModelName()
	{
		return lastModelName;
	}
	


	
	public String askAboutModel(Constraint constraintToAsk)
	{
		
		StringBuilder sb = new StringBuilder();
		String featureName;
		if (constraintToAsk instanceof PredicateConstraint)
			featureName = "";
		else
			featureName = constraintToAsk.getFeature().getPrettyName();
		
		if (getLastModelInstantiation().lastUnresolvedObjectReference != null)
		{
			// Add something here?
		}
		
		lastConstraintAsked = constraintToAsk;
		if (featureName.equals(FeatureConstants.NUMBER))
			sb.append("What is the greatest number of blocks that can be in the ");
		else if (constraintToAsk instanceof PredicateConstraint)
		{
			sb.append("Where can the ");
			sb.append(((PredicateConstraint) constraintToAsk).getSubject());
			sb.append(" be?");
			return sb.toString();
		}
		else
		{
			sb.append("What is the greatest the ");
			sb.append(featureName);
		}
		
		if (constraintToAsk instanceof StructureConstraint)
		{
			
			ReferringExpression re = ((StructureConstraint)constraintToAsk).getSubject();
			if (featureName.equals(FeatureConstants.NUMBER))
				sb.append(re);
			else
			{
				sb.append(" of the ");
				sb.append(re);
			}
		}
	
		if (!featureName.equals(FeatureConstants.NUMBER))
			sb.append(" can be?");
		return sb.toString();
	}
	
	

	public KQMLList whatNext(SystemState state, Goal currentGoal)
	{
		if (state == SystemState.LEARNING_DEMONSTRATION)
		{
			return getShowExampleGoal(currentGoal);
		}
		
		if (state == SystemState.LEARNING_CONSTRAINTS)
		{
			if (!getLastModelInstantiation().isIntroduced())
			{
				TextToSpeech.say("Can you tell me something about the structure?");
				lastGoal = new Goal("ONT::DESCRIBE");
				lastGoal.setParent(currentGoal);
				return GoalMessages.proposeAdoptContent(lastGoal);
			}
			

			
			if (!askedFeature)
			{
				Constraint constraintToAsk = getLastModelInstantiation().getConstraintToAsk();
				
				if (constraintToAsk == null && !showedExample)
				{
					showedExample = true;
					return getShowExampleGoal(currentGoal);
				}
				
				if (constraintToAsk != null)
				{
					
					TextToSpeech.say(askAboutModel(constraintToAsk));
					askedFeature = true;
					Query query = getQueryToAskAboutModel(currentGoal, constraintToAsk);
					query.setParent(currentGoal);
					lastGoal = query;
					return GoalMessages.proposeAskWhContent(query);		
				}
			}
			
			if (!showedExample)
			{
				showedExample = true;
				return getShowExampleGoal(currentGoal);
			}
			
			if (!showedUserExample)
			{
				KQMLList result = askAboutExample(currentGoal);
				showedUserExample = true;
				if (result != null)
				{
	
					return result; 
				}
			}

			
			TextToSpeech.say("Can you tell me anything else about it?");
			askedFeature = false;
			lastGoal = new Goal("ONT::DESCRIBE");
			lastGoal.setParent(currentGoal);
			return GoalMessages.proposeAdoptContent(lastGoal);
		}
		
		return null;
	}
	
	private KQMLList getShowExampleGoal(Goal currentGoal)
	{
		TextToSpeech.say("Can you show me an example?");
		lastGoal = new Goal("SHOW-EXAMPLE");
		lastGoal.setParent(currentGoal);
		return GoalMessages.proposeAdoptContent(lastGoal);
	}
	
	public KQMLList askAboutExample(Goal currentGoal)
	{
		GridModel2D satisfiedExample = generateExample();
		
		if (satisfiedExample == null)
			return null;
		
		TextToSpeech.say("Is this a correct example?");

		try {
			BlockMessageSender.sendPostRequest(satisfiedExample.getBlocks());
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}

		Query q = getQueryToAskAboutExample(currentGoal);
		
		lastGoal = q;
		lastGoal.setParent(currentGoal);
		
		return GoalMessages.proposeAskIfContent(q);
		
	}
	
	public GridModel2D generateExample()
	{
		System.out.println("Creating random instances to test");
		for (int i = 0; i < 300; i++)
		{
			GridModel2D currentGridModel = GridModel2D.randomSample(6);
			if (getLastModelInstantiation().testModelOnParticularStructureInstanceNoDebug(
					currentGridModel.getBlocks()))
			{
				return currentGridModel;
			}
		}
		
		return null;
	}
	
	private Query getQueryToAskAboutExample(Goal currentGoal)
	{
		String questionVariable = "Q" + GoalStateHandler.getNextId();

		KQMLList newTerm = new KQMLList();
		newTerm.add("ONT::RELN");
		newTerm.add(questionVariable);
		newTerm.add(":INSTANCE-OF");
		newTerm.add("ONT::STRUCTURE");
		
		
		Query newQuery = new Query();
		//newQuery.updateTerm();
		lastGoal = newQuery;
		newQuery.setParent(currentGoal);
		newQuery.addContextElement(newTerm);
		newQuery.addQuery(questionVariable, "");

		return newQuery;		
	}
	
	private Query getQueryToAskAboutModel(Goal currentGoal, Constraint constraint)
	{
		
		String questionVariable = "Q" + GoalStateHandler.getNextId();
		String whatVariable = "WH" + GoalStateHandler.getNextId();
		String featureName;
		if (constraint instanceof PredicateConstraint)
			featureName = "ONT::LOCATION";
		else
			featureName = constraint.getFeature().getName();
		
		KQMLList newTerm = new KQMLList();
		newTerm.add("ONT::RELN");
		newTerm.add(questionVariable);
		newTerm.add(":INSTANCE-OF");
		newTerm.add(featureName);
		
		KQMLList whatTerm = new KQMLList();
		whatTerm.add("ONT::WH-TERM");
		whatTerm.add(":INSTANCE-OF");
		whatTerm.add("ONT::REFERENTIAL-SEM");
		whatTerm.add(whatVariable);
		
		
		Query newQuery = new Query(whatVariable,"QI" + GoalStateHandler.getNextId(),new KQMLList());
		//newQuery.updateTerm();
		lastGoal = newQuery;
		newQuery.setParent(currentGoal);
		newQuery.addQuery(featureName, questionVariable);
		newQuery.addContextElement(newTerm);
		newQuery.addContextElement(whatTerm);

		return newQuery;
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
		constraintsReceived++;
	}
	
	public String processNewModel(KQMLList term, KQMLList context)
	{
		String variable = term.get(KQMLUtilities.VARIABLE).stringValue();
		String modelBaseName = "structure";
		if (term.getKeywordArg(":LEX") != null && 
				!term.getKeywordArg(":LEX").stringValue().equalsIgnoreCase("W::SHAPE"))
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

	public Goal getLastGoal() {
		return lastGoal;
	}
	
	public Constraint getLastConstraintAsked()
	{
		return lastConstraintAsked;
	}
	

}
