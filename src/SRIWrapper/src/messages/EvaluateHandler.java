package messages;

import java.io.IOException;
import java.util.List;

import utilities.KQMLUtilities;
import utilities.TextToSpeech;
import environment.Scene;
import environment.StructureInstance;
import features.BlockFeatureGroup;
import features.TemporalSequenceFeature;
import goals.Goal;
import goals.GoalStateHandler;
import goals.GoalStateHandler.SystemState;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;
import models.FeatureConstraint;
import models.FeatureProjection;
import models.ModelBuilder;
import models.ModelInstantiation;

public class EvaluateHandler {

	ModelBuilder modelBuilder;
	GoalStateHandler goalStateHandler;
	boolean teachingMode;
	private static int nextId = 1;
	
	public EvaluateHandler(GoalStateHandler gsh, ModelBuilder mb) {
		this.goalStateHandler = gsh;
		this.modelBuilder = mb;
		
		teachingMode = false;
	}
	
	private static int getNextId()
	{
		int toReturn = nextId;
		nextId++;
		return nextId;
	}

	public KQMLList handleAskIfMessage(KQMLList content, KQMLList context)
	{
		String eventLFSymbol = content.getKeywordArg(":QUERY").stringValue();
		KQMLList eventLF = KQMLUtilities.findTermInKQMLList(eventLFSymbol, context);
		//String eventToQueryID = goalLF.getKeywordArg(":NEUTRAL").stringValue();
		//KQMLList eventToQuery = KQMLUtilities.findTermInKQMLList(eventToQueryID, context);
		String modelTermID = eventLF.getKeywordArg(":NEUTRAL1").stringValue();
		KQMLList modelTerm = KQMLUtilities.findTermInKQMLList(modelTermID, context);
		//String modelName = modelTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		String modelName = modelTerm.getKeywordArg(":LEX").stringValue();
		//steps.add(new Step("checkmodel", cleanObjectToMake));
		if (modelBuilder.getModelInstantiation(modelName) == null)
		{
			TextToSpeech.say("I don't know what a " + KQMLUtilities.cleanLex(modelName) + 
					" is.");
		}
		
		if (Scene.currentScene == null || Scene.currentScene.integerBlockMapping == null)
		{
			TextToSpeech.say("I can't see anything to tell if it's a " +
								KQMLUtilities.cleanLex(modelName) + ".");
			return unacceptableContent("CANNOT-PROCESS", "ASK-IF", 
									content.getKeywordArg(":ID").stringValue(),
									"NIL", content.getKeywordArg(":AS"));
		}
		boolean satisfied = modelBuilder.getModelInstantiation(modelName)
					.testModelOnStructureInstance(Scene.currentScene.integerBlockMapping.values());
		
		StringBuilder response = new StringBuilder();
		
		if (modelBuilder.getModelInstantiation(modelName).constraints.isEmpty())
		{
			TextToSpeech.say("Yes, because I didn't get any constraints for the model of a " + 
								KQMLUtilities.cleanLex(modelName));
			return answerContent(true,context);
		}
		
		if (satisfied)
			response.append("Yes because ");
		else
			response.append("No because ");
		
		int reasonNumber = 0;
		for (FeatureConstraint fc : modelBuilder.getModelInstantiation(modelName).constraints)
		{
			if (reasonNumber > 0)
			{
				if (satisfied)
				{
					response.append(" and ");
				}
				else {
					if (fc.isSatisfied())
						response.append(" even though ");
					else
						response.append(" but ");
				}
			}
			response.append("the ");
			response.append(fc.reason());
			reasonNumber++;
		}
		response.append(".");
		TextToSpeech.say(response.toString());
		
		return answerContent(satisfied,context);
	}
	
	public KQMLList handleAskWhMessage(KQMLList content, KQMLList context)
	{
		String eventLFSymbol = content.getKeywordArg(":QUERY").stringValue();
		KQMLList eventLF = KQMLUtilities.findTermInKQMLList(eventLFSymbol, context);
		
		String propertySymbol = eventLF.getKeywordArg(":NEUTRAL").stringValue();
		KQMLList propertyTerm = KQMLUtilities.findTermInKQMLList(propertySymbol, context);
		String property = propertyTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		KQMLObject structureReferenceObject = propertyTerm.getKeywordArg(":FIGURE");
		String structureReferenceSymbol = null;
		KQMLList structureReferenceTerm = null;
		
		if (structureReferenceObject != null)
			structureReferenceSymbol = structureReferenceObject.stringValue();
		
		if (structureReferenceSymbol != null)
			structureReferenceTerm = 
				KQMLUtilities.findTermInKQMLList(structureReferenceSymbol, context);
		
		/*if (modelBuilder(modelName) == null)
		{
			TextToSpeech.say("I don't know what a " + KQMLUtilities.cleanLex(modelName) + 
					" is.");
		}*/
		
		if (Scene.currentScene == null || Scene.currentScene.integerBlockMapping == null)
		{
			TextToSpeech.say("I can't see anything to tell what its " +
								KQMLUtilities.cleanLex(property) + " is.");
			return unacceptableContent("CANNOT-PROCESS", "ASK-WH", 
									content.getKeywordArg(":ID").stringValue(),
									"NIL", content.getKeywordArg(":AS"));
		}
		
		StructureInstance structure = Scene.currentScene.getWholeStructureInstance("temp");
		String result = structure.getFeature(property).toString();
		
		TextToSpeech.say("The " + KQMLUtilities.cleanConcept(property) + " is " + result + "blocks");
		
		return answerContent(result,context);
	}
	
	public KQMLList handleYesNoAnswerMessage(KQMLList content, KQMLList context)
	{
		// Only handles "are you done?" right now
		String value = content.getKeywordArg(":VALUE").toString();
		
		String queryVariable = content.getKeywordArg(":QUERY").stringValue();
		KQMLList queryTerm = KQMLUtilities.findTermInKQMLList(queryVariable, context);
		String instanceOf = queryTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		if (instanceOf.equals("ONT::FINISHED"))
		{
			if (value.equals("ONT::TRUE"))
			{
				
				Goal goalToAnswer = goalStateHandler.getCurrentGoal();
				Goal newGoal = goalToAnswer.returnModifiedWithAskIfAnswer(content, context);
				goalStateHandler.addGoal(newGoal);
				newGoal.setCompleted();
				KQMLList effect = new KQMLList();
				effect.add("ADOPT");
				effect.add(":ID");
				effect.add(newGoal.getId());
				effect.add(":WHAT");
				effect.add(newGoal.getWhat());
				effect.add(":AS");
				KQMLList asList = new KQMLList();
				asList.add("MODIFICATION");
				asList.add(":OF");
				asList.add(goalToAnswer.getId());
				
				effect.add(asList);
				
				KQMLList newContext = new KQMLList();
				newContext.addAll(context);
				newContext.add(newGoal.getTerm());
				// Hack
				goalStateHandler.userCompleted = true;
				
				return acceptableEffectContent(content,effect, newContext);
				//return goalStateHandler.done(goalStateHandler.getCurrentGoal().getId());
			}
			else if (value.equals("ONT::FALSE"))
			{
				return goalStateHandler.waitingForUser(goalStateHandler.getCurrentGoal().getId());
			}
		}
		
		return failureContent("FAILED-TO-INTERPRET", "NIL",
				new KQMLList(new KQMLToken("MISSING-GOAL")), new KQMLList());
		
	}
	
	public KQMLList handleAnswerMessage(KQMLList content, KQMLList context)
	{
		// This is probably handling "use red blocks" or something, but needs
		// to handle yes/no answers as well
		if (content.getKeywordArg(":QUERY") == null)
		{
			return failureContent("FAILED-TO-INTERPRET", "NIL",
					new KQMLList(new KQMLToken("MISSING-GOAL")), new KQMLList());
		}
		
		KQMLObject value = content.getKeywordArg(":VALUE");
		
		if (value != null && (value.toString().equalsIgnoreCase("ONT::TRUE") || 
				value.toString().equalsIgnoreCase("ONT::FALSE")))
			return handleYesNoAnswerMessage(content, context);
		
			
		String queryVariable = content.getKeywordArg(":QUERY").stringValue();
		if (goalStateHandler.getGoal(queryVariable) == null)
		{
			return failureContent("FAILED-TO-INTERPRET", queryVariable,
					new KQMLList(new KQMLToken("MISSING-GOAL")), new KQMLList());			
		}
		

		KQMLList queryTerm = KQMLUtilities.findTermInKQMLList(queryVariable, context);
		
		String instanceOf = queryTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		
		if (instanceOf.equals("ONT::BLOCKS"))
		{
			Goal goalToAnswer = goalStateHandler.getCurrentGoal();
			Goal newGoal = goalToAnswer.returnModifiedWithAnswer(content, context);
			goalStateHandler.addGoal(newGoal);
			KQMLList effect = new KQMLList();
			effect.add("ADOPT");
			effect.add(":ID");
			effect.add(newGoal.getId());
			effect.add(":WHAT");
			effect.add(newGoal.getWhat());
			effect.add(":AS");
			KQMLList asList = new KQMLList();
			asList.add("MODIFICATION");
			asList.add(":OF");
			asList.add(goalToAnswer.getId());
			
			effect.add(asList);
			
			KQMLList newContext = new KQMLList();
			newContext.addAll(context);
			newContext.add(newGoal.getTerm());
			//return acceptableEffectContent(content,effect,newContext);
			return null;
		}
		else
		{
			Goal goalToAnswer = goalStateHandler.getCurrentGoal();
			Goal newGoal = goalToAnswer.returnModifiedWithAnswer(content, context);
			goalStateHandler.addGoal(newGoal);
			KQMLList effect = new KQMLList();
			effect.add("ADOPT");
			effect.add(":ID");
			effect.add(newGoal.getId());
			effect.add(":WHAT");
			effect.add(newGoal.getWhat());
			effect.add(":AS");
			KQMLList asList = new KQMLList();
			asList.add("SUBGOAL");
			asList.add(":OF");
			asList.add(goalToAnswer.getId());
			
			effect.add(asList);
			
			KQMLList newContext = new KQMLList();
			newContext.addAll(context);
			newContext.add(newGoal.getTerm());
			
			return acceptableEffectContent(content,effect, newContext);
		}
		
	}
	
	public KQMLList handleAssertionMessage(KQMLList content, KQMLList context)
	{
		String goal = content.getKeywordArg(":WHAT").stringValue();
		String id = content.getKeywordArg(":ID").stringValue();
		KQMLObject asObject = content.getKeywordArg(":AS");
		KQMLList goalLF = KQMLUtilities.findTermInKQMLList(goal, context);
		Goal assertionGoal = new Goal(goal,id,goalLF);
		goalStateHandler.addGoal(assertionGoal);
		KQMLList assertions = (KQMLList)goalLF.getKeywordArg(":EVENTS");
		
		String assertedObjectName = null;
		
		if (modelBuilder.currentModel != null)
			assertedObjectName = modelBuilder.currentModel.name;
		
		for (KQMLObject assertionObject : assertions)
		{
			KQMLList assertionContentTerm = KQMLUtilities.findTermInKQMLList(assertionObject.stringValue(), context);
			
			if (assertionContentTerm == null)
				continue;
			
			if (assertionContentTerm.getKeywordArg(":INSTANCE-OF") != null &&
					assertionContentTerm.getKeywordArg(":INSTANCE-OF").stringValue().equals("ONT::BE"))
			{
				if (assertionContentTerm.getKeywordArg(":NEUTRAL") != null)
				{
					String neutralSymbol = assertionContentTerm.getKeywordArg(":NEUTRAL").stringValue();
					KQMLList neutralList = KQMLUtilities.findTermInKQMLList(neutralSymbol, context);
					if (neutralList != null &&
							neutralList.getKeywordArg(":LEX") != null)
						assertedObjectName = neutralList.getKeywordArg(":LEX").stringValue();
				}
			}
		}
		
		if (assertedObjectName == null)
		{
			TextToSpeech.say("I don't know what your assertion is referring to.");
			return unacceptableContent("CANNOT-PROCESS", "ASSERTION", id, goal, asObject);
		}
		
		System.out.println("Model to learn: " + assertedObjectName);
		ModelInstantiation currentInstantiation = modelBuilder.getModelInstantiation(assertedObjectName);
		
		if (currentInstantiation == null)
		{
			modelBuilder.processNewModel(assertedObjectName);
			currentInstantiation = modelBuilder.getModelInstantiation(assertedObjectName);
//			TextToSpeech.say("I don't see anything on the table.");
//			return unacceptableContent("CANNOT-PROCESS", "ASSERTION", "NIL", goal, asObject);
		}
		
		currentInstantiation.getConstraintsFromKQML(context);
		
		TextToSpeech.say("Ok.");
		modelBuilder.processAssertion(content,context);
		goalStateHandler.systemState = SystemState.WAITING;
		
		return acceptableContent("ASSERTION", id, goal, goalLF, context);
	}

	public KQMLList handleAdoptMessage(KQMLList content, KQMLList context)
	{
		
		String goal = content.getKeywordArg(":WHAT").stringValue();
		String id = content.getKeywordArg(":ID").stringValue();
		KQMLObject asObject = content.getKeywordArg(":AS");
		KQMLList goalLF = KQMLUtilities.findTermInKQMLList(goal, context);
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		
		
		// I want to show/teach you what a tower is
		if (goalType.equalsIgnoreCase("ONT::SHOW") || goalType.equalsIgnoreCase("ONT::TEACH-TRAIN"))
		{
			if (goalType.equalsIgnoreCase("ONT::SHOW"))
				goalStateHandler.systemState = SystemState.LEARNING_DEMONSTRATION;
			else
				goalStateHandler.systemState = SystemState.LEARNING_CONSTRAINTS;
			String newModelName = "ONT::REFERENTIAL-SEM";
			KQMLObject whatPhrase = goalLF.getKeywordArg(":FORMAL");
			String whatPhraseSymbol;
			if (whatPhrase != null)
			{
				whatPhraseSymbol = whatPhrase.stringValue();
				KQMLList whatPhraseTerm = KQMLUtilities.findTermInKQMLList(whatPhraseSymbol, context);
				KQMLObject beTerm = whatPhraseTerm.getKeywordArg(":SUCHTHAT");
				
				if (beTerm != null)
				{
					String beTermSymbol = beTerm.stringValue();
					KQMLObject beTermObject = KQMLUtilities.findTermInKQMLList(beTermSymbol, context);
					if (beTermObject != null)
					{
						KQMLObject neutralObject = ((KQMLList)beTermObject).getKeywordArg(":NEUTRAL");
						if (neutralObject != null)
						{
							KQMLList neutralList = KQMLUtilities
													.findTermInKQMLList(neutralObject.stringValue(), context);
							if (neutralList != null)
								newModelName = neutralList.getKeywordArg(":LEX").stringValue();
						}
					}
				}
			}
			modelBuilder.processNewModel(newModelName);
			
		}
		
		
		return goalStateHandler.generateGoal(content, context);
/*
		System.out.println("ADOPT");
		if ( goalType.equalsIgnoreCase("ONT::CREATE"))
		{
			String objectToMakeID = goalLF.getKeywordArg(":AFFECTED-RESULT").stringValue();
			KQMLList objectToMakeLF = KQMLUtilities.findTermInKQMLList(objectToMakeID, context);
			String objectToMake = objectToMakeLF.getKeywordArg(":INSTANCE-OF").stringValue();
			String cleanObjectToMake = objectToMake.split("::")[1];
			
			if (modelBuilder.currentModel == null || !modelBuilder.currentModel.name.equals(cleanObjectToMake))
			{
				TextToSpeech.say("What is a " + cleanObjectToMake + "?");
				//steps.add(new Step("querymodeldefinition", cleanObjectToMake));
				modelBuilder.processNewModel(cleanObjectToMake);
			}
		}
		else if (goalType.equalsIgnoreCase("ONT::PUT"))
		{
			System.out.println("Putting object");
			FeatureProjection fp = new FeatureProjection(null);
			fp.extractProjectionFromKQML(context);
			String affected = goalLF.getKeywordArg(":AFFECTED").stringValue();
			String result = goalLF.getKeywordArg(":RESULT").stringValue();
			System.out.println("AFFECTED:" + affected);
			TemporalSequenceFeature tsfAffected = (TemporalSequenceFeature)fp.variableFGBindings.get(affected);
			tsfAffected.straighten();
			System.out.println(tsfAffected);
			List<BlockFeatureGroup> bfgList = tsfAffected.getBlockFeatureGroups();
			System.out.println("BFG List");
			for (BlockFeatureGroup bfg : bfgList)
			{
				System.out.println("Sending: " + bfg.getPointFeature().getValue());
				
				try {
					BlockMessageSender.sendPostRequest(bfg.getPointFeature().getValue());
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
			
			}
			//System.out.println("RESULT:");
			//System.out.println(fp.variableFGBindings.get(result));
			
		}
		else if ( goalType.equalsIgnoreCase("ONT::TEACH-TRAIN") || goalType.equalsIgnoreCase("ONT::SHOW"))
		{
			
			TextToSpeech.say("Ok.");
			return acceptableContent("ADOPT", id, goal, asObject);
			
		}
		return null;
		*/
	}
	

	
	private void sendGeneratedText()
	{
		
	}
	
	private KQMLList asGoal()
	{
		KQMLList response = new KQMLList();
		response.add("GOAL");
		return response;
	}
	
	public static KQMLList acceptableContent(String act, String id, String what, KQMLObject asList, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList acceptable = new KQMLList();
		acceptable.add("ACCEPTABLE");
		acceptable.add(":WHAT");
		
		KQMLList whatList = new KQMLList();
		whatList.add(act);
		whatList.add(":ID");
		whatList.add(id);
		whatList.add(":WHAT");
		whatList.add(what);
		whatList.add(":AS");
		whatList.add(asList);
		acceptable.add(whatList);
		
		response.add(acceptable);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	public static KQMLList acceptableContent(String act, String id, String what, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList acceptable = new KQMLList();
		acceptable.add("ACCEPTABLE");
		acceptable.add(":WHAT");
		
		KQMLList whatList = new KQMLList();
		whatList.add(act);
		whatList.add(":ID");
		whatList.add(id);
		whatList.add(":WHAT");
		whatList.add(what);
		acceptable.add(whatList);
		
		response.add(acceptable);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	public static KQMLList acceptableEffectContent(KQMLList what,
								KQMLList effect, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList acceptable = new KQMLList();
		acceptable.add("ACCEPTABLE");
		acceptable.add(":WHAT");
		
		acceptable.add(what);
		acceptable.add(":EFFECT");
		acceptable.add(effect);
		
		response.add(acceptable);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	public static KQMLList unacceptableContent(String type, String act, String id, String what,
											KQMLObject asList)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList unacceptable = new KQMLList();
		unacceptable.add("UNACCEPTABLE");
		unacceptable.add(":TYPE");
		unacceptable.add(type);
		unacceptable.add(":WHAT");
		
		KQMLList whatList = new KQMLList();
		whatList.add(act);
		whatList.add(":ID");
		whatList.add(id);
		whatList.add(":WHAT");
		whatList.add(what);
		whatList.add(":AS");
		whatList.add(asList);
		unacceptable.add(whatList);
		
		response.add(unacceptable);
		
		return response;
	}
	
	private static KQMLList failureContent(String type, String what, KQMLList reason,
											KQMLList possibleResolution)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList failure = new KQMLList();
		failure.add("FAILURE");
		failure.add(":TYPE");
		failure.add(type);
		failure.add(":WHAT");
		
		failure.add(what);
		failure.add(":REASON");
		failure.add(reason);
		failure.add(":POSSIBLE-RESOLUTION");
		failure.add(possibleResolution);
		response.add(failure);
		
		return response;		
	}
	
	
	private KQMLList answerContent(boolean answer, KQMLList context) 
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList answerContent = new KQMLList();
		answerContent.add("ANSWER");
		answerContent.add(":TO");
		answerContent.add(goalStateHandler.getCurrentGoal().getId());
		answerContent.add(":QUERY");
		answerContent.add(goalStateHandler.getCurrentGoal().getWhat());
		answerContent.add(":VALUE");
		if (answer == true)
			answerContent.add("ONT::TRUE");
		else
			answerContent.add("ONT::FALSE");
		
		response.add(answerContent);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	private KQMLList answerContent(String answerValue, KQMLList context) 
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList answerContent = new KQMLList();
		
		KQMLList valueList = valueList(answerValue);
		KQMLList newContext = new KQMLList();
		newContext.addAll(context);
		newContext.add(valueList);
		answerContent.add("ANSWER");
		answerContent.add(":TO");
		answerContent.add(goalStateHandler.getCurrentGoal().getId());
		answerContent.add(":QUERY");
		answerContent.add(goalStateHandler.getCurrentGoal().getWhat());
		answerContent.add(":VALUE");
		answerContent.add(valueList.get(1));
		response.add(answerContent);
		
		response.add(":CONTEXT");
		response.add(newContext);
		
		return response;
	}

	private KQMLList valueList(String value)
	{
		KQMLList result = new KQMLList();
		result.add("ONT::A");
		result.add(new KQMLToken("V" + getNextId()));
		result.add("NUMBER");
		result.add(":VALUE");
		result.add(value);
		
		return result;
	}
	
}
