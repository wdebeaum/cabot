package messages;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import utilities.KQMLUtilities;
import utilities.TextToSpeech;
import environment.Scene;
import environment.StructureInstance;
import features.BlockFeatureGroup;
import features.TemporalSequenceFeature;
import goals.Goal;
import goals.GoalMessages;
import goals.GoalStateHandler;
import goals.GoalStateHandler.SystemState;
import goals.Query;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;
import models.Constraint;
import models.ConstraintBundle;
import models.FeatureConstraint;
import models.FeatureProjection;
import models.GridModel2D;
import models.ModelBuilder;
import models.ModelInstantiation;
import models.PredicateConstraint;
import models.Response;
import models.StructureConstraint;

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
		Query q = new Query("",content.getKeywordArg(":ID").stringValue(), content);
		goalStateHandler.addGoal(q);
		String eventLFSymbol = content.getKeywordArg(":QUERY").stringValue();
		KQMLList eventLF = KQMLUtilities.findTermInKQMLList(eventLFSymbol, context);
		//String eventToQueryID = goalLF.getKeywordArg(":NEUTRAL").stringValue();
		//KQMLList eventToQuery = KQMLUtilities.findTermInKQMLList(eventToQueryID, context);
		String modelName;
		if (eventLF.getKeywordArg(":NEUTRAL1") != null)
		{
			String modelTermID = eventLF.getKeywordArg(":NEUTRAL1").stringValue();
			KQMLList modelTerm = KQMLUtilities.findTermInKQMLList(modelTermID, context);
			//String modelName = modelTerm.getKeywordArg(":INSTANCE-OF").stringValue();
			modelName = modelTerm.getKeywordArg(":LEX").stringValue();
			//steps.add(new Step("checkmodel", cleanObjectToMake));
		}
		else
		{
			modelName = modelBuilder.getLastModelName();
			if (modelName == null)
			{
				TextToSpeech.say("I don't know which model you are referring to.");
				return unacceptableContent("CANNOT-PROCESS", "ASK-IF", 
							content.getKeywordArg(":ID").stringValue(),
							"NIL", content.getKeywordArg(":AS"));
			}
		}
		if (modelBuilder.getModelInstantiation(modelName) == null)
		{
			TextToSpeech.say("I don't know what a " + KQMLUtilities.cleanLex(modelName) + 
					" is.");
			return unacceptableContent("CANNOT-PROCESS", "ASK-IF", 
					content.getKeywordArg(":ID").stringValue(),
					"NIL", content.getKeywordArg(":AS"));
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
		
		
		
		if (modelBuilder.getModelInstantiation(modelName).getConstraints().isEmpty())
		{
			TextToSpeech.say("Yes, but that's because I didn't get any "
					+ "constraints for the model of a " + 
								KQMLUtilities.cleanLex(modelName));
			return answerContent(content.getKeywordArg(":ID").stringValue(),
					content.getKeywordArg(":QUERY").stringValue(),
					true,context);
		}
		
		Response.isSatisfied(modelBuilder.getModelInstantiation(modelName), satisfied);
		
		return answerContent(content.getKeywordArg(":ID").stringValue(),
							content.getKeywordArg(":QUERY").stringValue(),satisfied,context);
	}
	
	public KQMLList handleAskWhMessage(KQMLList content, KQMLList context)
	{
		Query q = new Query(content.getKeywordArg(":WHAT").stringValue(),
					content.getKeywordArg(":ID").stringValue(), content);
		goalStateHandler.addGoal(q);
		String eventLFSymbol = content.getKeywordArg(":QUERY").stringValue();
		KQMLList eventLF = KQMLUtilities.findTermInKQMLList(eventLFSymbol, context);
		
		String propertySymbol = eventLF.getKeywordArg(":NEUTRAL1").stringValue();
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
		
		String result = "NIL";
		if (structure != null && structure.hasFeature(property))
		{
			result = "" + Math.round((double)(structure.getFeature(property).getValue()));
			
			TextToSpeech.say("The " + KQMLUtilities.cleanConcept(property) + " is " + result + " blocks");
		}
		else
		{
			TextToSpeech.say("I'm sorry, I don't know what that property is.");
		}
		
		return answerContent(result,context);
	}
	
	public KQMLList handleYesNoAnswerMessage(KQMLList content, KQMLList context)
	{
		// Clear planned blocks
		try {
			BlockMessageSender.clearBlocks();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
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
				return GoalMessages.waitingForUser(goalStateHandler.getCurrentGoal().getId());
			}
		}
		else if (instanceOf.equals("ONT::STRUCTURE"))
		{
			return acceptableContent(content, context);
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
		
		String toVariable = content.getKeywordArg(":TO").stringValue();
		if (goalStateHandler.getGoal(toVariable) == null)
		{
			return failureContent("FAILED-TO-INTERPRET", toVariable,
					new KQMLList(new KQMLToken("MISSING-GOAL")), new KQMLList());		
		}
		
		// There's a constraint waiting to be answered
		if (modelBuilder.getLastConstraintAsked() != null)
			return modelBuilder.answerCurrentConstraint(content, context);

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
		String parentGoalType = "";
		String proposedGoalType = "";
		if (goalStateHandler.getCurrentGoal() != null)
			parentGoalType = goalStateHandler.getCurrentGoal().getInstanceOf();
		if (modelBuilder.getLastGoal() != null)
			proposedGoalType = modelBuilder.getLastGoal().getInstanceOf();
		Goal assertionGoal = new Goal(goal,id,goalLF);
		goalStateHandler.addGoal(assertionGoal);
		KQMLList assertions = (KQMLList)goalLF.getKeywordArg(":EVENTS");
		
		String assertedObjectName = null;
		
		if (modelBuilder.getLastModelName() != null)
			assertedObjectName = modelBuilder.getLastModelName();
		//if (modelBuilder.currentModel != null)
		//	assertedObjectName = modelBuilder.currentModel.name;

		
		// This is for figuring out broadly what the object being described is
		KQMLList assertionContentTerm = null;
		KQMLList neutralList = null;
		for (KQMLObject assertionObject : assertions)
		{
			assertionContentTerm = KQMLUtilities.findTermInKQMLList(assertionObject.stringValue(), context);
			
			if (assertionContentTerm == null)
				continue;
			
			String relevantSymbol = null;
			if (assertionContentTerm.getKeywordArg(":NEUTRAL1") != null)
			{
				relevantSymbol = assertionContentTerm.getKeywordArg(":NEUTRAL1").stringValue();	
				KQMLList relevantTerm = KQMLUtilities.findTermInKQMLList(relevantSymbol, context);
				if (relevantTerm.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::EXAMPLE"))
				{
					TextToSpeech.say("Ok.");
					modelBuilder.processAssertion(content,context);
					return acceptableContent("ASSERTION", id, goal, goalLF, context);
				}
					
			}
			
			if (assertionContentTerm.getKeywordArg(":NEUTRAL") != null)
			{
				relevantSymbol = assertionContentTerm.getKeywordArg(":NEUTRAL").stringValue();

			}
			else if (assertionContentTerm.getKeywordArg(":AFFECTED") != null)
			{
				relevantSymbol = assertionContentTerm.getKeywordArg(":AFFECTED").stringValue();
			
			}
			else if (assertionContentTerm.getKeywordArg(":AFFECTED-RESULT") != null)
			{
				relevantSymbol = assertionContentTerm.getKeywordArg(":AFFECTED-RESULT").stringValue();			
			}

			
			if (relevantSymbol != null)
			{
				neutralList = KQMLUtilities.findTermInKQMLList(relevantSymbol, context);
				if (assertedObjectName == null && neutralList != null &&
						neutralList.getKeywordArg(":LEX") != null && 
						!neutralList.getKeywordArg(":SPEC").stringValue().equals("ONT::PRO"))
					assertedObjectName = neutralList.getKeywordArg(":LEX").stringValue();
			}

		}
		
		if (assertedObjectName == null)
		{
			TextToSpeech.say("I don't know what your assertion is referring to.");
			return unacceptableContent("CANNOT-PROCESS", "ASSERTION", id, goal, asObject);
		}
		
		System.out.println("Model to learn: " + assertedObjectName);
		ModelInstantiation currentInstantiation = modelBuilder.getModelInstantiation(assertedObjectName);
		ConstraintBundle cb = null;
		
		if (currentInstantiation == null)
		{
			modelBuilder.processNewModel(assertedObjectName);
			currentInstantiation = modelBuilder.getModelInstantiation(assertedObjectName);
		}
		
		if (neutralList != null)
		{
			cb = currentInstantiation.getConstraintsFromKQML(assertionContentTerm, context);
			
		}
		else
			System.out.println("No Neutral term to learn assertions");
		
		if (cb != null && cb.size() > 0)
		{
			StringBuilder sb = new StringBuilder();
			sb.append("Ok. It should be true that ");
			int currentConstraint = 0;
			for (Constraint c : cb.getConstraints())
			{
				sb.append(c.reason(true));
				currentConstraint += 1;
				if (cb.size() > currentConstraint)
					sb.append(" and ");
			}
			TextToSpeech.say(sb.toString());
			try {
				Thread.sleep(1600);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			modelBuilder.processAssertion(content,context);
		}
		else if (proposedGoalType != null &&
				proposedGoalType.equalsIgnoreCase("SHOW-EXAMPLE"))
		{
			TextToSpeech.say("Ok.");
			modelBuilder.processAssertion(content,context);
		}
		else if (cb != null && cb.getReferringExpressions().size() > 0)
		{
			currentInstantiation.lastUnresolvedObjectReference = cb.getReferringExpressions().get(0);
		}
		else
		{
			TextToSpeech.say("Hmm, I'm sorry, I couldn't understand any constraints from that." );
			// This takes awhile but shouldn't stop the next utterance
			if (TextToSpeech.SPEECH_ENABLED)
			{
				try {
					Thread.sleep(4100);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		//goalStateHandler.systemState = SystemState.WAITING;
		
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
			if (goalLF.getKeywordArg(":NEUTRAL") != null)
			{
				String neutralSymbol = goalLF.getKeywordArg(":NEUTRAL").stringValue();
				KQMLList neutralTerm = KQMLUtilities.findTermInKQMLList(neutralSymbol,
												context);
				if (neutralTerm.getKeywordArg(":INSTANCE-OF").stringValue().
											equalsIgnoreCase("ONT::EXAMPLE"))
				{
					int numberOfBlocksToUse = -1;
					// Check if a specific number of blocks should be used
					for (KQMLObject term : context)
					{
						if (term instanceof KQMLList)
						{
							KQMLList termAsList = (KQMLList)term;
							if (termAsList.getKeywordArg(":VALUE") != null)
							{
								numberOfBlocksToUse = Integer.parseInt(termAsList.getKeywordArg(":VALUE").stringValue());
								break;
							}
						}
					}
					
					GridModel2D example = null;
					if (numberOfBlocksToUse > 0)
						example = modelBuilder.generateExample(numberOfBlocksToUse);
					else
						example = modelBuilder.generateExample();
					
					if (example != null)
					{
						try {
							BlockMessageSender.sendPostRequest(example.getBlocks());
						}
						catch (IOException e)
						{
							e.printStackTrace();
						}
						return goalStateHandler.generateGoal(content, context);
					}
					
					
				}
			}
			
			if (goalType.equalsIgnoreCase("ONT::SHOW"))
				goalStateHandler.systemState = SystemState.LEARNING_DEMONSTRATION;
			else
				goalStateHandler.systemState = SystemState.LEARNING_CONSTRAINTS;
			String newModelName = "ONT::REFERENTIAL-SEM";
			KQMLObject whatPhrase = goalLF.getKeywordArg(":FORMAL");
			String whatPhraseSymbol;
			KQMLList whatPhraseTerm = null;
			if (whatPhrase != null)
			{
				whatPhraseSymbol = whatPhrase.stringValue();
				whatPhraseTerm = KQMLUtilities.findTermInKQMLList(whatPhraseSymbol, context);
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
			else {
				TextToSpeech.say("Sorry, I'm having trouble doing that.");
				return unacceptableContent(goalType, "ADOPT", id, goal, asObject);
			
			}
			//
			//modelBuilder.processNewModel(newModelName);
			modelBuilder.processNewModel(whatPhraseTerm,context);
			
		}
		// "Forget the constraint"
		else if (goalType.equalsIgnoreCase("ONT::OMIT") || goalType.equalsIgnoreCase("ONT::CAUSE-COME-FROM"))
		{
			List<Constraint> constraintList = modelBuilder.getLastModelInstantiation().constraints;
			if (constraintList.isEmpty())
			{
				TextToSpeech.say("I don't have any constraints to delete.");
				return unacceptableContent(goalType, "ADOPT", id, goal, asObject);
				
				
			}
			boolean plural = false;
			boolean removedConstraint = false;
			boolean universal = false;
			String objectType = "";
			int numOfRemovedConstraints = 0;
			Constraint lastConstraint = null;
			// Find a specific constraint
			if (goalLF.getKeywordArg(":AFFECTED") != null)
			{
				
				
				String constraintVariable = goalLF.getKeywordArg(":AFFECTED").stringValue();
				KQMLList constraintTermList = KQMLUtilities.findTermInKQMLList(constraintVariable, context);
				if (constraintTermList.get(KQMLUtilities.ONT_REF).stringValue().equalsIgnoreCase("ONT::THE-SET"))
					plural = true;
				if (constraintTermList.getKeywordArg(":SIZE") != null)
				{
					if (constraintTermList.getKeywordArg(":SIZE").stringValue().equalsIgnoreCase("ONT::UNIVERSAL"))
						universal = true;
				}
				if (constraintTermList.getKeywordArg(":ASSOC-WITH") != null)
				{
					String assocWithVariable = constraintTermList.getKeywordArg(":ASSOC-WITH").stringValue();
					KQMLList assocWithTermList = KQMLUtilities.findTermInKQMLList(assocWithVariable, context);
					
					if (assocWithTermList.getKeywordArg(":INSTANCE-OF") != null)
					{
						objectType = assocWithTermList.getKeywordArg(":INSTANCE-OF").stringValue();
						
						Iterator<Constraint> i = constraintList.iterator();
						while (i.hasNext())
						{
							Constraint constraint = i.next();
							boolean matchingConstraint = false;
							if (universal)
								matchingConstraint = true;
							
							if (constraint instanceof StructureConstraint)
							{
								
								if (((StructureConstraint)constraint).getSubject().getInstanceOf().
										equalsIgnoreCase(objectType))
								{
									matchingConstraint = true;
								}
							}
							else if (constraint instanceof PredicateConstraint)
							{
								
								if (((PredicateConstraint)constraint).getSubject().getInstanceOf().
										equalsIgnoreCase(objectType))
								{
									matchingConstraint = true;
								}
							}
							else if (constraint instanceof FeatureConstraint)
							{
								if (((FeatureConstraint)constraint).getFeature().getName().
										equalsIgnoreCase(objectType))
								{
									matchingConstraint = true;
								}
							}
							
							if (matchingConstraint)
							{
								removedConstraint = true;
								lastConstraint = constraint;
								numOfRemovedConstraints++;
								i.remove();
								if (!plural)
									break;
							}
						}
					}
						
				}
				
				if (removedConstraint)
				{
					
				}
			}
			if (!removedConstraint)
			{
				lastConstraint = constraintList.get(constraintList.size()-1);
				modelBuilder.getLastModelInstantiation().constraints.remove(lastConstraint);
			}
			if (plural)
			{
				TextToSpeech.say("I removed " + numOfRemovedConstraints + " " + KQMLUtilities.cleanOnt(objectType) + " constraint." );
			}
			else
			{
				TextToSpeech.say("I removed the constraint that the \"" + lastConstraint.reason(true) + "\"." );
			}
			try {
				Thread.sleep(1600);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
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
	
	public static KQMLList acceptableContent(KQMLList content, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList acceptable = new KQMLList();
		acceptable.add("ACCEPTABLE");
		acceptable.add(":WHAT");
		acceptable.add(content);
		
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
	
	public static KQMLList unacceptableAnswerContent(KQMLList content)
{
	KQMLList response = new KQMLList();
	response.add("REPORT");
	response.add(":CONTENT");
	KQMLList unacceptable = new KQMLList();
	unacceptable.add("UNACCEPTABLE");
	unacceptable.add(":TYPE");
	unacceptable.add("ANSWER");
	unacceptable.add(":WHAT");
	unacceptable.add(content);
	
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
	
	
	private KQMLList answerContent(String queryId, String queryWhat, boolean answer, KQMLList context) 
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList answerContent = new KQMLList();
		answerContent.add("ANSWER");
		answerContent.add(":TO");
		answerContent.add(queryId);
		answerContent.add(":QUERY");
		answerContent.add(queryWhat);
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
