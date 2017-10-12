package goals;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

import environment.Block;
import environment.BuildAction;
import environment.Scene;
import features.BlockFeatureGroup;
import features.TemporalSequenceFeature;
import messages.BlockMessageSender;
import messages.EvaluateHandler;
import models.FeatureConstraint;
import models.FeatureProjection;
import models.ModelBuilder;
import models.ModelInstantiation;
import utilities.KQMLUtilities;
import utilities.TextToSpeech;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLToken;
import TRIPS.SRIWrapper.SRIWrapper;


public class GoalStateHandler {
	
	private HashMap<String,Goal> nameGoalMapping;
	private HashMap<String,Goal> idGoalMapping;
	private Goal currentGoal;
	private KQMLList currentContext;
	private GoalState goalState;
	public SystemState systemState;
	private ModelBuilder modelBuilder;
	private SRIWrapper sriWrapper;
	private static int nextId = 1;
	private boolean checkingForGoalCompletion = false;
	public String lastReplyWith = "";
	public boolean userCompleted = false;
	public Block lastBlockPlaced;
	private boolean systemGoalStarted = false;
	public enum SystemState {LEARNING_DEMONSTRATION, LEARNING_CONSTRAINTS, TESTING, BUILDING, WAITING, CHECKING_IF_DONE}
	
	public enum GoalState {ACCEPTED, PROPOSED, COMPLETED, REJECTED, WAITING}

	public GoalStateHandler(ModelBuilder modelBuilder, SRIWrapper sriWrapper) {
		currentGoal = null;
		this.sriWrapper = sriWrapper;
		goalState = GoalState.WAITING;
		systemState = SystemState.WAITING;
		nameGoalMapping = new HashMap<String, Goal>();
		idGoalMapping = new HashMap<String, Goal>();
		this.modelBuilder = modelBuilder;
		lastBlockPlaced = null;
	}
	
	public int getNextId()
	{
		int idToReturn = nextId;
		nextId++;
		return idToReturn;
	}
	
	public KQMLList whatNext(KQMLList context, String goalId)
	{
		currentGoal = getGoal(goalId);
		currentContext = context;
		System.out.println("Doing whatNext for goal " + goalId);
		if (currentGoal != null)
		{
			//if (currentGoal.getType().equals(Goal.GoalType.QUERYINCONTEXT))
			//	return queryModel(currentGoal.getTerm(), context);
			System.out.println("with content: " + currentGoal.getTerm());
			String goalInstanceType = currentGoal.getInstanceOf();
			
			if (currentGoal.isCompleted())
			{
				checkingForGoalCompletion = false;
				return executionStatus(goalId, "ONT::DONE");
			}
			if (goalInstanceType == null)
			{
				System.out.println("Instance type was null");
			
				KQMLObject query = currentGoal.getTerm().getKeywordArg(":QUERY");
				if (query != null)
				{
					KQMLList queryTerm = KQMLUtilities.findTermInKQMLList(query.stringValue(),
												context);
					if (queryTerm != null)
					{
						KQMLObject figure = queryTerm.getKeywordArg(":FIGURE");
						if (figure != null && queryTerm.getKeywordArg(":INSTANCE-OF") != null)
						{
							
							String queryInstanceOf = queryTerm.getKeywordArg(":INSTANCE-OF").stringValue();
							String figureVariable = figure.stringValue();
							if (queryInstanceOf.equalsIgnoreCase("ONT::FINISHED") && 
									getGoal(figureVariable) != null)
							{
								System.out.println("Set goal " + figureVariable + " as completed!");
								getGoal(figureVariable).setCompleted();
								userCompleted = true;
							}
						}
					}
				}
				
				checkingForGoalCompletion = false;
				return executionStatus(goalId, "ONT::DONE");
			}
			
			KQMLList term = currentGoal.getTerm();
			
			if (goalInstanceType.equalsIgnoreCase("ONT::PUT") || goalInstanceType.equals("ONT::ADD-INCLUDE") ||
					goalInstanceType.equalsIgnoreCase("ONT::MOVE"))
			{
				System.out.println("Putting object");
				FeatureProjection fp = new FeatureProjection(null);
				if (lastBlockPlaced != null)
					FeatureProjection.setNewOrigin(lastBlockPlaced.position);
				// Get relevant elements from KQML
				fp.extractProjectionFromKQML(context);
				
//				String affected = term.getKeywordArg(":AFFECTED").stringValue();
//				
//				System.out.println("AFFECTED:" + affected);
//				TemporalSequenceFeature tsfAffected = (TemporalSequenceFeature)fp.variableFGBindings.get(affected);
//				tsfAffected.straighten();
//				System.out.println(tsfAffected);
				BuildAction ba = fp.getBuildAction();
				if (ba == null)
					return null;
				TemporalSequenceFeature tsfAffected = ba.getBuildSequence();
				List<BlockFeatureGroup> bfgList = tsfAffected.getBlockFeatureGroups();
				System.out.println("BFG List");
				for (BlockFeatureGroup bfg : bfgList)
				{
					System.out.println("Sending: " + bfg.getPointFeature().getValue());
					
					double newHeight = bfg.getPointFeature().getValue().get(2);
					if (newHeight - lastBlockPlaced.getZ() > Block.BLOCK_WIDTH)
					{
						bfg.getPointFeature().getValue().put(2,lastBlockPlaced.getZ() + Block.BLOCK_WIDTH);
					}
					try {
						BlockMessageSender.sendPostRequest(bfg.getPointFeature().getValue());
					}
					catch (IOException e)
					{
						e.printStackTrace();
					}
				
				}
				return executionStatus(goalId, "ONT::DONE");
			}
			else if (goalInstanceType.equalsIgnoreCase("ONT::EVENTS-IN-MODEL"))
			{
				return done(goalId);
			}
			else if (goalInstanceType.equalsIgnoreCase("ONT::CREATE"))
			{
				if (!userCompleted)
				{
					KQMLObject affectedResult = term.getKeywordArg(":AFFECTED-RESULT");
					systemState = SystemState.BUILDING;
					if (affectedResult == null)
					{
						TextToSpeech.say("What do you want to build?");
						return askWhatToBuild("ASK" + getNextId(), context);
					}
					
					String affectedResultVariable = affectedResult.stringValue();
					KQMLList affectedResultTerm = 
							KQMLUtilities.findTermInKQMLList(affectedResultVariable, context);
					String instanceType = affectedResultTerm.getKeywordArg(":INSTANCE-OF").stringValue();
					String lexName = affectedResultTerm.getKeywordArg(":LEX").stringValue();
					if (instanceType.equalsIgnoreCase("ONT::REFERENTIAL-SEM"))
					{
						TextToSpeech.say("What do you want to build?");
						return askWhatToBuild("ASK" + getNextId(), context);					
					}
					// If we don't know the model
					else if (!modelBuilder.hasModel(lexName))
					{
						TextToSpeech.say("Can you teach me what a " +
									KQMLUtilities.cleanLex(lexName) + " is?");
						systemState = SystemState.LEARNING_CONSTRAINTS;
						
						return waitingForUser(currentGoal.getId());	
					}
					else if (currentGoal.getAgent(context) == null)
					{
						TextToSpeech.say("Who should build it?");
						
					}
					else
					{
						TextToSpeech.say("Go ahead.");
						checkingForGoalCompletion = true;
						//return workingOnIt(goalId);
						return null;
					}
				}
				else // User finished their part, start working on system's
				{
					if (!systemGoalStarted)
					{
						systemGoalStarted = true;
						TextToSpeech.say("Ok, good. I want to build a column, but we don't have " +
											"enough blocks.");
						
						KQMLList column = new KQMLList();
						column.add("ONT::F");
						column.add("C" + getNextId());
						column.add(":INSTANCE-OF");
						column.add("ONT::COLUMN");
						column.add(":LEX");
						column.add("W::COLUMN");
						
						KQMLList buildContent = new KQMLList();
						buildContent.add("ONT::RELN");
						buildContent.add("B" + getNextId());
						buildContent.add(":INSTANCE-OF");
						buildContent.add("ONT::CREATE");
						buildContent.add(":AFFECTED-RESULT");
						buildContent.add(column.get(1));
						
						KQMLList asList = new KQMLList();
						asList.add("SUBGOAL");
						asList.add(":OF");
						asList.add(currentGoal.getId());
						
						Goal goalToBuild = new Goal(buildContent.get(1).stringValue(),
											"BW" + getNextId(),buildContent);
						return proposeAdoptContent(goalToBuild.getId(), goalToBuild.getWhat(), asList, context);
					}
					else
					{
						return waitingForUser(currentGoal.getId());
					}
				}
			}
			else if (goalInstanceType.equalsIgnoreCase("SHOW-EXAMPLE"))
			{
				return waitingForUser(currentGoal.getId());
			}
		}
		else // No current goal
		{
			// Should we start building our own thing?
			if (systemState == SystemState.BUILDING)
			{
				
			}
			System.out.println("No goal with name or id " + goalId);
		}
		
		System.out.println("Goal state: " + goalState.toString());
		
		if (goalState == GoalState.WAITING || goalState == GoalState.COMPLETED)
		{
			TextToSpeech.say("I'm waiting for you to do or suggest something.");
			return suggestSomething(context);
		}
		
		if (goalState == GoalState.ACCEPTED && (systemState == SystemState.LEARNING_DEMONSTRATION ||
												systemState == SystemState.LEARNING_CONSTRAINTS))
		{
			Goal newGoal = modelBuilder.whatNext(systemState);
			String currentGoalId = "NIL";
			if (currentGoal != null)
			{
				if (newGoal != null)
					newGoal.setParent(currentGoal);
				currentGoalId = currentGoal.getId();
			}
			if (newGoal == null)
			{
				return waitingForUser(currentGoalId);
			}
			context.add(newGoal.getTerm());
			
			return proposeAdoptContent(newGoal.getId(), newGoal.getWhat(), newGoal.getAsList(), context);
		}

		
		return null;
	}
	
	private KQMLList suggestSomething(KQMLList context)
	{
		Goal suggestGoal = new Goal("ONT::YOU-SUGGEST-SOMETHING");
		return proposeAdoptContent(suggestGoal.getId(), suggestGoal.getWhat(), suggestGoal.getAsList(), context);
	}
	
	private KQMLList askWhatToBuild(String id, KQMLList context)
	{
		KQMLList currentTerm = currentGoal.getTerm();
		String questionVariable = "WH" + getNextId();
		String queryVariable = "Q" + getNextId();
		String queryId = "QI" + getNextId();
		KQMLList newContext = new KQMLList();
		KQMLList newTerm = new KQMLList();
		newTerm.addAll(currentTerm);
		
		
		if (newTerm.getKeywordArg(":AFFECTED-RESULT") != null)
			newTerm.removeKeywordArg(":AFFECTED-RESULT");
		newTerm.add(":AFFECTED-RESULT");
		newTerm.add(questionVariable);
		
		Goal newQuery = new Goal(queryVariable,queryId,newTerm);
		newQuery.updateTerm();
		addGoal(newQuery);
		newContext.addAll(context);
		newContext.add(newQuery.getTerm());
		
		return proposeAskWhContent(id,questionVariable,queryVariable,newContext);
	}
	
	private KQMLList askHowManyBlocksLeft(String id, KQMLList context)
	{
		
		KQMLList currentTerm = currentGoal.getTerm();
		String questionVariable = "WH" + getNextId();
		String queryVariable = "Q" + getNextId();
		String queryId = "QI" + getNextId();
		KQMLList newContext = new KQMLList();
		
		KQMLList blocksTerm = new KQMLList();
		String blocksWhat = "BWH" + getNextId();
		KQMLList remainingTerm = new KQMLList();
		String remainingWhat = "RWH" + getNextId();
		
		remainingTerm.add("ONT::F");
		remainingTerm.add("R" + getNextId());
		remainingTerm.add(":INSTANCE-OF");
		remainingTerm.add("ONT::REMAINING");
		remainingTerm.add(":FIGURE");
		remainingTerm.add(blocksWhat);
		
		
		blocksTerm.add("ONT::INDEF-SET");
		blocksTerm.add("B" + getNextId());
		blocksTerm.add(":INSTANCE-OF");
		blocksTerm.add("ONT::BLOCKS");
		blocksTerm.add(":SIZE");
		blocksTerm.add(questionVariable);
		blocksTerm.add(":MODs");
		blocksTerm.add(new KQMLList(new KQMLToken(remainingWhat)));
		
		
		Goal newQuery = new Goal(blocksTerm.get(1).stringValue(),
								"QF" + getNextId(),
									blocksTerm);
		newQuery.updateTerm();
		
		addGoal(newQuery);
		
		newContext.addAll(currentContext);
		newContext.add(newQuery.getTerm());
		newContext.add(remainingTerm);
		
		return proposeAskWhContent(id,
						questionVariable,queryVariable, newContext);
		
	}
	
	public void checkNewScene(Scene s)
	{
		
		Block lastBlock = null;
		for (Block b : s.integerBlockMapping.values())
		{
			if (lastBlock == null || b.getId() < lastBlock.getId())
				lastBlock = b;
		}
		
		if (lastBlock != null)
			lastBlockPlaced = lastBlock; 
		
		if (!checkingForGoalCompletion)
			return;
		
		System.out.println("Checking scene " + s);
		KQMLList term = currentGoal.getTerm();
		KQMLObject affectedResult = term.getKeywordArg(":AFFECTED-RESULT");

		String affectedResultVariable = affectedResult.stringValue();
		KQMLList affectedResultTerm = 
				KQMLUtilities.findTermInKQMLList(affectedResultVariable, currentContext);
		String instanceType = affectedResultTerm.getKeywordArg(":INSTANCE-OF").stringValue();
		String lexName = affectedResultTerm.getKeywordArg(":LEX").stringValue();
		
		if (lexName.equalsIgnoreCase("W::COLUMN"))
		{
			
		}
		
		ModelInstantiation mi = modelBuilder.getModelInstantiation(lexName);
		if (mi == null)
		{
			System.out.println("Unknown model name error");
			return;
		}
		
		boolean satisfied = mi.testModelOnStructureInstance(s.integerBlockMapping.values());
		
		// TESTING
		//boolean satisfied = true;
		if (satisfied)
		{
			TextToSpeech.say("That looks like a " + KQMLUtilities.cleanLex(lexName) +
								" to me. Are you finished?");
			checkingForGoalCompletion = false;
			waitingForUser(currentGoal.getId());
			askIfDone(currentGoal.getId());
		}
		
	}
	
	public Goal getCurrentGoal() {
		return currentGoal;
	}

	public Goal getGoal(String idOrWhat)
	{
		if (nameGoalMapping.containsKey(idOrWhat))
			return nameGoalMapping.get(idOrWhat);
		if (idGoalMapping.containsKey(idOrWhat))
			return idGoalMapping.get(idOrWhat);
		
		return null;
	}
	
	public void addGoal(Goal g)
	{
		if (g == null)
			return;
		nameGoalMapping.put(g.getWhat(),g);
		idGoalMapping.put(g.getId(),g);
	}
	
	public KQMLList generateGoal(KQMLList evaluateContent, KQMLList context)
	{
		String what = evaluateContent.getKeywordArg(":WHAT").stringValue();
		String id = evaluateContent.getKeywordArg(":ID").stringValue();
		KQMLObject asObject = evaluateContent.getKeywordArg(":AS");
		KQMLList goalLF = KQMLUtilities.findTermInKQMLList(what, context);
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		
		Goal newGoal = new Goal(what, id, goalLF);
		goalState = GoalState.ACCEPTED;
		addGoal(newGoal);
		
		return EvaluateHandler.acceptableContent("ADOPT", id, what, asObject, context);
	}
	
	// TODO : Finish
	public KQMLList generateSubGoal(KQMLList evaluateContent, String subgoalWhat, KQMLList context)
	{
		String what = evaluateContent.getKeywordArg(":WHAT").stringValue();
		String id = evaluateContent.getKeywordArg(":ID").stringValue();
		KQMLObject asObject = evaluateContent.getKeywordArg(":AS");
		KQMLList goalLF = KQMLUtilities.findTermInKQMLList(what, context);
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		
		Goal newGoal = new Goal(what, id, goalLF);
		goalState = GoalState.ACCEPTED;
		addGoal(newGoal);
		
		return EvaluateHandler.acceptableContent("ADOPT", id, what, asObject, context);
	}
	
	private KQMLList executionStatus(String goalId, String status)
	{
		KQMLList response = new KQMLList();
		response.add("REPORT");
		response.add(":CONTENT");
		KQMLList executionStatusContent = new KQMLList();
		executionStatusContent.add("EXECUTION-STATUS");
		executionStatusContent.add(":GOAL");
		executionStatusContent.add(goalId);
		executionStatusContent.add(":STATUS");
		executionStatusContent.add(status);
		
		response.add(executionStatusContent);
		
		return response;		
	}
	
	public KQMLList waitingForUser(String goalId)
	{
		return executionStatus(goalId,"ONT::WAITING-FOR-USER");
	}
	
	private KQMLList workingOnIt(String goalId)
	{
		return executionStatus(goalId,"ONT::WORKING-ON-IT");
	}
	
	public KQMLList done(String goalId)
	{
		return executionStatus(goalId,"ONT::DONE");
	}
	
	private void askIfDone(String goalId)
	{
		systemState = SystemState.CHECKING_IF_DONE;
		KQMLList finished = new KQMLList();
		finished.add("ONT::F");
		finished.add("F" + getNextId());
		finished.add(":INSTANCE-OF");
		finished.add("ONT::FINISHED");
	
		
		finished.add(":FIGURE");
		finished.add(currentGoal.getWhat());
		
		Goal newQuery = new Goal(finished.get(1).stringValue(),
								"QF" + getNextId(),
									finished);
		newQuery.updateTerm();
		
		//addGoal(newQuery);
		
		KQMLList newContext = new KQMLList();
		newContext.addAll(currentContext);
		newContext.add(newQuery.getTerm());
		
		KQMLList responseContent = proposeAskIfContent(newQuery.getId(),
						finished.get(1).stringValue(), newContext);
		KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
		performativeToSend.setParameter(":RECEIVER", "DAGENT");
		System.out.println("Response content: " + responseContent);
		performativeToSend.setParameter(":CONTENT",responseContent);
		performativeToSend.setParameter(":IN-REPLY-TO", lastReplyWith);
		sriWrapper.sendKQMLPerformative(performativeToSend);
		
	}
	
	private void sendDone(String goalId)
	{
		KQMLList responseContent = done(goalId);
		KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
		performativeToSend.setParameter(":RECEIVER", "DAGENT");
		System.out.println("Response content: " + responseContent);
		performativeToSend.setParameter(":CONTENT",responseContent);
		performativeToSend.setParameter(":IN-REPLY-TO", lastReplyWith);
		sriWrapper.sendKQMLPerformative(performativeToSend);
	}
	
	
	private KQMLList proposeAdoptContent(String id, String what, KQMLObject asList, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("PROPOSE");
		response.add(":CONTENT");
		KQMLList adoptContent = new KQMLList();
		adoptContent.add("ADOPT");
		adoptContent.add(":ID");
		adoptContent.add(id);
		adoptContent.add(":WHAT");
		adoptContent.add(what);
		adoptContent.add(":AS");
		adoptContent.add(asList);
		
		response.add(adoptContent);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	private KQMLList proposeAskWhContent(String id, String what, String query, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("PROPOSE");
		response.add(":CONTENT");
		KQMLList adoptContent = new KQMLList();
		adoptContent.add("ASK-WH");
		adoptContent.add(":ID");
		adoptContent.add(id);
		adoptContent.add(":WHAT");
		adoptContent.add(what);
		adoptContent.add(":QUERY");
		adoptContent.add(query);
		adoptContent.add(":AS");
		
		KQMLList asList = new KQMLList();
		asList.add("QUERY-IN-CONTEXT");
		asList.add(":GOAL");
		asList.add(currentGoal.getId());
		adoptContent.add(asList);
		
		response.add(adoptContent);
		

		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;		
	}
	
	private KQMLList proposeAskIfContent(String id, String query, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("PROPOSE");
		response.add(":CONTENT");
		KQMLList adoptContent = new KQMLList();
		adoptContent.add("ASK-IF");
		adoptContent.add(":ID");
		adoptContent.add(id);
		adoptContent.add(":QUERY");
		adoptContent.add(query);
		adoptContent.add(":AS");
		
		KQMLList asList = new KQMLList();
		asList.add("QUERY-IN-CONTEXT");
		asList.add(":GOAL");
		asList.add(currentGoal.getId());
		adoptContent.add(asList);
		
		response.add(adoptContent);
		
		response.add(":CONTEXT");
		response.add(context);
		
		Goal newGoal = new Goal(id + "WH",id,adoptContent);
		addGoal(newGoal);
		
		return response;		
	}
	
}
