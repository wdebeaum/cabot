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
import models.Constraint;
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
	private Goal lastProposedGoal;
	private Goal topLevelGoal;
	private Constraint lastConstraint;
	private KQMLList currentContext;
	private GoalState goalState;
	public SystemState systemState;
	private ModelBuilder modelBuilder;
	public SRIWrapper sriWrapper;
	private static int nextId = 1;
	private boolean checkingForGoalCompletion = false;
	public String lastReplyWith = "";
	public boolean userCompleted = false;
	public Block lastBlockPlaced;
	private boolean systemGoalStarted = false;
	public enum SystemState {LEARNING_DEMONSTRATION, LEARNING_CONSTRAINTS, TESTING, BUILDING, WAITING, CHECKING_IF_DONE, CHECKING_CONSTRAINT, FINISHED}
	
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
		lastProposedGoal = null;
	}
	
	public static int getNextId()
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
			
			if (systemState.equals(SystemState.CHECKING_CONSTRAINT))
			{
				return GoalMessages.askIfConstraintShouldBeAdded(currentGoal);
			}
			
			if (systemState.equals(SystemState.FINISHED))
			{
				checkingForGoalCompletion = false;
				if (topLevelGoal != null)
					return GoalMessages.executionStatus(topLevelGoal.getId(), "ONT::DONE");
				else
					return GoalMessages.executionStatus(goalId, "ONT::DONE");
			}
			
			if (currentGoal.isCompleted())
			{
				checkingForGoalCompletion = false;
				return GoalMessages.executionStatus(goalId, "ONT::DONE");
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
				return GoalMessages.executionStatus(goalId, "ONT::DONE");
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
				return GoalMessages.executionStatus(goalId, "ONT::DONE");
			}
			else if (goalInstanceType.equalsIgnoreCase("ONT::EVENTS-IN-MODEL"))
			{
				return GoalMessages.done(goalId);
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
						
						return GoalMessages.waitingForUser(currentGoal.getId());	
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
						return GoalMessages.proposeAdoptContent(goalToBuild.getId(), goalToBuild.getWhat(), asList, context);
					}
					else
					{
						return GoalMessages.waitingForUser(currentGoal.getId());
					}
				}
			}
			else if (goalInstanceType.equalsIgnoreCase("SHOW-EXAMPLE"))
			{
				return getModelBuilderGoal(context);
				//return GoalMessages.waitingForUser(currentGoal.getId());
			}
			else if (goalInstanceType.equalsIgnoreCase("ONT::DESCRIBE"))
			{
				//return GoalMessages.waitingForUser(currentGoal.getId());
				return getModelBuilderGoal(context);
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
			return GoalMessages.suggestSomething(context);
		}
		
		// The typical state for the latest version of the system
		if (goalState == GoalState.ACCEPTED && (systemState == SystemState.LEARNING_DEMONSTRATION ||
												systemState == SystemState.LEARNING_CONSTRAINTS))
		{
			return getModelBuilderGoal(context);
		}

		
		return GoalMessages.waitingForUser(currentGoal.getId());
	}

	
	private KQMLList getModelBuilderGoal(KQMLList context)
	{
		
		System.out.println("Getting model builder goal");
		KQMLList response = modelBuilder.whatNext(systemState, currentGoal);
		Goal newGoal = modelBuilder.getLastGoal();
		String currentGoalId = "NIL";
		if (currentGoal != null)
		{
			if (newGoal != null)
				newGoal.setParent(currentGoal);
			currentGoalId = currentGoal.getId();
		}
		if (newGoal == null)
		{
			return GoalMessages.waitingForUser(currentGoalId);
		}
		addGoal(newGoal);
		context.add(newGoal.getTerm());
		
		return response;
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
		
		KQMLList askWhContent = GoalMessages.askWhContent(queryId, 
				questionVariable, queryVariable, currentGoal.getId());
		
		return GoalMessages.propose(askWhContent,newContext);
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
		
		KQMLList adoptContent = 
				GoalMessages.askWhContent(queryId, questionVariable,
						queryVariable, currentGoal.getId());
		Goal askGoal = new Goal(questionVariable,queryId, adoptContent);
		addGoal(askGoal);
		return GoalMessages.propose(adoptContent, context);
		
	}
	
	/**
	 * This preemptively checks a model being built against the known model to suggest 
	 * that it may be done.
	 * @param s
	 */
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

		String lexName = affectedResultTerm.getKeywordArg(":LEX").stringValue();
		
		
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
			GoalMessages.waitingForUser(currentGoal.getId());
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
	

	public void examplesViolatedByNewConstraint(Constraint c)
	{
		lastConstraint = c;
		systemState = SystemState.CHECKING_CONSTRAINT;
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
		KQMLList askIfContent = GoalMessages.askIfContent(newQuery.getId(), 
				finished.get(1).stringValue(), currentGoal.getId());
		
		KQMLList responseContent = GoalMessages.propose(askIfContent, newContext);
		addGoal(newQuery);
		KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
		performativeToSend.setParameter(":RECEIVER", "DAGENT");
		System.out.println("Response content: " + responseContent);
		performativeToSend.setParameter(":CONTENT",responseContent);
		performativeToSend.setParameter(":IN-REPLY-TO", lastReplyWith);
		sriWrapper.sendKQMLPerformative(performativeToSend);
		
	}
	
	private void sendDone(String goalId)
	{
		KQMLList responseContent = GoalMessages.done(goalId);
		KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
		performativeToSend.setParameter(":RECEIVER", "DAGENT");
		System.out.println("Response content: " + responseContent);
		performativeToSend.setParameter(":CONTENT",responseContent);
		performativeToSend.setParameter(":IN-REPLY-TO", lastReplyWith);
		sriWrapper.sendKQMLPerformative(performativeToSend);
	}

	public Goal getTopLevelGoal() {
		return topLevelGoal;
	}

	public void setTopLevelGoal(Goal topLevelGoal) {
		this.topLevelGoal = topLevelGoal;
	}
	
	

	
}
