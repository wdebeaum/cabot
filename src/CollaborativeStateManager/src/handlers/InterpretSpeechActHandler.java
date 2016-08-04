package handlers;

import java.util.List;

import plans.ActionPlanner;
import plans.GoalPlanner;
import states.Action;
import states.Goal;
import extractors.EventExtractor;
import extractors.OntologyReader;
import handlers.IDHandler;
import TRIPS.KQML.*;

public class InterpretSpeechActHandler extends MessageHandler{

	
	KQMLList innerContent = null;
	
	String speechAct;
	String what;
	
	KQMLObject context;

	KQMLObject whatLF = null;
	String activeGoal = null;
	OntologyReader ontologyReader;
	GoalPlanner goalPlanner;
	ActionPlanner actionPlanner;


	public InterpretSpeechActHandler(KQMLPerformative msg, KQMLList content,
										GoalPlanner goalPlanner, 
										OntologyReader ontologyReader,
										ActionPlanner actionPlanner)
	{
		super(msg,content);
		
		this.ontologyReader = ontologyReader;
		this.goalPlanner = goalPlanner;
		this.actionPlanner = actionPlanner;
	}
	
	public KQMLList process()
	{
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;
		
		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList)innerContentObj;
		
		speechAct = innerContent.get(0).stringValue();
		what = innerContent.getKeywordArg(":content").stringValue();
		context = innerContent.getKeywordArg(":context");	
		
		KQMLObject activeGoalObject = innerContent.getKeywordArg(":active-goal");
		
		if ((activeGoalObject != null) &&
		    !activeGoalObject.stringValue().equalsIgnoreCase("nil") &&
		    !activeGoalObject.stringValue().equals("-"))
		    {
			activeGoal = activeGoalObject.stringValue();
		    }
		for (KQMLObject lfTerm : (KQMLList)context)
		{
			if (((KQMLList)lfTerm).get(1).stringValue().equalsIgnoreCase(what))
				whatLF = lfTerm;
		}
		
		switch (speechAct.toLowerCase())
		{
		case "propose":
			return handlePropose();
		case "ont::ask-what-is":
			return handleWhatIs();
		case "ont::evaluate-result":
			return handleEvaluateResult();
		case "assertion":
			return handleAssertion();
		case "ont::ask-if":
			return handleAskIf();
		}
		
		System.out.println("Unrecognized speech act: " + speechAct);
		return null;
	}
	
	private KQMLList missingActiveGoal()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");
		
		
		List<Goal> possibleSolutionGoals =
				goalPlanner.generatePossibleGoals(ontologyReader.getRootGoals());
		
		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		KQMLList adoptContentList = new KQMLList();
		for (Goal possibleSolutionGoal : possibleSolutionGoals)
		{
			System.out.println("Possible solution: " + possibleSolutionGoal.getKQMLTerm());
			newContext.add(possibleSolutionGoal.getKQMLTerm());
			KQMLList adoptContent = adoptContent(possibleSolutionGoal.getVariableName(),
					"GOAL",null);
			System.out.println("Content: " + adoptContent);
			adoptContentList.add(adoptContent);
		}
		
		return failureMessage(what, newContext,failureReason, adoptContentList);
	}
	
	private KQMLList handleAssertion()
	{
		
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal();
		}
		
		EventExtractor ee = new EventExtractor(ontologyReader);
		ee.apply((KQMLList)context);
		
		KQMLList assertionRelnContent = new KQMLList();
		assertionRelnContent.add("ont::RELN");
		assertionRelnContent.add(ee.getID());
		assertionRelnContent.add(":instance-of");
		assertionRelnContent.add("ONT::EVENTS-IN-MODEL");
		assertionRelnContent.add(":events");
		assertionRelnContent.add(ee.getEventIDsInContext());
		KQMLList assertionEventContent = new KQMLList();
		assertionEventContent.add(assertionRelnContent);
		assertionEventContent.addAll((KQMLList)context);
		
		KQMLList assertionContent = new KQMLList();
		assertionContent.add("ASSERTION");
		assertionContent.add(":what");
		assertionContent.add(ee.getID());
		assertionContent.add(":as");
		KQMLList contributesList = new KQMLList();
		contributesList.add("CONTRIBUTES-TO");
		contributesList.add(":goal");
		contributesList.add(activeGoal);
		assertionContent.add(contributesList);
		
		Action action = new Action(ee.getID());
		if (goalPlanner.getGoal(activeGoal) != null)
			action.setContributesTo(goalPlanner.getGoal(activeGoal));
		actionPlanner.addAction(action);
		
		
		
		return reportContent(assertionContent, assertionEventContent);
		
	}
	
	private KQMLList handleEvaluateResult()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal();
		}
		
		String evaluateId = IDHandler.getNewID();
		String causeEffectId = IDHandler.getNewID();
		KQMLList evaAdoptContent = adoptContent(evaluateId,"SUBGOAL",activeGoal);
		KQMLList evaReln = new KQMLList();
		evaReln.add("ont::RELN");
		evaReln.add(evaluateId);
		evaReln.add("instance-of");
		evaReln.add("ONT::EVALUATE");
		evaReln.add(":content");
		evaReln.add(causeEffectId);
		
		KQMLList causeReln = new KQMLList();
		causeReln.add("ont::RELN");
		causeReln.add(causeEffectId);
		causeReln.add("instance-of");
		causeReln.add("ONT::CAUSE-EFFECT");
		causeReln.add(":action");
		causeReln.add("nil");
		causeReln.add(":result");
		causeReln.add(what);
		
		KQMLList evaReportContext = new KQMLList();
		evaReportContext.add(evaReln);
		evaReportContext.add(causeReln);
		evaReportContext.addAll((KQMLList)context);
		
		return reportContent(evaAdoptContent, evaReportContext);
		
	}
	
	private KQMLList handlePropose()
	{
		KQMLList proposeAdoptContent;
		if (activeGoal == null)
		{
			proposeAdoptContent = adoptContent(what,"GOAL",null);
			goalPlanner.addGoal(new Goal(what,(KQMLList)context));
		}
		else
		{
			proposeAdoptContent = adoptContent(what,"SUBGOAL",activeGoal);
			goalPlanner.addGoal(new Goal(what,(KQMLList)context), activeGoal);
		}
		return reportContent(proposeAdoptContent, context);
	}
	
	private KQMLList handleWhatIs()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
//		if (activeGoal == null && currentAcceptedGoal == null)
//		{
//			return missingActiveGoal();
//		}
		
		String newId = IDHandler.getNewID();
		KQMLList askAdoptContent;
//		if (((KQMLList)whatLF).getKeywordArg(":instance-of").stringValue().
//				equalsIgnoreCase("ONT:MEDICATION"))
//			askAdoptContent = adoptContent(newId,"SUBGOAL",activeGoal);
//		else
		
		
		
    	KQMLList askRelnContent = new KQMLList();
    	askRelnContent.add("ont::RELN");
    	askRelnContent.add(newId);
    	askRelnContent.add(":instance-of");
    	askRelnContent.add("ONT::IDENTIFY");
    	askRelnContent.add(":neutral");
    	askRelnContent.add(what);

    	if (activeGoal == null)
    	{
    		goalPlanner.addGoal(new Goal(askRelnContent));
    		askAdoptContent = adoptContent(newId, "GOAL", null);
    		
    	}
    	else
    	{
    		goalPlanner.addGoal(new Goal(askRelnContent, goalPlanner.getGoal(activeGoal)));
    		askAdoptContent = adoptContent(newId, "SUBGOAL", activeGoal);
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(askRelnContent);
    	contextToSend.addAll((KQMLList)context);
    	return reportContent(askAdoptContent, contextToSend);
	}
	
	private KQMLList handleAskIf()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		String newId = IDHandler.getNewID();
		KQMLList askAdoptContent;
		if (activeGoal != null)
			askAdoptContent = adoptContent(newId, "SUBGOAL", activeGoal);
		else
			askAdoptContent = adoptContent(newId, "GOAL", null);
		
		
    	KQMLList askRelnContent = new KQMLList();
    	//askRelnContent.add("ont::RELN");
    	//askRelnContent.add(newId);
    	//askRelnContent.add(":instance-of");
    	askRelnContent.add("ASK-IF");
    	askRelnContent.add(":what");
    	askRelnContent.add(what);
    	askRelnContent.add(":as");
    	
    	
    	KQMLList queryInContext = new KQMLList();
    	queryInContext.add("QUERY-IN-CONTEXT");
    	queryInContext.add(":goal");
    	queryInContext.add(newId);
    	
    	askRelnContent.add(queryInContext);

//    	if (activeGoal == null)
//    	{
//    		goalPlanner.addGoal(new Goal(askRelnContent));
//    	}
//    	else
//    	{
//    		goalPlanner.addGoal(new Goal(askRelnContent, goalPlanner.getGoal(activeGoal)));
//    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(askRelnContent);
    	contextToSend.addAll((KQMLList)context);
    	return reportContent(askAdoptContent, contextToSend);		
	}

	
	
    private KQMLList relnContent(String id, String instanceOf, String what, KQMLList context)
    {
    	KQMLList relnContent = new KQMLList();
    	relnContent.add("ont::RELN");
    	relnContent.add(id);
    	relnContent.add(":instance-of");
    	relnContent.add(instanceOf);
    	relnContent.add(":content");
    	return relnContent;
    }
    

    

    
    private KQMLList adoptContent(String what, String goalType, String subgoalOf)
    {
    	KQMLList adopt = new KQMLList();
    	
    	adopt.add("ADOPT");
    	adopt.add(":what");
    	adopt.add(what);
    	adopt.add(":as");
    	
    	KQMLList goal = new KQMLList();
    	goal.add(goalType);
    	if (goalType.equalsIgnoreCase("SUBGOAL"))
    	{
    		goal.add(":of");
    		goal.add(subgoalOf);
    	}
    	adopt.add(goal);
    	
    	return adopt;
    }
}
