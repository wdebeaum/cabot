package handlers;

import java.util.ArrayList;
import java.util.List;

import plans.ActionPlanner;
import plans.GoalPlanner;
import states.Action;
import states.Goal;
import utilities.KQMLUtilities;
import extractors.EventExtractor;
import extractors.OntologyReader;
import extractors.TermExtractor;
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
	
	public List<KQMLList> process()
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
			goalPlanner.setActiveGoal(activeGoal);
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
		case "ont::ask-conditional-if":
			return handleAskConditionalIf();
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
	
	private KQMLList missingGoalToModify()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL-TO-MODIFY");

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		KQMLList solutionList = new KQMLList();
		KQMLList selectList = new KQMLList();
		selectList.add("SELECT-GOAL-TO-MODIFY");
		
		solutionList.add(selectList);

		
		return failureMessage(what, newContext,failureReason, solutionList);
	}
	
	private List<KQMLList> handleAssertion()
	{
		
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		if (activeGoal == null && currentAcceptedGoal == null)
		{
			ArrayList<KQMLList> result = new ArrayList<KQMLList>();
			result.add(missingActiveGoal());
			return result;
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
		KQMLList assertionEventContext = new KQMLList();
		assertionEventContext.add(assertionRelnContent);
		assertionEventContext.addAll((KQMLList)context);
		
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
		
		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(assertionContent, assertionEventContext));
		return result;
		
	}
	
	private List<KQMLList> handleEvaluateResult()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		if (activeGoal == null && currentAcceptedGoal == null)
		{
			ArrayList<KQMLList> result = new ArrayList<KQMLList>();
			result.add(missingActiveGoal());
			return result;
		}
		
		// TODO: Get test parameter
		
		String evaluateId = IDHandler.getNewID();
		String causeEffectId = IDHandler.getNewID();
		KQMLList evaAdoptContent = adoptContent(evaluateId,"SUBGOAL",activeGoal);
		KQMLList evaReln = new KQMLList();
		evaReln.add("ont::RELN");
		evaReln.add(evaluateId);
		evaReln.add(":INSTANCE-OF");
		evaReln.add("ONT::EVALUATE");
		evaReln.add(":content");
		evaReln.add(causeEffectId);
		
		KQMLList causeReln = new KQMLList();
		causeReln.add("ont::RELN");
		causeReln.add(causeEffectId);
		causeReln.add(":INSTANCE-OF");
		causeReln.add("ONT::CAUSE");
		causeReln.add(":action");
		

		causeReln.add(":result");
		causeReln.add(what);
		
		KQMLList evaReportContext = new KQMLList();
		evaReportContext.add(evaReln);
		evaReportContext.add(causeReln);
		evaReportContext.addAll((KQMLList)context);
		
		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(evaAdoptContent, evaReportContext));
		return result;
		
	}
	
	// TODO: Deal with edge cases
	private List<KQMLList> handlePropose()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		KQMLList proposeAdoptContent = null;
		
		KQMLObject asObject = innerContent.getKeywordArg(":AS");
		KQMLList asList = null;
		if (asObject != null && asObject instanceof KQMLList)
		{
			asList = (KQMLList)asObject;
			String asType = asList.get(0).stringValue().toUpperCase();
			
			switch (asType)
			{
			case "MODIFY":
				if (activeGoal == null && currentAcceptedGoal == null)
				{
					ArrayList<KQMLList> result = new ArrayList<KQMLList>();
					result.add(missingActiveGoal());
					return result;
				}
				proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context));
				if (proposeAdoptContent == null)
				{
					ArrayList<KQMLList> result = new ArrayList<KQMLList>();
					result.add(missingGoalToModify());
					return result;
				}
				break;
			}
		}
		else if (activeGoal == null)
		{
			proposeAdoptContent = adoptContent(what,"GOAL",null);
			goalPlanner.addGoal(new Goal(what,(KQMLList)context));
		}
		else
		{
			proposeAdoptContent = adoptContent(what,"SUBGOAL",activeGoal);
			goalPlanner.addGoal(new Goal(what,(KQMLList)context), activeGoal);
		}

		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(proposeAdoptContent, context));
		return result;
	}
	
	private List<KQMLList> handleWhatIs()
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
    	
    	KQMLList askWhatIsContent = new KQMLList();
    	askWhatIsContent.add("ASK-WHAT-IS");
    	askWhatIsContent.add(":what");
    	askWhatIsContent.add(what);
    	askWhatIsContent.add(":as");
    	
    	
    	KQMLList queryInContext = new KQMLList();
    	queryInContext.add("QUERY-IN-CONTEXT");
    	queryInContext.add(":goal");
    	queryInContext.add(newId);
    	
    	askWhatIsContent.add(queryInContext);
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(askRelnContent);
    	contextToSend.addAll((KQMLList)context);

		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(askAdoptContent, contextToSend));
		//result.add(reportContent(askWhatIsContent, contextToSend));
		return result;
	}
	
	// TODO Change event to conditional
	private List<KQMLList> handleAskConditionalIf()
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
		
		
    	KQMLList conditionalContent = new KQMLList();
    	conditionalContent.add("ont::RELN");
    	String newConditionalId = IDHandler.getNewID();
    	conditionalContent.add(newConditionalId);
    	conditionalContent.add(":instance-of");
    	// TODO Determine if causal or conditional
    	conditionalContent.add("CONDITIONAL");
    	

    	KQMLObject conditionObject = innerContent.getKeywordArg(":CONDITION");
    	
    	if (!KQMLUtilities.isKQMLNull(conditionObject))
    	{
    		conditionalContent.add(":factor");
    		String condition = conditionObject.stringValue();
    		conditionalContent.add(condition);
    		
    	}
    	
    	conditionalContent.add(":OUTCOME");
    	conditionalContent.add(what);
    	
    	KQMLList queryGoalContent = new KQMLList();
    	queryGoalContent.add("ont::RELN");
    	queryGoalContent.add(newId);
    	queryGoalContent.add(":instance-of");
    	queryGoalContent.add("ONT::EVALUATE");
    	queryGoalContent.add(":neutral");
    	queryGoalContent.add(newConditionalId);

    	if (activeGoal == null)
    	{
    		goalPlanner.addGoal(new Goal(queryGoalContent));
    	}
    	else
    	{
    		goalPlanner.addGoal(new Goal(queryGoalContent, goalPlanner.getGoal(activeGoal)));
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(queryGoalContent);
    	contextToSend.add(conditionalContent);
    	contextToSend.addAll((KQMLList)context);

		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(askAdoptContent, contextToSend));
		
		//result.add(reportContent(askRelnContent, contextToSend));
		
		
		return result;
	}
	
	private List<KQMLList> handleAskIf()
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
    	askRelnContent.add("ASK-IF");
    	askRelnContent.add(":what");
    	askRelnContent.add(what);
    	askRelnContent.add(":as");
    	
    	
    	KQMLList queryInContext = new KQMLList();
    	queryInContext.add("QUERY-IN-CONTEXT");
    	queryInContext.add(":goal");
    	queryInContext.add(newId);
    	
    	askRelnContent.add(queryInContext);
    	
    	KQMLList queryGoalContent = new KQMLList();
    	queryGoalContent.add("ont::RELN");
    	queryGoalContent.add(newId);
    	queryGoalContent.add(":instance-of");
    	queryGoalContent.add("ONT::EVALUATE");
    	queryGoalContent.add(":neutral");
    	queryGoalContent.add(what);

    	if (activeGoal == null)
    	{
    		goalPlanner.addGoal(new Goal(queryGoalContent));
    	}
    	else
    	{
    		goalPlanner.addGoal(new Goal(queryGoalContent, goalPlanner.getGoal(activeGoal)));
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(queryGoalContent);
    	contextToSend.addAll((KQMLList)context);

		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		result.add(reportContent(askAdoptContent, contextToSend));
		
		//result.add(reportContent(askRelnContent, contextToSend));
		
		
		return result;
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
    	if (!goalType.equalsIgnoreCase("GOAL"))
    	{
    		goal.add(":of");
    		goal.add(subgoalOf);
    	}
    	adopt.add(goal);
    	
    	return adopt;
    }
}
