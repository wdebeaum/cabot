package handlers;

import java.util.ArrayList;
import java.util.List;

import plans.GoalPlanner;
import states.Action;
import states.Elaboration;
import states.Goal;
import states.Query;
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
	String query;
	KQMLObject context;

	KQMLObject whatLF = null;
	String activeGoal = null;
	OntologyReader ontologyReader;
	GoalPlanner goalPlanner;


	public InterpretSpeechActHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
										GoalPlanner goalPlanner, 
										OntologyReader ontologyReader)
	{
		super(msg,content, referenceHandler);
		
		this.ontologyReader = ontologyReader;
		this.goalPlanner = goalPlanner;
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
		KQMLObject queryObject = innerContent.getKeywordArg(":QUERY");
		if (queryObject != null)
			query = queryObject.stringValue();
		
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
		case "answer":
			return handleAnswer();
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
		case "acceptable":
			return handleAcceptable();
		case "not-acceptable":
			return handleNotAcceptable();
		}
		
		System.out.println("Unrecognized speech act: " + speechAct);
		return null;
	}
	
	private KQMLList handleAnswer()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
		{
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal();
		}
		
		Query queryToAnswer = null;
		for (Query q : goalPlanner.getQueries())
		{
			if (q.getParent() != null && q.getParent().equals(currentAcceptedGoal))
			{
				queryToAnswer = q;
				break;
			}
		}
		
		if (queryToAnswer == null)
			return missingQueryToAnswer();
		
		KQMLList eventTerm = TermExtractor.extractTerm(what, (KQMLList)context);
		KQMLObject typeObject = eventTerm.getKeywordArg(":INSTANCE-OF");
		KQMLObject neutralObject = eventTerm.getKeywordArg(":NEUTRAL");
		KQMLObject agentObject = eventTerm.getKeywordArg(":AGENT");
		KQMLObject refersToObject = eventTerm.getKeywordArg(":REFERS-TO");
		
		String initiativeAgent = null;
		
//		if (agentObject != null)
//		{
//			initiativeAgent = agentObject.stringValue();
//		}
//		else if (neutralObject != null)
//		{
//			initiativeAgent = neutralObject.stringValue();
//		}

//		System.out.println("Agent: " + initiativeAgent);
        
		//Goal replacementGoal = new Goal(currentAcceptedGoal);
		Elaboration elaboration = new Elaboration(eventTerm, (KQMLList)context);
		referenceHandler.addReference(elaboration.getKQMLTerm());
        //referenceHandler.addReference(replacementGoal.getKQMLTerm());
		//replacementGoal.setInitiativeAgent(initiativeAgent, (KQMLList)context);
		//KQMLList answer = goalPlanner.modify(elaboration,
			//	currentAcceptedGoal.getVariableName());
		KQMLList answerContent = queryToAnswer.answerContent(what, (KQMLList)context);
		
		
		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());
//        if (replacementGoal != null)
//        {
//            
//            KQMLList replacementContext = referenceHandler.generateContextForTerm(replacementGoal.getKQMLTerm());
//            
//            newContext.addAll(replacementContext);
//            
//            newContext.addAll(replacementGoal.getOriginalContext());
//        }
		
        if (elaboration != null)
        {
            
            KQMLList replacementContext = referenceHandler.generateContextForTerm(elaboration.getKQMLTerm());
            
            newContext.addAll(replacementContext);
            
            newContext.addAll(elaboration.getOriginalContext());
        }
        
        newContext.addAll(queryToAnswer.getOriginalContext());
		return reportContent(answerContent, newContext);

	}
	
	private KQMLList handleNotAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList handleAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList missingActiveGoal(String attemptedGoalType)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");
		List<String> parentGoalTypes = ontologyReader.getParentGoals(attemptedGoalType);
		
		if (parentGoalTypes == null)
			return missingActiveGoal();
		
		List<Goal> possibleSolutionGoals =
				goalPlanner.generatePossibleGoals(parentGoalTypes);
		
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
			// Temporary? Only add one 
			break;
		}
		
		return failureMessage(what, newContext,failureReason, adoptContentList);
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
			// Temporary? Only add one 
			break;
		}
		
		return failureMessage(what, newContext,failureReason, adoptContentList);
	}
	
	private KQMLList missingGoal()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");
	
		return failureMessage(what, context,failureReason);
	}
	
	private KQMLList missingQueryToAnswer()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-QUERY-TO-ANSWER");
	
		return failureMessage(what, context,failureReason);
	}
	
	private KQMLList missingGoal(String otherWhat)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");
	
		return failureMessage(otherWhat, context,failureReason);
	}
	
	private KQMLList noEventsInContext()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("NO-EVENTS-IN-CONTEXT");
		
		return failureMessage(what,context,failureReason);
	}

	
	private KQMLList handleAssertion()
	{
		
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
		{
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal();
		}
		
		System.out.println("Active goal: " + activeGoal + " at time of assertion");
		
		EventExtractor ee = new EventExtractor(ontologyReader);
		ee.apply((KQMLList)context);
		if (ee.getEventIDsInContext().isEmpty())
		{
			return noEventsInContext();
		}
		
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
		if (currentAcceptedGoal != null)
			assertionEventContext.addAll(currentAcceptedGoal.getAdditionalContext());
		
		KQMLList assertionContent = new KQMLList();
		assertionContent.add("ASSERTION");
		assertionContent.add(":what");
		assertionContent.add(ee.getID());
		
		KQMLList contributesList = new KQMLList();
		
		Goal contributesToGoal = goalPlanner.getGoal(activeGoal);
		System.out.println("ContributesTo: " + contributesToGoal);

		if (contributesToGoal instanceof Action)
		{
			System.out.println("Checking for better goal to contribute to");
			contributesToGoal = 
					goalPlanner.getNonActionAncestor(activeGoal);
			System.out.println("ContributesTo: " + contributesToGoal);
		}
		if (contributesToGoal != null)
		{
			assertionContent.add(":as");
			contributesList.add("CONTRIBUTES-TO");
			contributesList.add(":goal");
			contributesList.add(contributesToGoal.getVariableName());
			assertionContent.add(contributesList);
		}
		
		Action action = new Action(assertionRelnContent, new KQMLList());
		action.setActionType("ASSERTION");
		if (contributesToGoal != null)
			action.setContributesTo(contributesToGoal);
		goalPlanner.addGoal(action,contributesToGoal.getVariableName());
		
		return reportContent(assertionContent, assertionEventContext);
		
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
		//causeReln.add()
		

		causeReln.add(":result");
		causeReln.add(what);
		
		KQMLList evaReportContext = new KQMLList();
		evaReportContext.add(evaReln);
		evaReportContext.add(causeReln);
		evaReportContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			evaReportContext.addAll(currentAcceptedGoal.getAdditionalContext());

		return reportContent(evaAdoptContent, evaReportContext);
		
	}
	
	// TODO: Deal with edge cases
	private KQMLList handlePropose()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		KQMLList proposeAdoptContent = null;
        Goal replacementGoal = null;
		
		// The system wants to rollback a goal
		if (what != null)
		{
			KQMLList term = TermExtractor.extractTerm(what, (KQMLList)context);
            System.out.println("Checking goal " + what);
			if (term != null)
			{
				if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::ROLLBACK"))
				{
					goalPlanner.rollback();
				}
				else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::RESTART"))
				{
					goalPlanner.startOver();
				}
				else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::EXECUTE"))
				{
					// Now we'll just return it as an elaboration of the goal under discussion
					
//                    System.out.println("Execute message found. Changing initiative.");
//					KQMLObject agentSymbolObject = term.getKeywordArg(":AGENT");
//                    
//					if (agentSymbolObject != null)
//					{
//						String agentSymbol = agentSymbolObject.stringValue();
//						System.out.println("Agent: " + agentSymbol);
//                        Goal goalToModify = goalPlanner.getGoalUnderDiscussion();
//						replacementGoal = new Goal(goalToModify);
//                        referenceHandler.addReference(replacementGoal.getKQMLTerm());
//						replacementGoal.setInitiativeAgent(agentSymbol, (KQMLList)context);
//						proposeAdoptContent = goalPlanner.modify(replacementGoal,goalToModify.getVariableName());
//					}
					
					proposeAdoptContent = adoptContent(what,"ELABORATION",goalPlanner.getGoalUnderDiscussion().getVariableName());
				}
			}
			
		}
		if (proposeAdoptContent == null)
		{
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
						return missingActiveGoal();
					}
					proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context));
					if (proposeAdoptContent == null)
					{
						return missingGoalToModify(what,context);
					}
					break;
				case "ANSWER":
					KQMLObject toObject = asList.getKeywordArg(":TO");
					if (toObject != null)
					{
						if (!goalPlanner.hasGoal(toObject.stringValue()))
							return missingGoal(toObject.stringValue());
						goalPlanner.setCompleted(goalPlanner.getGoal(toObject.stringValue()));
						proposeAdoptContent = answerContent(what, toObject.stringValue());
					}
					else if (activeGoal != null)
					{
						
						goalPlanner.setCompleted(goalPlanner.getActiveGoal());
						proposeAdoptContent = answerContent(what, activeGoal);
						
					}
					else
					{
						return missingActiveGoal();
					}
					break;
					
				}
			}
			else if (activeGoal == null || currentAcceptedGoal == null)
			{
				KQMLList term = TermExtractor.extractTerm(what, (KQMLList)context);
				String goalType = term.getKeywordArg(":INSTANCE-OF").stringValue();
				if (!ontologyReader.isRootGoal(goalType))
					return missingActiveGoal(goalType);
				proposeAdoptContent = adoptContent(what,"GOAL",null);
				goalPlanner.addGoal(new Goal(what,(KQMLList)context));
			}
			else if (currentAcceptedGoal.isFailed())
			{
				proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context));
				if (proposeAdoptContent == null)
				{
					return missingGoalToModify(what,context);
				}
			}
			else // Currently adds as subgoal by default
			{
				proposeAdoptContent = adoptContent(what,"SUBGOAL",activeGoal);
				goalPlanner.addGoal(new Goal(what,(KQMLList)context), activeGoal);
			}
		}

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());
        if (replacementGoal != null)
        {
            
            KQMLList replacementContext = referenceHandler.generateContextForTerm(replacementGoal.getKQMLTerm());
            
            newContext.addAll(replacementContext);
            newContext.addAll(replacementGoal.getOriginalContext());
        }
		return reportContent(proposeAdoptContent, newContext);
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
    		Goal newGoal = new Goal(askRelnContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal);
    		askAdoptContent = adoptContent(newId, "GOAL", null);
    		
    	}
    	else
    	{
    		Goal newGoal = new Goal(askRelnContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal, activeGoal);
    		askAdoptContent = adoptContent(newId, "SUBGOAL", activeGoal);
    	}
    	
    	KQMLList askWhatIsContent = new KQMLList();
    	askWhatIsContent.add("ASK-WHAT-IS");
    	askWhatIsContent.add(":QUERY");
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
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askWhatIsContent, contextToSend));
		return reportContent(askAdoptContent, contextToSend);
	}
	
	// TODO Change event to conditional
	private KQMLList handleAskConditionalIf()
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
    	conditionalContent.add("ONT::COND");
    	

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
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal);
    	}
    	else
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal, activeGoal);
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(queryGoalContent);
    	contextToSend.add(conditionalContent);
    	contextToSend.addAll((KQMLList)context);
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askRelnContent, contextToSend));
		
		
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
		{
			//askAdoptContent = adoptContent(newId, "GOAL", null);
			return missingActiveGoal();
		}
		
		
    	KQMLList askRelnContent = new KQMLList();
    	askRelnContent.add("ASK-IF");
    	askRelnContent.add(":QUERY");
    	askRelnContent.add(what);
    	askRelnContent.add(":AS");
    	
    	
    	KQMLList queryInContext = new KQMLList();
    	queryInContext.add("QUERY-IN-CONTEXT");
    	queryInContext.add(":goal");
    	queryInContext.add(newId);
    	
    	askRelnContent.add(queryInContext);
    	
    	KQMLList queryGoalContent = new KQMLList();
    	queryGoalContent.add("ont::RELN");
    	queryGoalContent.add(newId);
    	queryGoalContent.add(":instance-of");
    	queryGoalContent.add("ONT::QUERY-MODEL");
    	queryGoalContent.add(":neutral");
    	queryGoalContent.add(what);

    	if (activeGoal == null)
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal);
    	}
    	else
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal, activeGoal);
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(queryGoalContent);
    	contextToSend.addAll((KQMLList)context);
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askRelnContent, contextToSend));
		
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
    	if (!goalType.equalsIgnoreCase("GOAL"))
    	{
    		goal.add(":of");
    		goal.add(subgoalOf);
    	}
    	adopt.add(goal);
    	
    	return adopt;
    }
    
    private KQMLList answerContent(String what, String query)
    {
    	KQMLList answer = new KQMLList();
    	
    	answer.add("ADOPT");
    	answer.add(":what");
    	answer.add(what);
    	answer.add(":as");
    	
    	KQMLList goal = new KQMLList();
    	goal.add("ANSWER");
    	goal.add(":TO");
    	goal.add(query);
    	
    	answer.add(goal);
    	
    	return answer;
    }
}