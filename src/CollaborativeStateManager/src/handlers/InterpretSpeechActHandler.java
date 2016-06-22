package handlers;

import extractors.EventExtractor;
import handlers.IDHandler;
import TRIPS.KQML.*;

public class InterpretSpeechActHandler {

	
	KQMLList innerContent = null;
	
	String speechAct;
	String what;
	KQMLPerformative msg;
	KQMLList content;
	
	KQMLObject context;

	KQMLObject whatLF = null;
	KQMLObject activeGoal;
	KQMLObject activeGoalWhat = null;



	public InterpretSpeechActHandler(KQMLPerformative msg, KQMLList content)
	{
		this.msg = msg;
		this.content = content;
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
		
		activeGoal = innerContent.getKeywordArg(":active-goal");
		if (!activeGoal.stringValue().equalsIgnoreCase("nil") &&
				!activeGoal.stringValue().equals("-"))
			activeGoalWhat = ((KQMLList)activeGoal).getKeywordArg(":WHAT");
		
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
		}
		
		return null;
	}
	
	private KQMLList handleAssertion()
	{
		EventExtractor ee = new EventExtractor();
		ee.apply((KQMLList)context);
		KQMLList newContextList = new KQMLList();
		
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
		contributesList.add(activeGoalWhat);
		assertionContent.add(contributesList);
		
		return reportContent(assertionContent, assertionEventContent);
		
	}
	
	private KQMLList handleEvaluateResult()
	{
		String evaluateId = IDHandler.getNewID();
		String causeEffectId = IDHandler.getNewID();
		KQMLList evaAdoptContent = adoptContent(evaluateId,"SUBGOAL",activeGoalWhat);
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
		if (activeGoalWhat == null)
			proposeAdoptContent = adoptContent(what,"GOAL",null);
		else
			proposeAdoptContent = adoptContent(what,"SUBGOAL",activeGoalWhat);
		return reportContent(proposeAdoptContent, context);
	}
	
	private KQMLList handleWhatIs()
	{
		KQMLList askAdoptContent;
		if (((KQMLList)whatLF).getKeywordArg(":instance-of").stringValue().
				equalsIgnoreCase("ONT:MEDICATION"))
			askAdoptContent = adoptContent(what,"SUBGOAL",activeGoalWhat);
		else
			askAdoptContent = adoptContent(what, "SUBGOAL", activeGoalWhat);
		
    	KQMLList askRelnContent = new KQMLList();
    	askRelnContent.add("ont::RELN");
    	askRelnContent.add(IDHandler.getNewID());
    	askRelnContent.add(":instance-of");
    	askRelnContent.add("ONT::IDENTIFY");
    	askRelnContent.add(":affected");
    	askRelnContent.add(what);
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
    

    
    private KQMLList reportContent(KQMLObject content, KQMLObject context)
    {
    	KQMLList reportContent = new KQMLList();
    	reportContent.add("REPORT");
    	reportContent.add(":content");
    	reportContent.add(content);
    	reportContent.add(":context");
    	reportContent.add(context);
    	
    	return reportContent;
    }
    
    private KQMLList adoptContent(String what, String goalType, KQMLObject subgoalOf)
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
