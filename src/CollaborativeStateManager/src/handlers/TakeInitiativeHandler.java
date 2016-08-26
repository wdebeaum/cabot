package handlers;

import TRIPS.KQML.*;

import java.util.*;

import extractors.TermExtractor;
import plans.GoalPlanner;

public class TakeInitiativeHandler extends MessageHandler {

	private GoalPlanner goalPlanner;
	
	public TakeInitiativeHandler(KQMLPerformative msg, KQMLList content, GoalPlanner goalPlanner)
	{
		super(msg,content);
		this.goalPlanner = goalPlanner;
	}
	
	public List<KQMLList> process()
	{
		ArrayList<KQMLList> result = new ArrayList<KQMLList>();
		
		//KQMLList goal = (KQMLList)(content.getKeywordArg(":GOAL"));
		
		KQMLObject goalObject = content.getKeywordArg(":GOAL");
		KQMLObject context = content.getKeywordArg(":CONTEXT");
		if (context == null || context.stringValue().equalsIgnoreCase("NIL") || context.stringValue().equals("-"))
			context = new KQMLList();
		if (goalObject == null)
		{
			System.out.println("Goal parameter not set");
			return null;
		}
		String goalWhat = goalObject.stringValue();
		
		KQMLList goalLF = null;
		for (KQMLObject lfTerm : (KQMLList)context)
		{
			if (((KQMLList)lfTerm).get(1).stringValue().equalsIgnoreCase(goalWhat))
				goalLF = (KQMLList)lfTerm;
		}
		if (goalLF == null)
		{
			if (goalPlanner.hasGoal(goalWhat))
			{
				System.out.println("Searching goalplanner for variable: " + goalWhat);
				goalLF = goalPlanner.getGoal(goalWhat).getKQMLTerm();
			}
			else
			{
				System.out.println("No such goal in planner");
				return null;
			}
		}
		
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		System.out.println("Goal type: *" + goalType + "*");
		KQMLList takeInitContent = null;
		String goalTypeUpper = goalType.toUpperCase();
		if (goalPlanner.getPrivateGoal() != null && goalPlanner.getPrivateGoal().getVariableName().equalsIgnoreCase(goalWhat))
			takeInitContent = takeInitiativeContent("YES", goalWhat, context);
		else if (goalTypeUpper.contains("IDENTIFY") || 
				goalTypeUpper.contains("EVALUATE") || goalTypeUpper.contains("QUERY"))
			takeInitContent = takeInitiativeContent("YES", goalWhat, context);
		else
		{
			KQMLObject agentObject = goalLF.getKeywordArg(":AGENT");

			if (agentObject != null)
			{
				String agent = agentObject.stringValue();
				KQMLList agentTerm = TermExtractor.extractTerm(agent, (KQMLList)context);
				KQMLObject agentEquals = agentTerm.getKeywordArg(":REFERS-TO");
				if (agentEquals != null)
				{
					// The user gave the system a command
					if (agentEquals.stringValue().equalsIgnoreCase("ONT::SYS"))
						takeInitContent = takeInitiativeContent("YES", goalWhat, context);
					// The user said "we" or "us" - continue with previous initiative
					else if (agentEquals.stringValue().equalsIgnoreCase("ONT::US"))
					{
						if (goalPlanner.isGlobalSystemInitiative())
							takeInitContent = takeInitiativeContent("YES", goalWhat, context);
						else
							takeInitContent = takeInitiativeContent("YES", goalWhat, context);
					}
					else if (agentEquals.stringValue().equalsIgnoreCase("ONT::USER"))
					{
						if (goalPlanner.isGlobalSystemInitiative())
							takeInitContent = takeInitiativeContent("NO", goalWhat, context);
						else
							takeInitContent = takeInitiativeContent("NO", goalWhat, context);
					}
						
					
				}
			}
			
		}
		if (takeInitContent == null)
			takeInitContent = takeInitiativeContent("NO", goalWhat, context);
		
		result.add(takeInitContent);
		return result;
		
	}
	
    private KQMLList takeInitiativeContent(String result, String goal, KQMLObject context)
    {
    	KQMLList initContent = new KQMLList();
    	initContent.add("TAKE-INITIATIVE");
    	initContent.add(":result");
    	initContent.add(result);
    	if (result.equalsIgnoreCase("YES"))
    	{
    		goalPlanner.setGlobalSystemInitiative(true);
    		goalPlanner.getGoal(goal).setSystemTookInitiative(true);
    	}
    	else
    	{
    		goalPlanner.setGlobalSystemInitiative(false);
    		goalPlanner.getGoal(goal).setSystemTookInitiative(false);
    	}
    	initContent.add(":goal");
    	initContent.add(goal);
    	initContent.add(":context");
    	initContent.add(context);
    	return initContent;
    }
}
