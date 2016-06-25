package handlers;

import TRIPS.KQML.*;

public class TakeInitiativeHandler {

	KQMLPerformative msg;
	KQMLList content;
	
	public TakeInitiativeHandler(KQMLPerformative msg, KQMLList content)
	{
		this.msg = msg;
		this.content = content;
	}
	
	public KQMLList process()
	{
		
		KQMLList goal = (KQMLList)(content.getKeywordArg(":GOAL"));
		String goalWhat = goal.getKeywordArg(":WHAT").stringValue();
		KQMLObject context = content.getKeywordArg(":CONTEXT");
		KQMLList goalLF = null;
		for (KQMLObject lfTerm : (KQMLList)context)
		{
			if (((KQMLList)lfTerm).get(1).stringValue().equalsIgnoreCase(goalWhat))
				goalLF = (KQMLList)lfTerm;
		}
		if (goalLF == null)
			return null;
		
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		System.out.println("Goal type: *" + goalType + "*");
		KQMLList takeInitContent;
		String goalTypeUpper = goalType.toUpperCase();
		if (goalTypeUpper.contains("V32008"))
			takeInitContent = takeInitiativeContent("NO", goal, context);
		else if (goalTypeUpper.contains("IDENTIFY") || 
				goalTypeUpper.contains("EVALUATE"))
			takeInitContent = takeInitiativeContent("YES", goal, context);
		else
			takeInitContent = takeInitiativeContent("NO", goal, context);
		
		return takeInitContent;
		
	}
	
    private KQMLList takeInitiativeContent(String result, KQMLList goal, KQMLObject context)
    {
    	KQMLList initContent = new KQMLList();
    	initContent.add("TAKE-INITIATIVE");
    	initContent.add(":result");
    	initContent.add(result);
    	initContent.add(":goal");
    	initContent.add(goal);
    	initContent.add(":context");
    	initContent.add(context);
    	return initContent;
    }
}
