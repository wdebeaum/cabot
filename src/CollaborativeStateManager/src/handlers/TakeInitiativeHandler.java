package handlers;

import TRIPS.KQML.*;

public class TakeInitiativeHandler extends MessageHandler {


	
	public TakeInitiativeHandler(KQMLPerformative msg, KQMLList content)
	{
		super(msg,content);
	}
	
	public KQMLList process()
	{
		
		//KQMLList goal = (KQMLList)(content.getKeywordArg(":GOAL"));
		
		KQMLObject goalObject = content.getKeywordArg(":GOAL");
		KQMLObject context = content.getKeywordArg(":CONTEXT");
		if (context == null)
			context = new KQMLList();
		if (goalObject == null)
			return failureMessage(context);
		String goalWhat = goalObject.stringValue();
		
		KQMLList goalLF = null;
		for (KQMLObject lfTerm : (KQMLList)context)
		{
			if (((KQMLList)lfTerm).get(1).stringValue().equalsIgnoreCase(goalWhat))
				goalLF = (KQMLList)lfTerm;
		}
		if (goalLF == null)
			return failureMessage(context);
		
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		System.out.println("Goal type: *" + goalType + "*");
		KQMLList takeInitContent;
		String goalTypeUpper = goalType.toUpperCase();
		if (goalTypeUpper.contains("V32008"))
			takeInitContent = takeInitiativeContent("NO", goalWhat, context);
		else if (goalTypeUpper.contains("IDENTIFY") || 
				goalTypeUpper.contains("EVALUATE"))
			takeInitContent = takeInitiativeContent("YES", goalWhat, context);
		else
			takeInitContent = takeInitiativeContent("NO", goalWhat, context);
		
		return takeInitContent;
		
	}
	
    private KQMLList takeInitiativeContent(String result, String goal, KQMLObject context)
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
