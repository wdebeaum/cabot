package handlers;

import java.util.List;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;

public abstract class MessageHandler {

	KQMLPerformative msg;
	KQMLList content;
	
	
	public MessageHandler(KQMLPerformative msg, KQMLList content)
	{
		this.msg = msg;
		this.content = content;
	}
	
	public abstract List<KQMLList> process();
	
	public KQMLList failureMessage(String what, KQMLObject context)
	{
		return failureMessage(what,context,null);
	}
	
	public KQMLList failureMessage(String what,KQMLObject context, KQMLObject reason, KQMLObject possibleSolutions)
	{
		
		KQMLList failureContent = new KQMLList();
		failureContent.add("FAILURE");
		failureContent.add(":type");
		failureContent.add("FAILED-TO-INTERPRET");
		failureContent.add(":WHAT");
		failureContent.add(what);
		failureContent.add(":REASON");
		if (reason == null)
			failureContent.add("NIL");
		else
			failureContent.add(reason);
		
		failureContent.add(":POSSIBLE-RESOLUTION");
		if (possibleSolutions == null)
			failureContent.add("NIL");
		else
			failureContent.add(possibleSolutions);
		
		
		return reportContent(failureContent, context);		
	}
	
	public KQMLList failureMessage(String what, KQMLObject context, KQMLObject reason)
	{
		return failureMessage(what,context,reason,null);	
	}
	
	public KQMLList failureMessage(KQMLObject context)
	{
		return failureMessage("NIL", context);
	}
	
	public KQMLList failureMessage()
	{
		return failureMessage(new KQMLList());
	}
	
    protected KQMLList reportContent(KQMLObject content, KQMLObject context)
    {
    	KQMLList reportContent = new KQMLList();
    	reportContent.add("REPORT");
    	reportContent.add(":content");
    	reportContent.add(content);
    	reportContent.add(":context");
    	reportContent.add(context);
    	
    	return reportContent;
    }
}
