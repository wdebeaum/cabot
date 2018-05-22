package goals;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class GoalMessages {

	public GoalMessages() {
		// TODO Auto-generated constructor stub
	}
	
	public static KQMLList suggestSomething(KQMLList context)
	{
		Goal suggestGoal = new Goal("ONT::YOU-SUGGEST-SOMETHING");
		return proposeAdoptContent(suggestGoal.getId(), suggestGoal.getWhat(), suggestGoal.getAsList(), context);
	}
	
	public static KQMLList executionStatus(String goalId, String status)
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
	

	public static KQMLList proposeAdoptContent(String id, String what, KQMLObject asList, KQMLList context)
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
	
	public static KQMLList proposeAdoptContent(Goal g)
	{
		KQMLList newContext = g.getContext();
		newContext.add(g.getTerm());
		return proposeAdoptContent(g.getId(), g.getWhat(), g.getAsList(), newContext);
	}
	
//	public static KQMLList proposeAskWhContent(String id, String what, String query, 
//			String parentId, KQMLList context)
//	{
//		KQMLList response = new KQMLList();
//		response.add("PROPOSE");
//		response.add(":CONTENT");
//		KQMLList adoptContent = new KQMLList();
//		adoptContent.add("ASK-WH");
//		adoptContent.add(":ID");
//		adoptContent.add(id);
//		adoptContent.add(":WHAT");
//		adoptContent.add(what);
//		adoptContent.add(":QUERY");
//		adoptContent.add(query);
//		adoptContent.add(":AS");
//		
//		KQMLList asList = new KQMLList();
//		asList.add("QUERY-IN-CONTEXT");
//		asList.add(":GOAL");
//		asList.add(parentId);
//		adoptContent.add(asList);
//		
//		response.add(adoptContent);
//		
//
//		
//		response.add(":CONTEXT");
//		response.add(context);
//		
//		return response;		
//	}
	
//	public static KQMLList proposeAskIfContent(String id, String query, 
//								String parentId, KQMLList context)
//	{
//		KQMLList response = new KQMLList();
//		response.add("PROPOSE");
//		response.add(":CONTENT");
//		KQMLList adoptContent = new KQMLList();
//		adoptContent.add("ASK-IF");
//		adoptContent.add(":ID");
//		adoptContent.add(id);
//		adoptContent.add(":QUERY");
//		adoptContent.add(query);
//		adoptContent.add(":AS");
//		
//		KQMLList asList = new KQMLList();
//		asList.add("QUERY-IN-CONTEXT");
//		asList.add(":GOAL");
//		asList.add(parentId);
//		adoptContent.add(asList);
//		
//		response.add(adoptContent);
//		
//		response.add(":CONTEXT");
//		response.add(context);
//		
//		Goal newGoal = new Goal(id + "WH",id,adoptContent);
//		addGoal(newGoal);
//		
//		return response;		
//	}
	
	public static KQMLList propose(KQMLList content, KQMLList context)
	{
		KQMLList response = new KQMLList();
		response.add("PROPOSE");
		response.add(":CONTENT");	
		
		response.add(content);
		
		response.add(":CONTEXT");
		response.add(context);
		
		return response;
	}
	
	public static KQMLList askIfContent(String id, String query, String parentId)
	{
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
		asList.add(parentId);
		adoptContent.add(asList);
		
		return adoptContent;
	}
	
	public static KQMLList askWhContent(String id, String query,
			String what, String parentId)
	{
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
		asList.add(parentId);
		adoptContent.add(asList);
		
		return adoptContent;
	}
	
	public static KQMLList proposeAskWhContent(Query q)
	{
		KQMLList content =  askWhContent(q.getId(), q.getQueryId(), q.getWhat(), q.getParent().getId());
		KQMLList context = new KQMLList();
		context.addAll(q.getContext());
		return propose(content, context);
	}
	
	public static KQMLList proposeAskIfContent(Query q)
	{
		KQMLList content =  askIfContent(q.getId(), q.getQuery(), q.getParent().getId());
		KQMLList context = new KQMLList();
		context.addAll(q.getContext());
		return propose(content, context);
	}
	
	public static KQMLList waitingForUser(String goalId)
	{
		return executionStatus(goalId,"ONT::WAITING-FOR-USER");
	}
	
	public static KQMLList workingOnIt(String goalId)
	{
		return executionStatus(goalId,"ONT::WORKING-ON-IT");
	}
	
	public static KQMLList done(String goalId)
	{
		return executionStatus(goalId,"ONT::DONE");
	}
}
