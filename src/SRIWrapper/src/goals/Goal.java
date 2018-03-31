package goals;

import java.util.Iterator;
import java.util.ListIterator;

import utilities.KQMLUtilities;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLToken;

public class Goal {

	private String what;
	private String id;
	private KQMLList term;
	private static int nextGoalWhat = 0;
	private static int nextGoalId = 1;
	private Goal parent;
	private GoalType type;
	private KQMLList context;
	private boolean completed = false;
	
	
	public enum GoalType {GOAL, SUBGOAL, MODIFICATION, ELABORATION, QUERYINCONTEXT }
	
	public Goal(String what, String id, KQMLList term) {
		this.what = what;
		this.id = id;
		this.term = term;
		parent = null;
		type = GoalType.GOAL;
		context = new KQMLList();
	}
	
	public Goal(String instance)
	{
		id = "GOALID" + getNextId();
		what = "GOALWHAT" + getNextWhat();
		term = new KQMLList();
		term.add("ONT::RELN");
		term.add(what);
		term.add(":INSTANCE-OF");
		term.add(instance);
		type = GoalType.GOAL;
		context = new KQMLList();
	}
	
	public boolean isCompleted()
	{
		return completed;
	}
	
	public void setCompleted()
	{
		completed = true;
	}
	
	public String getAgent(KQMLList context)
	{
		String agent = null;
		if (term.getKeywordArg(":AGENT") != null)
		{
			String agentVariable = term.getKeywordArg(":AGENT").stringValue();
			KQMLList agentTerm = KQMLUtilities.findTermInKQMLList(agentVariable, context);
			if (agentTerm.getKeywordArg(":REFERS-TO") != null)
				agent = agentTerm.getKeywordArg(":REFERS-TO").stringValue();
		}
		
		return agent;
	}
	
	private static int getNextId()
	{
		nextGoalId += 2;
		return nextGoalId;
	}
	
	public void addContext(KQMLList newContext)
	{
		context.addAll(newContext);
	}
	
	public String getInstanceOf()
	{
		if (term == null)
			return null;
		
		KQMLObject instanceOfObject = term.getKeywordArg(":INSTANCE-OF");
		if (instanceOfObject == null)
			return null;
		
		return instanceOfObject.stringValue();
	}
	
	private static int getNextWhat()
	{
		nextGoalWhat += 2;
		return nextGoalWhat;
	}

	public String getWhat() {
		return what;
	}

	public String getId() {
		return id;
	}

	public KQMLList getTerm() {
		return term;
	}

	public Goal getParent() {
		return parent;
	}

	public void setParent(Goal parent) {
		this.parent = parent;
		type = GoalType.SUBGOAL;
	}
	
	public KQMLList getAsList()
	{
		KQMLList response = new KQMLList();
		String typeName = type.toString();
		String parameter = ":OF";
		
		if (parent == null)
		{
			response.add("GOAL");
			return response;
		}
		
		if (type.equals(GoalType.QUERYINCONTEXT))
		{
			typeName = "QUERY-IN-CONTEXT";
			parameter = ":GOAL";
		}
		
		response.add(typeName);
		response.add(parameter);
		response.add(parent.getId());
		return response;
	}

	public GoalType getType() {
		return type;
	}
	
	public Goal returnModifiedWithAskIfAnswer(KQMLList answerTerm, KQMLList newContext)
	{
		KQMLList newTerm = new KQMLList();
		newTerm.addAll(term);
		Goal toReturn = null;
		String queryVariable = answerTerm.getKeywordArg(":QUERY").stringValue();
		KQMLList query = KQMLUtilities.findTermInKQMLList(queryVariable, newContext);
		System.out.println("Query: " + query);

		toReturn = new Goal("G" + getNextWhat(),"G" + getNextId(),newTerm);
		
		System.out.println("Answer-Modified term: " + toReturn.term );
		toReturn.context = newContext;
		
		return toReturn;
				
	}
	
	public Goal returnModifiedWithAnswer(KQMLList answerTerm, KQMLList newContext)
	{
		KQMLList newTerm = new KQMLList();
		newTerm.addAll(term);
		Goal toReturn = null;
		String queryVariable = answerTerm.getKeywordArg(":QUERY").stringValue();
		KQMLList query = KQMLUtilities.findTermInKQMLList(queryVariable, newContext);
		System.out.println("Query: " + query);
		if (answerTerm.getKeywordArg(":WHAT") != null &&
				answerTerm.getKeywordArg(":VALUE") != null)
		{
			String whatVariable = answerTerm.getKeywordArg(":WHAT").stringValue();
			System.out.println("What variable: *" + whatVariable + "*");
			String valueVariable = answerTerm.getKeywordArg(":VALUE").stringValue();
			
			int argumentIndex = query.indexOfIgnoreCase(whatVariable) - 1;
			System.out.println("Index of argument: " + argumentIndex);
			String argument = query.get(argumentIndex).stringValue();
			System.out.println("Replacing argument: " + argument);
			newTerm.removeKeywordArg(argument);
			newTerm.add(argument);
			newTerm.add(valueVariable);
			
			toReturn = new Goal("G" + getNextWhat(),"G" + getNextId(),newTerm);
			toReturn.updateTerm();
		}
		System.out.println("Answer-Modified term: " + toReturn.term );
		toReturn.context = newContext;
		
		return toReturn;
		
	}
	
	public void updateTerm()
	{
		term.set(1, new KQMLToken(what));
	}
	
}