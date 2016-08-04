package plans;

import java.util.HashMap;

import states.Action;


public class ActionPlanner {

	private HashMap<String,Action> variableActionMapping;
	
	public ActionPlanner()
	{
		variableActionMapping = new HashMap<String, Action>();
	}
	
	public void addAction(Action a)
	{
		variableActionMapping.put(a.getName(), a);
	}
	
	public Action getAction(String variableName)
	{
		return variableActionMapping.get(variableName);
	}
	
	public boolean hasAction(String variableName)
	{
		return variableActionMapping.containsKey(variableName);
	}
}
