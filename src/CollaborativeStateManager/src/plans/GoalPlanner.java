package plans;

import handlers.IDHandler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;

import TRIPS.KQML.KQMLList;
import states.Goal;

public class GoalPlanner {

	private HashMap<Goal,Goal> goalConnections;
	private HashMap<String,Goal> variableGoalMapping;
	private Goal activeGoal;
	
	public GoalPlanner()
	{
		goalConnections = new HashMap<Goal,Goal>();
		variableGoalMapping = new HashMap<String,Goal>();
	}
	
	public void addGoal(Goal goal, String parentVariableName)
	{
		String upperCaseParentVariableName = null;
		if (parentVariableName != null)
			upperCaseParentVariableName = parentVariableName.toUpperCase();
		goalConnections.put(goal,getGoal(upperCaseParentVariableName));
		variableGoalMapping.put(goal.getVariableName().toUpperCase(),goal);
	}
	
	public void addGoal(Goal goal)
	{
		addGoal(goal,null);
	
	}
	
	public void removeGoal(String variableName)
	{
		String upperCaseVariableName = variableName.toUpperCase();
		if (variableGoalMapping.containsKey(upperCaseVariableName))
		{
			Goal goalToRemove = variableGoalMapping.get(upperCaseVariableName);
			if (activeGoal == goalToRemove)
				activeGoal = null;
			variableGoalMapping.remove(upperCaseVariableName);
			goalConnections.remove(goalToRemove);
			
			// TODO: Remove child goals of removed parent
		}
	}
	
	public Goal getGoal(String variableName)
	{
		if (variableName != null && 
				variableGoalMapping.containsKey(variableName.toUpperCase()))
			return variableGoalMapping.get(variableName.toUpperCase());
		
		return null;
	}
	
	public boolean hasGoal(String variableName)
	{
		return variableGoalMapping.containsKey(variableName.toUpperCase());
	}
	
	public boolean hasActiveGoal()
	{
		return (activeGoal != null);
	}
	
	public Goal getActiveGoal() {
		return activeGoal;
	}

	public void setActiveGoal(Goal goal) {
		if (!goalConnections.containsKey(goal))
			addGoal(goal);
		this.activeGoal = goal;
	}
	
	public void setActiveGoal(String goal)
	{
		setActiveGoal(getGoal(goal));
	}

	public List<Goal> generatePossibleGoals(Collection<String> goalTypes)
	{
		List<Goal> goalsToReturn = new ArrayList<Goal>();
		
		for (String goalType : goalTypes)
		{
			KQMLList goalTerm = new KQMLList();
			goalTerm.add("ONT::RELN");
			goalTerm.add(IDHandler.getNewID());
			goalTerm.add(":INSTANCE-OF");
			goalTerm.add(goalType);
			
			Goal newGoal = new Goal(goalTerm);
			addGoal(newGoal);
			goalsToReturn.add(newGoal);
		}
		
		return goalsToReturn;
	}
}
