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

	private HashMap<Goal,Goal> goalConnections; // Child -> Parent
	private HashMap<String,Goal> variableGoalMapping;
	private Goal activeGoal;
	private Goal privateGoal;
	private boolean globalSystemInitiative = false;
	
	public GoalPlanner()
	{
		goalConnections = new HashMap<Goal,Goal>();
		variableGoalMapping = new HashMap<String,Goal>();
		privateGoal = null;
	}
	
	public void addGoal(Goal goal, String parentVariableName)
	{
		String upperCaseParentVariableName = null;
		if (parentVariableName != null)
			upperCaseParentVariableName = parentVariableName.toUpperCase();
		goalConnections.put(goal,getGoal(upperCaseParentVariableName));
		variableGoalMapping.put(goal.getVariableName().toUpperCase(),goal);
		goal.setParent(getGoal(upperCaseParentVariableName));
		System.out.println("Added goal " + goal.getVariableName() + " with parent "
				+ upperCaseParentVariableName);
	}
	
	public void addGoal(Goal goal)
	{
		addGoal(goal,null);
	
	}
	
	public void addPrivateGoal(Goal goal)
	{
		this.privateGoal = goal;
		addGoal(goal);
	}
	
	public List<Goal> getFailedGoals()
	{
		List<Goal> result = new ArrayList<Goal>();
		for (Goal g : variableGoalMapping.values())
		{
			if (g.isFailed())
				result.add(g);
		}
		
		return result;
	}
	
	public KQMLList modify(Goal newGoal, String oldGoalName)
	{
		System.out.println("Modifying goal " + newGoal.getVariableName());
		Goal oldGoal = getGoal(oldGoalName);
		if (oldGoal == null)
		{
			System.out.println("No goal " + oldGoalName + " found.");
			return null;
		}
		Goal parentGoal = oldGoal.getParent();
		System.out.println("Replacing goal " + oldGoal.getVariableName() + 
				" with " + newGoal.getVariableName());
		replaceGoal(newGoal, oldGoal);
		
		return newGoal.adoptContent("MODIFICATION", oldGoalName);
		
//		if (parentGoal != null)
//			return newGoal.adoptContent("SUBGOAL", parentGoal.getVariableName());
//		else
//			return newGoal.adoptContent("GOAL", null);
		
		
	}
	
	public KQMLList modify(Goal newGoal)
	{
		System.out.println("Modifying goal " + newGoal.getVariableName());
		List<Goal> failedGoals = getFailedGoals();
		// No failed goals, just add this to the active goal
		if (failedGoals.isEmpty() && activeGoal != null)
		{
			System.out.println("Adding goal: " + newGoal.getVariableName());
			System.out.println("Active goal: " + activeGoal.getVariableName());
			addGoal(newGoal, activeGoal.getVariableName());
			return newGoal.adoptContent("MODIFICATION", activeGoal.getVariableName());
			//return newGoal.adoptContent("SUBGOAL", activeGoal.getVariableName());
		}
		// The active goal has failed, replace it
		else if (activeGoal != null && activeGoal.isFailed())
		{
			System.out.println("Replacing goal " + activeGoal.getVariableName() + 
					" with " + newGoal.getVariableName());
			Goal activeParentGoal = activeGoal.getParent();
			String activeGoalName = activeGoal.getVariableName();
			replaceGoal(newGoal, activeGoal);
			
			return newGoal.adoptContent("MODIFICATION", activeGoalName);
			
//			if (activeParentGoal != null)
//				return newGoal.adoptContent("SUBGOAL", activeParentGoal.getVariableName());
//			else
//				return newGoal.adoptContent("GOAL", null);
		}
		else
		{
			
			// TODO: Make this smarter
			for (Goal failedGoal : failedGoals)
			{
				Goal parentGoal = failedGoal.getParent();
				System.out.println("Replacing goal " + failedGoal.getVariableName() + 
						" with " + newGoal.getVariableName());
				replaceGoal(newGoal, failedGoal);
				
				return newGoal.adoptContent("MODIFICATION", failedGoal.getVariableName());
				
//				if (parentGoal != null)
//					return newGoal.adoptContent("SUBGOAL", parentGoal.getVariableName());
//				else
//					return newGoal.adoptContent("GOAL", null);
			}
		}
		
		return null;
	}
	
	public void replaceGoal(Goal newGoal, Goal oldGoal)
	{
		Goal parent = oldGoal.getParent();
		newGoal.setParent(parent);
		if (oldGoal == activeGoal)
			activeGoal = newGoal;
		removeGoal(oldGoal.getVariableName());
		if (parent == null)
			addGoal(newGoal,null);
		else
			addGoal(newGoal,parent.getVariableName());
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

	public Goal getPrivateGoal() {
		return privateGoal;
	}

	public boolean isGlobalSystemInitiative() {
		return globalSystemInitiative;
	}

	public void setGlobalSystemInitiative(boolean globalSystemInitiative) {
		this.globalSystemInitiative = globalSystemInitiative;
	}
	
	
	
}
