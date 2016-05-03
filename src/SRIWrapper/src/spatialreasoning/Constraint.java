package spatialreasoning;

import java.util.ArrayList;
import java.util.List;

import environment.Block;

public class Constraint {

	List<String> variables;
	Predicate predicate;
	
	public Constraint(PredicateType predicateType, String a)
	{
		predicate = new Predicate(predicateType);
		variables = new ArrayList<String>();
		variables.add(a);
	}
	
	public Constraint(PredicateType predicateType, String a, String b)
	{
		predicate = new Predicate(predicateType);
		variables = new ArrayList<String>();
		variables.add(a);
		variables.add(b);
	}
	
	public String getStepToResolveConstraint(List<Block> assignment)
	{
		if (assignment.size() == 1)
			return "Put the " + assignment.get(0).label + " block" + predicate.getActionString();
		else if (assignment.size() == 2)
			return "Put the " + assignment.get(0).label + " block" + predicate.getActionString() + "the " + assignment.get(1).label + " block.";
		else
			return null;
	}
}
