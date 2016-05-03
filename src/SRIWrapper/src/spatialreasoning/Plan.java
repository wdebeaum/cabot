package spatialreasoning;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import utilities.TextToSpeech;
import environment.Scene;

public class Plan {

	public List<Constraint> constraints;
	public Set<String> variables;
	public List<Step> steps;
	public String nextBlock;
	public int currentStep;
	
	public Plan()
	{
		constraints = new ArrayList<Constraint>();
		steps = new ArrayList<Step>();
		currentStep = 0;
	}
	
	
	public List<Constraint> generatePlan(Scene s)
	{
		List<Constraint> remainingConstraints = new ArrayList<Constraint>();
		
		return remainingConstraints;
	}
	
	public void executeNextStep()
	{
		if (currentStep == steps.size() - 1)
		{
			currentStep++;
			TextToSpeech.say("We're done!");
			return;
		}
		else if (currentStep > steps.size() - 1)
		{
			return;
		}
		if (steps.get(currentStep).execute())
			currentStep++;
	}
}
