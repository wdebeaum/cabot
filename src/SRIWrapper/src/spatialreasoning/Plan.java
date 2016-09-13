package spatialreasoning;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import TRIPS.KQML.KQMLList;
import models.ModelBuilder;
import utilities.KQMLUtilities;
import utilities.TextToSpeech;
import environment.Scene;

public class Plan {

	public List<Constraint> constraints;
	public Set<String> variables;
	public List<Step> steps;
	public String nextBlock;
	public int currentStep;
	public String lastUtterance;
	private ModelBuilder modelBuilder;
	
	public Plan(ModelBuilder modelBuilder)
	{
		constraints = new ArrayList<Constraint>();
		steps = new ArrayList<Step>();
		currentStep = 0;
		this.modelBuilder = modelBuilder;
	}
	
	public void processKQML(KQMLList content, KQMLList context)
	{
		String speechAct = content.get(0).stringValue();
		String goal = content.getKeywordArg(":WHAT").stringValue();
		KQMLList goalLF = KQMLUtilities.findTermInKQMLList(goal, context);
		String goalType = goalLF.getKeywordArg(":INSTANCE-OF").stringValue();
		
		if (speechAct.equalsIgnoreCase("ADOPT") )
		{
			if ( goalType.equalsIgnoreCase("ONT::CREATE"))
			{
				String objectToMakeID = goalLF.getKeywordArg(":AFFECTED-RESULT").stringValue();
				KQMLList objectToMakeLF = KQMLUtilities.findTermInKQMLList(objectToMakeID, context);
				String objectToMake = objectToMakeLF.getKeywordArg(":INSTANCE-OF").stringValue();
				String cleanObjectToMake = objectToMake.split("::")[1];
				
				if (!modelBuilder.currentModel.name.equals(cleanObjectToMake))
				{
					steps.add(new Step("querymodeldefinition", cleanObjectToMake));
					modelBuilder.processNewModel(cleanObjectToMake);
				}
			}
			else if (goalType.equalsIgnoreCase("ONT::QUERY-MODEL"))
			{
				String eventToQueryID = goalLF.getKeywordArg(":NEUTRAL").stringValue();
				KQMLList eventToQuery = KQMLUtilities.findTermInKQMLList(eventToQueryID, context);
				String modelTermID = eventToQuery.getKeywordArg(":NEUTRAL1").stringValue();
				KQMLList modelTerm = KQMLUtilities.findTermInKQMLList(modelTermID, context);
				String modelName = modelTerm.getKeywordArg(":INSTANCE-OF").stringValue();
				String cleanModelName = modelName.split("::")[1]; 
				//boolean result = modelBuilder.getModelInstantiation(cleanModelName)
					//		.testModelOnStructureInstance(newBlocks);
			}
		}
		else if(speechAct.equalsIgnoreCase("ASSERTION"))
		{
			KQMLList assertions = (KQMLList)goalLF.getKeywordArg(":EVENTS");
			
		}
		
		
		
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
