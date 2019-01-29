package models;

import environment.Scene;
import utilities.TextToSpeech;

public class Response {

	public static void isSatisfiedVerbose(ModelInstantiation mi, boolean satisfied)
	{
		StringBuilder response = new StringBuilder();
		
		if (satisfied)
			response.append("I think so, because ");
		else
			response.append("I don't think so, because ");
		
		
		int reasonNumber = 0;
		for (Constraint c : mi.getConstraints())
		{
			boolean constraintSatisfied = c.isSatisfied(Scene.currentScene);
			if (reasonNumber > 0)
			{
				if (satisfied)
				{
					response.append(" and ");
				}
				else {
					if (constraintSatisfied)
						response.append(" even though ");
					else
						response.append(" but ");
				}
			}
			if (c instanceof StructureConstraint)
			{
				if (((StructureConstraint)c).getSubject().isRestricted())
				{
					if (constraintSatisfied)
						response.append("only ");
					else
						response.append("more than one of ");
				}
					
				
					
			}
			response.append("the ");
			response.append(c.reason());
			reasonNumber++;
		}
		response.append(".");
		TextToSpeech.say(response.toString());
	}
	
	public static void isSatisfied(ModelInstantiation mi, boolean satisfied)
	{
		StringBuilder response = new StringBuilder();
		
		if (satisfied)
			response.append("It looks correct to me.");
		else
		{
			int reasonNumber = 0;
			response.append("I don't think so, because ");
		
			

			for (Constraint c : mi.getConstraints())
			{
				if (reasonNumber > 0)
					response.append(" and ");
				boolean constraintSatisfied = c.isSatisfied(Scene.currentScene);
				if (constraintSatisfied)
					continue;
				
				if (c instanceof StructureConstraint)
				{
					if (((StructureConstraint)c).getSubject().isRestricted())
					{
						
						response.append("more than one of ");
					}
						
					
						
				}
				response.append("the ");
				response.append(c.reason());
				reasonNumber++;
			}
			response.append(".");
		}
		TextToSpeech.say(response.toString());
	}

}