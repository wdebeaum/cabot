package utilities;

import TRIPS.KQML.*;

public class KQMLUtilities {
	
	public static int ONT_REF = 0;
	public static int VARIABLE = 1;

	public static KQMLList findTermInKQMLList(String variableName,KQMLList list)
	{
		for (KQMLObject term : list)
		{
			if (term instanceof KQMLList)
			{
				KQMLList termList = (KQMLList)term;
				if (termList.get(1).stringValue().equalsIgnoreCase(variableName))
					return termList;
			}
		}
		return null;
	}
	
	public static String cleanLex(String lex)
	{
		return lex.substring(3);
	}
	
	public static String cleanConcept(String concept)
	{
		String result = concept.substring(5);
		result.replace("-SCALE", "");
		return result;
	}

}
