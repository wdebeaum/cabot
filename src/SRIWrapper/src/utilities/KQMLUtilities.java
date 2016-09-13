package utilities;

import TRIPS.KQML.*;

public class KQMLUtilities {

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

}
