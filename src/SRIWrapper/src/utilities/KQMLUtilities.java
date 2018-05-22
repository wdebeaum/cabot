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
	
	public static KQMLList setKeywordArgTo(KQMLList list, String arg, KQMLObject obj)
	{
		int index = list.indexOf(arg);
		list.removeKeywordArg(arg);
		
		list.add(index,new KQMLToken(arg));
		list.add(index+1,obj);
		
		
		return list;
		
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
	
	public static String cleanOnt(String concept)
	{
		if (concept.contains("ONT::"))
			concept = concept.substring(5);
		if (concept.contains("-VALUE"))
			concept = concept.substring(0,concept.length()-6);
		if (concept.contains("-SCALE"))
			concept = concept.substring(0,concept.length()-6);
		
		return concept;
	}

}
