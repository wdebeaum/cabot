package utilities;

import java.util.HashSet;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class KQMLUtilities {

	
	public static boolean isKQMLNull(KQMLObject kqmlObject)
	{
		if (kqmlObject == null)
			return true;
		if (kqmlObject.stringValue().equalsIgnoreCase("NIL") ||
				kqmlObject.stringValue().equals("-"))
			return true;
		
		return false;
	}
	
	public static KQMLList removedDuplicates(KQMLList list)
	{
		HashSet<KQMLObject> set = new HashSet<KQMLObject>();
		for (KQMLObject obj : list)
			set.add(obj);
		
		KQMLList toReturn = new KQMLList();
		for (KQMLObject obj : set)
			toReturn.add(obj);
		
		return toReturn;
	}
}
