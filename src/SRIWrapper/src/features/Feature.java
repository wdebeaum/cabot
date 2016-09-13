package features;

import TRIPS.KQML.*;



public abstract class Feature<T> {

	protected String name;
	
	public Feature(String name)
	{
		this.name = name;
	}
	
	public abstract T getValue();
	
	public abstract void setValue(T newValue);

	public static boolean isIndicatedClass(String[] indicatorClasses, KQMLList lfTerm)
	{
		for (String indicatorClass : indicatorClasses)
		{
			KQMLObject concept = lfTerm.get(2);
			KQMLList conceptList = null;
			if (!(concept instanceof KQMLList))
				return false;
			
			conceptList = (KQMLList)concept;
			
			if (conceptList.get(1).stringValue().equalsIgnoreCase(indicatorClass))
				return true;
		}
		
		return false;
	}
	
	public static boolean isIndicatedGloss(String[] indicatorGlosses, KQMLList lfTerm)
	{
		for (String indicatorGloss : indicatorGlosses)
		{
			KQMLObject concept = lfTerm.get(2);
			if (concept.stringValue().equalsIgnoreCase(indicatorGloss))
				return true;
		}
		return false;
	}
	
	public String getName()
	{
		return name;
	}
	
}
