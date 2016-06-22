package extractors;

import handlers.IDHandler;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class EventExtractor {

	String id;
	KQMLList eventContext;
	KQMLList relnEventContext;
	KQMLList originalContext;
	KQMLList eventIDs;
	
	public EventExtractor()
	{
		eventContext = new KQMLList();
		relnEventContext = new KQMLList();
		eventIDs = new KQMLList();		
	}
	
	public void apply(KQMLList context)
	{
		originalContext = context;
		id = IDHandler.getNewID();
		for (KQMLObject term : context)
		{
			if (!(term instanceof KQMLList))
				continue;
			KQMLList termList = (KQMLList)term;
			
			if (termList.get(0).stringValue().toUpperCase().contains("RELN") && 
					termList.get(3).stringValue().toUpperCase().contains("ACTIVATE"))
			{
				eventIDs.add(termList.get(1));
				eventContext.add(term);
			}
		}
	}
	
	public KQMLList getEventsInContext()
	{
		return eventContext;
	}
	
	public KQMLList getOriginalContext()
	{
		return originalContext;
	}
	
	public KQMLList getEventIDsInContext()
	{
		return eventIDs;
	}
	
	public String getID()
	{
		return id;
	}
	
	
}
