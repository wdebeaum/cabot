package goals;

import TRIPS.KQML.KQMLList;

public class Query extends Goal {

	protected String query;
	protected String queryId;
	
	public Query(String what, String id, KQMLList term) {
		super(what, id, term);
		type = GoalType.QUERYINCONTEXT;
		// TODO Auto-generated constructor stub
	}
	
	public Query()
	{
		super("", "QI" + getNextId(), new KQMLList());
	}

	public Query(String instance) {
		super(instance);
		// TODO Auto-generated constructor stub
	}
	
	public void addQuery(String query, String id)
	{
		this.query = query;
		queryId = id;

	}

	public String getQuery() {
		return query;
	}

	public String getQueryId() {
		return queryId;
	}
	
	

}
