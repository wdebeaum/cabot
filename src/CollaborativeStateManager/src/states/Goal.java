package states;

import java.util.*;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;

public class Goal {

	Goal parent;
	KQMLList term;
	boolean accepted;
	boolean failed;
	List<KQMLList> failureMessages;
	
	public Goal(KQMLList term)
	{
		this(term, null);
		
	}
	
	public Goal(KQMLList term, Goal parent)
	{
		this.term = term;
		this.parent = parent;
		accepted = false;
		failed = false;
		failureMessages = new ArrayList<KQMLList>();
	}
	
	public Goal(String variableName, KQMLList context, Goal parent)
	{
		TermExtractor te = new TermExtractor();
		this.term = te.extractTerm(variableName, context);
		this.parent = parent;	
		accepted = false;
		failed = false;
		failureMessages = new ArrayList<KQMLList>();
	}
	
	public Goal(String variableName, KQMLList context)
	{
		this(variableName,context,null);
	}
	
	
	public KQMLList getKQMLTerm()
	{
		return term;
	}
	
	public String getVariableName()
	{
		return term.get(1).stringValue();
	}
	
	public void setAccepted()
	{
		accepted = true;
	}

	public boolean isFailed() {
		return failed;
	}

	public void setFailed(boolean failed) {
		this.failed = failed;
	}
	
	
}
