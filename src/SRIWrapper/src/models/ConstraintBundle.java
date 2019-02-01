package models;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ConstraintBundle {
	
	private List<Constraint> constraints;
	private List<ReferringExpression> referringExpressions;
	
	public ConstraintBundle()
	{
		constraints = new ArrayList<Constraint>();
		referringExpressions = new ArrayList<ReferringExpression>();
	}
	
	public List<Constraint> getConstraints()
	{
		return constraints;
	}
	
	public List<ReferringExpression> getReferringExpressions()
	{
		return referringExpressions;
	}
	
	public int size()
	{
		return constraints.size();
	}
	
	public void add(Constraint c)
	{
		constraints.add(c);
	}
	
	public void add(ReferringExpression re)
	{
		referringExpressions.add(re);
	}
	
	public void addAll(Collection<Constraint> cc)
	{
		constraints.addAll(cc);
	}
}
