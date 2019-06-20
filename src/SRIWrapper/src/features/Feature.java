package features;

import java.util.*;

import TRIPS.KQML.*;
import models.ReferringExpression;
import utilities.KQMLUtilities;



public abstract class Feature<T> implements FeatureGroup{

	protected String name;
	protected String prettyName;
	protected boolean constant;
	protected boolean comparable;
	private ReferringExpression source;
	private boolean inferred;
	
	public Feature(String name)
	{
		this.name = name;
		prettyName = KQMLUtilities.cleanConcept(name);
		constant = false;
		source = null;
		inferred = false;
		comparable = true;
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
	
	public String getPrettyName()
	{
		return prettyName;
	}
	
	public void setPrettyName(String prettyName)
	{
		this.prettyName = prettyName;
	}
	
	public Map<String,Feature> getFeatures()
	{
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.put(name,this);
		return result;
	}
	
	public String toString()
	{
		return "" + getValue().toString();
	}
	
	public boolean isConstant()
	{
		return (constant && source == null);
	}
	
	public void setConstant(boolean value)
	{
		constant = value;
		System.out.println("Feature " + name + " set to constant");
	}

	public ReferringExpression getSource() {
		return source;
	}

	public void setSource(ReferringExpression source) {
		this.source = source;
	}
	
	public void setInferred()
	{
		inferred = true;
	}
	
	public boolean isInferred()
	{
		return inferred;
	}
	
	public boolean isComparable()
	{
		return comparable;
	}
	
}
