package owlground.objects;

import java.util.Collection;
import java.util.HashSet;

import owlground.language.Demonstration;

/**
 * Represents groups of WorldObjects that are referenced as a set.
 * @author iperera
 *
 */
public class WorldObjectGroup {
	HashSet<String> universalProperties; 	//Properties that hold for all objects
	HashSet<String> perceivedUniversalProperties;	// Our guesses for properties that hold for all objects
	HashSet<String> negatedProperties;
	HashSet<String> perceivedWords;
	
	Demonstration demonstration; 
	
	HashSet<WorldObject> worldObjects;
	String quantifier;
	String proform;
	String className;
	String number;
	boolean isNumericNumber;
	int numericNumber;
	boolean focus;
	
	public WorldObjectGroup()
	{
		universalProperties = new HashSet<String>();
		perceivedUniversalProperties = new HashSet<String>();
		negatedProperties = new HashSet<String>();
		perceivedWords = new HashSet<String>();
		worldObjects = new HashSet<WorldObject>();
		demonstration = null;
		number = "";
		focus = false;
		quantifier = "";
	}

	public WorldObjectGroup(WorldObject wo)
	{
		this();
		addWorldObject(wo);

	}
	
	public WorldObjectGroup(WorldObject wo, Demonstration demonstration)
	{
		this(wo);
		this.demonstration = demonstration;
		wo.setDemonstration(demonstration);
	}
	public WorldObjectGroup(WorldObject wo, Demonstration demonstration, String className, 
			String quantifier)
	{
		this(wo, demonstration);
		this.className = className;
		this.quantifier = quantifier;
	}
	
	public void addWorldObject(WorldObject wo)
	{
		worldObjects.add(wo);
		if (demonstration != null && demonstration.getUtterance() != null)
			demonstration.getUtterance().addWorldObject(wo);
		if (universalProperties.size() == 0)
			universalProperties = new HashSet<String>(wo.getProperties());
		else
		{
			universalProperties.retainAll(wo.getProperties());
		}
	}
	
	public HashSet<String> getCommonPerceivedProperties()
	{
		// Get the intersection of all the perceived properties in the group
		HashSet<String> commonProperties = new HashSet<String>();
		for (WorldObject wo : worldObjects)
		{
			commonProperties = wo.getPerceivedProperties();
			break;
		}
		for (WorldObject wo : worldObjects)
		{
			for (String property : commonProperties)
			{
				if (!wo.getPerceivedProperties().contains(property))
					commonProperties.remove(property);
			}
		}
		
		return commonProperties;
	}
	
	public Demonstration getDemonstration() {
		return demonstration;
	}

	public void setDemonstration(Demonstration demonstration) {
		this.demonstration = demonstration;
	}

	public String getClassName() {
		return className;
	}

	public void setClassName(String className) {
		this.className = className;
	}

	public HashSet<String> getUniversalProperties() {
		return universalProperties;
	}

	public HashSet<String> getPerceivedUniversalProperties() {
		return perceivedUniversalProperties;
	}

	public HashSet<String> getNegatedProperties() {
		return negatedProperties;
	}

	public HashSet<String> getPerceivedWords() {
		return perceivedWords;
	}

	public HashSet<WorldObject> getWorldObjects() {
		return worldObjects;
	}

	public String getQuantifier() {
		return quantifier;
	}
	
	public String getNumber() {
		return number;
	}

	public void setNumber(String number) {
		this.number = number;
		
		isNumericNumber = true;
		numericNumber = 0;
		try 
		{
			numericNumber = Integer.parseInt(number);
		}
		catch(NumberFormatException e)
		{
			isNumericNumber = false;
		}
	}

	public void setQuantifier(String quantifier) {
		this.quantifier = quantifier;
	}
	
	public boolean isPlural()
	{
		return quantifier.equals("ONT::THE-SET") || quantifier.equals("ONT::BARE") ||
				quantifier.equals("ONT::INDEF-SET") || number.equals("ONT::SOME");
	}
	
	public boolean isDefinite()
	{
		return quantifier.equals("ONT::THE-SET") || quantifier.equals("ONT::THE");
	}
	
	public boolean isExhaustive()
	{
		return isDefinite() && !isDeictic();
	}
	
	public boolean isDeictic()
	{
		return proform.equals("ONT::THIS") ||
				proform.equals("ONT::THAT") ||
				proform.equals("ONT::THOSE") ||
				proform.equals("ONT::THESE");
	}
	
	public boolean isNull()
	{
		return quantifier.equals("ONT::NONE");
	}
	
	public boolean referencesFilled()
	{
		if (worldObjects.size() > 0 && !isPlural())
			return true;
		if (isNumericNumber && numericNumber < 0 && isPlural())
			return false;
		if (isNumericNumber && worldObjects.size() > 0 && worldObjects.size() >= numericNumber)
			return true;
		
		return false;
	}
	
	/**
	 * Return true is the given collection of objects is compatible with the 
	 * quantifier associated with this group
	 * @param objects
	 * @return
	 */
	public boolean compatibleSet(Collection<WorldObject> objects)
	{
		int numberOfObjects = objects.size();

		if (numberOfObjects > 1 && !isPlural())
		{
			//System.out.println(numberOfObjects + " objects but not plural");
			return false;
		}
		if (numberOfObjects < 2 && isPlural())
		{
			//System.out.println(numberOfObjects + " object but plural");
			return false;
		}
		if (isNumericNumber && numberOfObjects != numericNumber)
		{
			//System.out.println(numberOfObjects + " objects but should be " + numericNumber);
			return false;
		}
		
		return true;
	}

	public boolean isFocus() {
		return focus;
	}

	public void setFocus(boolean focus) {
		this.focus = focus;
	}
	
	public void clearWorldObjects()
	{
		worldObjects.clear();
	}
}
