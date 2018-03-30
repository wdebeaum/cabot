package environment;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import TRIPS.KQML.KQMLList;

public class ReferringExpressionInterpreter {

	Scene currentScene;
	KQMLList referringExpression;
	
	public ReferringExpressionInterpreter(Scene currentScene, KQMLList referringExpression) {
		this.currentScene = currentScene;
		this.referringExpression = referringExpression;
	}
	
	// Return a list of structure instances with most likely first
	public List<StructureInstance> resolveReference()
	{
		return null;
	}

	private List<StructureInstance> getInstancesByStructureType()
	{
		return null;
	}
	
	private List<StructureInstance> getInstancesByPredicate()
	{
		return null;
	}
	

}
