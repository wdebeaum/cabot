package models;

import environment.*;
import java.util.*;

public class StructureModel {

	public String name;
	public HashSet<StructureInstance> positiveStructureInstances;
	public HashSet<StructureInstance> negativeStructureInstances;
	public HashSet<FeatureConstraint> inferredFeatureConstraints;
	public HashSet<FeatureConstraint> explicitFeatureConstraints;
	
	public StructureModel(String name)
	{
		this.name = name;
		positiveStructureInstances = new HashSet<StructureInstance>();
		negativeStructureInstances = new HashSet<StructureInstance>();
		inferredFeatureConstraints = new HashSet<FeatureConstraint>();
		explicitFeatureConstraints = new HashSet<FeatureConstraint>();
	}
	
	public void addPositiveStructureInstance(StructureInstance si)
	{
		positiveStructureInstances.add(si);
	}
	
	public void addNegativeStructureInstance(StructureInstance si)
	{
		negativeStructureInstances.add(si);
	}
	
	public void addInferredConstraint(FeatureConstraint fc)
	{
		inferredFeatureConstraints.add(fc);
	}
	
	public void addExplicitConstraint(FeatureConstraint fc)
	{
		inferredFeatureConstraints.add(fc);
	}
	
	
}
