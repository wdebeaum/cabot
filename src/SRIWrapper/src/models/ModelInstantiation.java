package models;

import java.util.*;

import utilities.KQMLUtilities;
import environment.*;
import TRIPS.KQML.*;
import features.*;


public class ModelInstantiation {

	private StructureInstance currentStructureInstance;
	private HashMap<String,FeatureGroup> components;
	private List<FeatureConstraint> constraints;
	
	public ModelInstantiation() {
		currentStructureInstance = new StructureInstance("placeholder", new ArrayList<Block>());
		components = new HashMap<String,FeatureGroup>();
		constraints = new ArrayList<FeatureConstraint>();
	}

	public void addKQMLTerm(KQMLList term, KQMLList context)
	{
		String variableName = term.get(1).stringValue();
		KQMLObject concept = term.get(2);
		KQMLList conceptList = null;
		if (concept instanceof KQMLList)
		{
			conceptList = (KQMLList)concept;
			String ontologyName = conceptList.get(1).stringValue();
			
			if (ontologyName.equalsIgnoreCase("ONT::GENERAL-STRUCTURE"))
			{
				components.put(variableName, currentStructureInstance);
				currentStructureInstance.setName(conceptList.stringValue());
			}
		}
	}
	
	public void addConstraint()
	{
		
	}
	
	public void getConstraintsFromKQML(KQMLList context)
	{
		int constraintsFound = 0;
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			boolean result = extractFeature(termList,context);
			
		}
	}
	
	private boolean extractFeature(KQMLList term, KQMLList context)
	{
		KQMLObject concept = term.get(2);
		KQMLList conceptList = null;
		String scale = "unnamed";
		Feature extractedFeature = null;
		Feature groundFeature = null;
		String ground = null;
		String operatorString = null;
		KQMLList groundTerm = null;
		if (concept instanceof KQMLList)
		{
			conceptList = (KQMLList)concept;
			// Get the scale for naming the feature
			if (term.getKeywordArg(":SCALE") != null)
			{
				scale = term.getKeywordArg(":SCALE").stringValue();
				extractedFeature = currentStructureInstance.getFeature(scale);
			}
			
			// Get the comparison object or property
			if (term.getKeywordArg(":GROUND") != null)
			{
				ground = term.getKeywordArg(":GROUND").stringValue();
				groundTerm = KQMLUtilities.findTermInKQMLList(ground, context);

			}
			// Get the ONT::MORE-VAL or equivalent
			operatorString = conceptList.get(1).stringValue();
			FeatureConstraint.Operator operator = FeatureConstraint.operatorFromTRIPS(operatorString);
			
			if (groundTerm != null)
			{
				if (groundTerm.getKeywordArg(":SCALE") != null)
				{
					String groundScale = groundTerm.getKeywordArg(":SCALE").stringValue();
					groundFeature = currentStructureInstance.getFeature(groundScale);
				}
				else
				{
					KQMLObject groundConcept = groundTerm.get(2);
					if (groundConcept instanceof KQMLList)
					{
						String groundFeatureName = ((KQMLList)groundConcept).get(2).stringValue();
						groundFeature = currentStructureInstance.getFeature(groundFeatureName);
					}
				}
			}
			if (extractedFeature == null || groundFeature == null || operator == null)
				return false;
			FeatureConstraint newConstraint = new FeatureConstraint(extractedFeature,
														operator, 
														FeatureConstraint.ComparisonType.VALUE, 
														groundFeature);
			System.out.println("Extracted feature: " + newConstraint);
			constraints.add(newConstraint);
			return true;
			
		}
		
		return false;
	}
	
	//TODO
	public List<FeatureConstraint> unsatisfiedConstraints()
	{
		List<FeatureConstraint> results = new ArrayList<FeatureConstraint>();
		for (FeatureConstraint constraint : constraints)
		{
			
		}
		
		return results;
	}
	
	public boolean testModelOnStructureInstance(Set<Block> newBlocks)
	{
		currentStructureInstance.setBlocks(newBlocks);
		
		for (FeatureConstraint constraint : constraints)
		{
			System.out.println("Constraint:");
			System.out.println(constraint);
			boolean satisfied = constraint.isSatisfied();
			if (satisfied)
				System.out.println(" is satisfied");
			else
			{
				System.out.println(" is not satisfied");
				return false;
			}
		}
		return true;
	}
	
}
