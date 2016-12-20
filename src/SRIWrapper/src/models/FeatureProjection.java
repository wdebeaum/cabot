package models;

import utilities.KQMLUtilities;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.Block;
import environment.StructureInstance;
import features.*;

import java.util.*;

import org.jblas.DoubleMatrix;

public class FeatureProjection {

	private StructureInstance structureInstance;
	public HashMap<String,FeatureGroup> variableFGBindings; // ONT::XXXX -> featureGroup
	
	public FeatureProjection(StructureInstance structureInstance) {
		this.structureInstance = structureInstance;
		this.variableFGBindings = new HashMap<String,FeatureGroup>();
	}
	
	public boolean extractProjectionFromKQML(KQMLList context)
	{
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractProjectionFeatureGroup(term,context);
		}
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractProjectionOperator(term,context);
		}
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractProjectionModifier(term,context);
		}
		
		return true;
	}

	private boolean extractProjectionOperator(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":INSTANCE-OF") != null)
		{
			String variable = term.get(1).stringValue();
			KQMLObject instance = term.getKeywordArg(":INSTANCE-OF");
			KQMLObject elementTypeObject = term.getKeywordArg(":ELEMENT-TYPE");
			
			if (instance == null)
				return false;
			
			switch (instance.stringValue().toUpperCase())
			{
			case "ONT::IN-LOC":
				String figure = term.getKeywordArg(":FIGURE").stringValue();
				String ground = term.getKeywordArg(":GROUND").stringValue();
				if (!variableFGBindings.containsKey(figure))
				{
					System.out.println("No figure for in-loc");
					return false;
				}
				
				if (!variableFGBindings.containsKey(ground))
				{
					System.out.println("No ground for in-loc");
					return false;
				}
				
				FeatureGroup modifier = variableFGBindings.get(figure);
				FeatureGroup modified = variableFGBindings.get(ground);
				
				System.out.println("Trying in-loc projection");
				if (modified instanceof TemporalSequenceFeature && 
						modifier instanceof TemporalSequenceFeature)
				{
					TemporalSequenceFeature tsfModifier = (TemporalSequenceFeature)modifier;
					TemporalSequenceFeature tsfModified = (TemporalSequenceFeature)modified;
					
					tsfModifier.projectOnto(tsfModified);
					System.out.println("projected sequence onto sequence");
				}
				return true;
				
				
			}
			
		}
		
		return false;
	}
	
	private boolean extractProjectionModifier(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":MOD") != null)
		{
			String variable = term.get(1).stringValue();
			String mod = term.getKeywordArg(":MOD").stringValue();
			KQMLList modTerm = KQMLUtilities.findTermInKQMLList(mod, context);
			String figure = modTerm.getKeywordArg(":FIGURE").stringValue();
			String ground = modTerm.getKeywordArg(":GROUND").stringValue();
			if (!variableFGBindings.containsKey(figure))
			{
				System.out.println("No figure for mod");
				return false;
			}
			
			if (!variableFGBindings.containsKey(ground))
			{
				System.out.println("No ground for mod");
				return false;
			}
			
			// This is kind of weird, check later
			FeatureGroup modifier = variableFGBindings.get(ground);
			FeatureGroup modified = variableFGBindings.get(figure);
			
			System.out.println("Trying mod projection");
			if (modified instanceof TemporalSequenceFeature && 
					modifier instanceof TemporalSequenceFeature)
			{
				TemporalSequenceFeature tsfModifier = (TemporalSequenceFeature)modifier;
				TemporalSequenceFeature tsfModified = (TemporalSequenceFeature)modified;
				
				tsfModifier.projectOnto(tsfModified);
				tsfModified.straighten();
				System.out.println("projected count onto sequence");
			}
			

			
		}
		
		return false;		
	}
	
	private boolean extractProjectionFeatureGroup(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":INSTANCE-OF") != null)
		{
			String variable = term.get(1).stringValue();
			KQMLObject instance = term.getKeywordArg(":INSTANCE-OF");
			KQMLObject elementTypeObject = term.getKeywordArg(":ELEMENT-TYPE");
			
			if (instance == null)
				return false;
			
			switch (instance.stringValue().toUpperCase())
			{
			case "ONT::NUMBER":
				int value = Integer.parseInt(term.getKeywordArg(":VALUE").stringValue());
				CountFeature cf = new CountFeature("count");
				cf.setValue(value);
				variableFGBindings.put(variable,cf);
				System.out.println("Extracted count of " + cf.getValue());
				return true;
			case "ONT::ROW-FORMATION":
				TemporalSequenceFeature row = new TemporalSequenceFeature("row");
				variableFGBindings.put(variable,row);
				System.out.println("Extracted row");
				return true;
			case "ONT::SET":
				if (elementTypeObject == null)
					return false;
				String elementType = elementTypeObject.stringValue();
				
				
				String size = null;
				if (term.getKeywordArg(":SIZE") != null)
				{
					size = term.getKeywordArg(":SIZE").stringValue();
				}
				else
				{
					System.out.println("No size of set");
					return false;
				}
				
				KQMLList sizeTerm = KQMLUtilities.findTermInKQMLList(size, context);
				int number = Integer.parseInt(sizeTerm.getKeywordArg(":VALUE").stringValue());
				PointFeature origin = new PointFeature("origin");
				origin.setValue(new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH}));
				FeatureGroup set = buildRow(origin,elementType,number);
				System.out.println("Built set: " + set);
				variableFGBindings.put(variable,set);
				return true;
			case "ONT::INCREASE":
				TemporalSequenceFeature increase = new TemporalSequenceFeature("increase");
				for (int i = 1; i < 11; i++)
				{
					CountFeature cf2 = new CountFeature("count");
					cf2.setValue(i);
					increase.addToSequence(cf2);
				}
				System.out.println("Built increase feature: " + increase);
				variableFGBindings.put(variable,increase);
				return true;
			
			}
			
			
		}
		return false;
	}
	
	private TemporalSequenceFeature buildRow(PointFeature origin, String elementType, int size)
	{
		System.out.println("Building row");
		TemporalSequenceFeature row = new TemporalSequenceFeature("row");
		row.getDirectionFeature().setValue(new DoubleMatrix(new double[]{1,0,0}));
		for (int i = 0; i < size; i++)
		{
			if (elementType.equalsIgnoreCase("ONT::TOWER"))
			{
				PointFeature nextOrigin = row.getNextInDirection(Block.BLOCK_WIDTH);
				row.addToSequence(buildTower(nextOrigin,1));
			}
		}
		
		return row;
	}
	
	private TemporalSequenceFeature buildTower(PointFeature origin, int height)
	{
		System.out.println("Building tower");
		TemporalSequenceFeature tower = new TemporalSequenceFeature("tower");
		tower.getDirectionFeature().setValue(new DoubleMatrix(new double[]{0,0,1}));
		
		for (int i = 0; i < height; i++)
		{
			PointFeature pf = tower.getNextInDirection(Block.BLOCK_WIDTH);
			Block b = new Block(pf.getValue());
			BlockFeatureGroup bfg = new BlockFeatureGroup(b);
			bfg.setPointFeature(pf);
			tower.addToSequence(bfg);
		}
		return tower;
	}
	
	
}
