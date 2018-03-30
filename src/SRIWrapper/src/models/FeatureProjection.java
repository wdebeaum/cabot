package models;

import utilities.KQMLUtilities;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import environment.*;
import features.*;

import java.util.*;

import org.jblas.DoubleMatrix;

public class FeatureProjection {

	private StructureInstance structureInstance;
	public HashMap<String,FeatureGroup> variableFGBindings; // ONT::XXXX -> featureGroup
	private static PointFeature origin;
	private static PointFeature nextLocation;
	private static boolean useNextLocation = false; 
	private static boolean upwards = false;
	private BuildAction buildAction;
	
	public FeatureProjection(StructureInstance structureInstance) {
		this.structureInstance = structureInstance;
		this.variableFGBindings = new HashMap<String,FeatureGroup>();
		if (origin == null)
		{
			origin = new PointFeature(FeatureConstants.ORIGIN);
			origin.setValue(new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH / 2.0}));
		}
		if (nextLocation == null)
		{
			nextLocation = new PointFeature("nextLocation");
			nextLocation.setValue(origin.getValue());
		}
		useNextLocation = false;
	}
	
	
	public static void setNewOrigin(DoubleMatrix point)
	{
		if (origin == null)
		{
			origin = new PointFeature(FeatureConstants.ORIGIN);	
		}
		origin.setValue(point);
	}
	
	public BuildAction getBuildAction() {
		return buildAction;
	}



	public boolean extractProjectionFromKQML(KQMLList context)
	{
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractStaticOperators(term,context);
		}
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
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractAction(term,context);
		}
		for (KQMLObject kqmlObject : context)
		{
			KQMLList term = (KQMLList)kqmlObject;
			extractProjectionManner(term,context);
		}
		
		return true;
	}
	
	private boolean extractAction(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":INSTANCE-OF") != null)
		{
			String variable = term.get(1).stringValue();
			KQMLObject instance = term.getKeywordArg(":INSTANCE-OF");
			
			if (instance == null)
				return false;

			switch (instance.stringValue().toUpperCase())
			{
			case "ONT::ADD-INCLUDE":
			case "ONT::MOVE":
			case "ONT::PUT":
				buildAction = new BuildAction();
				// Thing being put
				String affected = term.getKeywordArg(":AFFECTED").stringValue();
				FeatureGroup objectsToBuild = variableFGBindings.get(affected);
				if (objectsToBuild == null)
				{
					System.out.println("Affected term for build action undefined.");
					return false;
				}
				if (!(objectsToBuild instanceof UnorderedGroupingFeature))
				{
					System.out.println("Affected term is not an unordered grouping");
					return false;
				}
				
				if (useNextLocation)
				{
					buildAction.getBuildSequence().getOrigin().setValue(nextLocation.getValue());
				}
				else
				{
					buildAction.getBuildSequence().getOrigin().setValue(origin.getValue());
				}
				
				buildAction.projectFrom((UnorderedGroupingFeature)objectsToBuild);
				return true;
			}
		}
		return false;
	}
	
	private boolean extractStaticOperators(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return false;
		
		String variable = term.get(1).stringValue();
		KQMLObject instance = term.getKeywordArg(":INSTANCE-OF");
		
		
		if (instance == null)
			return false;
		
		switch (instance.stringValue().toUpperCase())
		{
		case "ONT::TOP-LOCATION":
			
			return true;
		case "ONT::ON":
			upwards = true;
			System.out.println("Old origin" + origin.getValue());
			origin.setValue(origin.getValue().add(new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH})));
			System.out.println("New origin" + origin.getValue());
			return true;
		case "ONT::OTHER":
			useNextLocation = true;
			System.out.println("Using next location: " + nextLocation.getValue());
			return true;
		}
		return false;
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
	
	private boolean extractProjectionManner(KQMLList term, KQMLList context)
	{
		if (term.getKeywordArg(":MANNER") != null)
		{
			String variable = term.get(1).stringValue();
			String manner = term.getKeywordArg(":MANNER").stringValue();
			KQMLList mannerTerm = KQMLUtilities.findTermInKQMLList(manner, context);
			
			
			// This is probably wrong
			String figure = null;
			String ground = null;
			if (mannerTerm.getKeywordArg(":FIGURE") != null)
				figure = mannerTerm.getKeywordArg(":FIGURE").stringValue();
			else
				return false;
			
			if (mannerTerm.getKeywordArg(":GROUND") != null)
				ground = mannerTerm.getKeywordArg(":GROUND").stringValue();
			else
				ground = variable;
			
			if (!variableFGBindings.containsKey(ground))
			{
				System.out.println("No ground for manner modification");
				return false;
			}
			
			// This is kind of weird, check later
			FeatureGroup modifier = variableFGBindings.get(ground);
			FeatureGroup modified = variableFGBindings.get(figure);
			
			System.out.println("Trying manner projection");
			if (modified != null && 
					modified instanceof TemporalSequenceFeature && 
					modifier instanceof TemporalSequenceFeature)
			{
				TemporalSequenceFeature tsfModifier = (TemporalSequenceFeature)modifier;
				TemporalSequenceFeature tsfModified = (TemporalSequenceFeature)modified;
				
				tsfModifier.projectOnto(tsfModified);
				tsfModified.straighten();
				System.out.println("projected count onto sequence");
			}
			else if (modifier instanceof TemporalSequenceFeature)
			{
				TemporalSequenceFeature tsfModifier = (TemporalSequenceFeature)modifier;
				TemporalSequenceFeature tsfModified = null;
				if (buildAction != null && buildAction.getBuildSequence() != null)
				{
					tsfModified = buildAction.getBuildSequence();
				}
				else
				{
					System.out.println("Build action or its sequence is null");
					return false;
				}
				
				tsfModifier.projectOnto(tsfModified);
				tsfModified.straighten();
				System.out.println("Projected manner onto sequence");
					
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
			
			if (modTerm.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::OTHER"))
				return false;
			
			// This is probably wrong
			String figure = null;
			String ground = null;
			if (modTerm.getKeywordArg(":FIGURE") != null)
				figure = modTerm.getKeywordArg(":FIGURE").stringValue();
			else
				return false;
			
			if (modTerm.getKeywordArg(":GROUND") != null)
				ground = modTerm.getKeywordArg(":GROUND").stringValue();
			else
				ground = variable;
			

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
				System.out.println("Projected " + tsfModifier.getName() + " onto sequence " +
										tsfModified.getName());
				return true;
			}
			else
			{
				System.out.println("Incompatible modification types for mod projection");
			}
		}
		else if (term.getKeywordArg(":AFFECTED") != null && 
				term.getKeywordArg(":INSTANCE-OF") != null)
		{
			String instanceOf = term.getKeywordArg(":INSTANCE-OF").stringValue();
			String affected = term.getKeywordArg(":AFFECTED").stringValue();
			String variable = term.get(1).stringValue();
			if (instanceOf.equalsIgnoreCase("ONT::INCREASE") || 
					instanceOf.equalsIgnoreCase("ONT::DECREASE"))
			{
				if (!variableFGBindings.containsKey(variable))
				{
					System.out.println("No figure for modification through affected");
					return false;
				}
				if (!variableFGBindings.containsKey(affected))
				{
					System.out.println("No figure for modification through affected");
					return false;
				}
				
				FeatureGroup modifier = variableFGBindings.get(variable);
				FeatureGroup modified = variableFGBindings.get(affected);
				
				System.out.println("Trying affected projection");
				
				if (modified instanceof TemporalSequenceFeature && 
						modifier instanceof TemporalSequenceFeature)
				{
					TemporalSequenceFeature tsfModifier = (TemporalSequenceFeature)modifier;
					TemporalSequenceFeature tsfModified = (TemporalSequenceFeature)modified;
					
					tsfModifier.projectOnto(tsfModified);
					tsfModified.straighten();
					System.out.println("Projected " + tsfModifier.getName() +
							" onto sequence " + tsfModified.getName());
					return true;
				}
				else
				{
					System.out.println("Incompatible modification types for affected projection");
				}
				
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
			System.out.println("Current origin: " + origin.getValue().toString());
			switch (instance.stringValue().toUpperCase())
			{
			case "ONT::NUMBER":
				int value = Integer.parseInt(term.getKeywordArg(":VALUE").stringValue());
				CountFeature cf = new CountFeature("count");
				cf.setValue(value);
				variableFGBindings.put(variable,cf);
				System.out.println("Extracted count of " + cf.getValue());
				return true;
			case "ONT::REFERENTIAL-SEM":
			case "ONT::BLOCK":
				
				TemporalSequenceFeature block;
				
				if (useNextLocation)
				{
					if (upwards)
						block = buildTower(nextLocation,"ONT::BLOCK",1);
					else
						block = buildRow(nextLocation,"ONT::BLOCK",1);
				}
				else
				{
					if (upwards)
						block = buildTower(origin,"ONT::BLOCK",1);
					else
						block = buildRow(origin,"ONT::BLOCK",1);
				}
//				if (useNextLocation)
//					block.getOrigin().setValue(nextLocation.getValue());
//				else
//					block.getOrigin().setValue(origin.getValue());
				variableFGBindings.put(variable,block);
				System.out.println("Extracted block");
				return true;
			case "ONT::ROW-FORMATION":
				TemporalSequenceFeature row = new TemporalSequenceFeature("row");
				if (useNextLocation)
				{
					row.getOrigin().setValue(nextLocation.getValue());
					// Remove if not working
					//buildAction.getBuildSequence().getOrigin().setValue(nextLocation.getValue());
				}
				else
				{
					row.getOrigin().setValue(origin.getValue());
					// Remove if not working
					//buildAction.getBuildSequence().getOrigin().setValue(origin.getValue());
				}
				variableFGBindings.put(variable,row);
				System.out.println("Extracted row");
				return true;
			case "ONT::TOWER":
				TemporalSequenceFeature tower = new TemporalSequenceFeature("tower");
				if (useNextLocation)
				{
					tower.getOrigin().setValue(nextLocation.getValue());
					// Remove if not working
					//buildAction.getBuildSequence().getOrigin().setValue(nextLocation.getValue());
				}
				else
				{
					tower.getOrigin().setValue(origin.getValue());
					// Remove if not working
					//buildAction.getBuildSequence().getOrigin().setValue(origin.getValue());
				}
				variableFGBindings.put(variable,tower);
				System.out.println("Extracted tower");				
			case "ONT::SET":
				if (elementTypeObject == null)
					return false;
				String elementType = elementTypeObject.stringValue();
				UnorderedGroupingFeature ugf = new UnorderedGroupingFeature("set");
				
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
				
				ugf.getCountFeature().setValue(number);
				// This is a hack - fix!
				FeatureGroup set;
				if (useNextLocation)
					set = buildRow(nextLocation,elementType,number);
				else
					set = buildRow(origin,elementType,number);
				
				System.out.println("Built set: " + set);
				variableFGBindings.put(variable,set);
				return true;
			case "ONT::INCREASE":
				TemporalSequenceFeature increase = new TemporalSequenceFeature("increase");
				for (int i = 1; i < 11; i++)
				{
					CountFeature cf2 = new CountFeature("count");
					cf2.setValue(i);
					increase.add(cf2);
				}
				System.out.println("Built increase feature: " + increase);
				variableFGBindings.put(variable,increase);
				return true;
			case "ONT::DECREASE":
				TemporalSequenceFeature decrease = new TemporalSequenceFeature("decrease");
				for (int i = 1; i < 5; i++)
				{
					CountFeature cf2 = new CountFeature("count");
					cf2.setValue(i);
					decrease.add(cf2);
				}
				System.out.println("Built increase feature: " + decrease);
				variableFGBindings.put(variable,decrease);
				return true;
			case "ONT::HEIGHT-SCALE":
				TemporalSequenceFeature height = new TemporalSequenceFeature("ONT::HEIGHT-SCALE");
				for (int i = 1; i < 11; i++)
				{
					CountFeature cf2 = new CountFeature("height");
					cf2.setValue(1); // Just set to default of 1
					height.add(cf2);
				}
				System.out.println("Built height feature:" + height);
				variableFGBindings.put(variable, height);
				return true;
			case "ONT::WIDTH-SCALE":
				TemporalSequenceFeature width = new TemporalSequenceFeature("ONT::WIDTH-SCALE");
				for (int i = 1; i < 11; i++)
				{
					CountFeature cf2 = new CountFeature("width");
					cf2.setValue(1); // Just set to default of 1
					width.add(cf2);
				}
				System.out.println("Built width feature:" + width);
				variableFGBindings.put(variable, width);
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
		row.getOrigin().setValue(origin.getValue());
		for (int i = 0; i < size; i++)
		{
			if (elementType.equalsIgnoreCase("ONT::TOWER"))
			{
				PointFeature nextOrigin = row.getNextInDirection(Block.BLOCK_WIDTH);
				row.add(buildTower(nextOrigin,1));
			}
			else if (elementType.equalsIgnoreCase("ONT::BLOCK"))
			{
				PointFeature nextOrigin = row.getNextInDirection(Block.BLOCK_WIDTH);
				row.add(buildTower(nextOrigin,1));
			}
		}
		nextLocation = row.getNextInDirection(Block.BLOCK_WIDTH);
		System.out.println("Next location: " + nextLocation.getValue());
		
		return row;
	}
	
	private TemporalSequenceFeature buildTower(PointFeature origin, String elementType, int size)
	{
		System.out.println("Building row");
		TemporalSequenceFeature row = new TemporalSequenceFeature("row");
		row.getDirectionFeature().setValue(new DoubleMatrix(new double[]{0,0,1}));
		row.getOrigin().setValue(origin.getValue());
		for (int i = 0; i < size; i++)
		{
			if (elementType.equalsIgnoreCase("ONT::TOWER"))
			{
				PointFeature nextOrigin = row.getNextInDirection(Block.BLOCK_WIDTH);
				row.add(buildTower(nextOrigin,1));
			}
			else if (elementType.equalsIgnoreCase("ONT::BLOCK"))
			{
				PointFeature nextOrigin = row.getNextInDirection(Block.BLOCK_WIDTH);
				row.add(buildTower(nextOrigin,1));
			}
		}
		nextLocation = row.getNextInDirection(Block.BLOCK_WIDTH);
		System.out.println("Next location: " + nextLocation.getValue());
		
		return row;
	}
	
	private TemporalSequenceFeature buildTower(PointFeature origin, int height)
	{
		System.out.println("Building tower");
		TemporalSequenceFeature tower = new TemporalSequenceFeature("tower");
		tower.getOrigin().setValue(origin.getValue());
		tower.getDirectionFeature().setValue(new DoubleMatrix(new double[]{0,0,1}));
		
		for (int i = 0; i < height; i++)
		{
			PointFeature pf = tower.getNextInDirection(Block.BLOCK_WIDTH);
			Block b = new Block(pf.getValue());
			BlockFeatureGroup bfg = new BlockFeatureGroup(b);
			bfg.setPointFeature(pf);
			tower.add(bfg);
		}
		
		nextLocation = tower.getNextInDirection(Block.BLOCK_WIDTH);
		System.out.println("Next location: " + nextLocation.getValue());
		return tower;
	}
	
	
}
