package features;

import java.util.*;

import org.jblas.DoubleMatrix;

import features.*;
import environment.Block;

public class TemporalSequenceFeature extends Feature<List> implements FeatureGroup{

	private List<FeatureGroup> sequence;
	private DirectionFeature direction;
	private CountFeature count;
	private PointFeature origin;
	
	public TemporalSequenceFeature(String name) {
		super(name);
		sequence = new ArrayList<FeatureGroup>();
		direction = new DirectionFeature("direction");
		direction.setValue(new DoubleMatrix(new double[]{1,0,0}));
		origin = new PointFeature("origin");
		origin.setValue(new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH/2}));
		count = new CountFeature("count");
	}
	
	public TemporalSequenceFeature projectOnto(DirectionFeature original, DirectionFeature newDirection)
	{
		int originalMaxAxis = original.getMaxAxis();
		int newMaxAxis = newDirection.getMaxAxis();
		
		Collection<FeatureGroup> sortedPositions = sortedPositions(original);
		double minZ = Double.MAX_VALUE;
		for (FeatureGroup fg : sortedPositions)
		{
			((BlockFeatureGroup)fg).getPointFeature().getValue().swapRows(originalMaxAxis, newMaxAxis);
			// Make sure everything is above the table
			if (((BlockFeatureGroup)fg).getPointFeature().getValue().get(2) < minZ)
				minZ = ((BlockFeatureGroup)fg).getPointFeature().getValue().get(2);
		}
		
		for (FeatureGroup fg : sortedPositions)
		{
			double oldZ = ((BlockFeatureGroup)fg).getPointFeature().getValue().get(2);
			double newValue = oldZ - minZ + Block.BLOCK_WIDTH / 2;
			// Make sure everything is above the table
			((BlockFeatureGroup)fg).getPointFeature().getValue().put(2,newValue);
;
			
			
		}
		TemporalSequenceFeature result = new TemporalSequenceFeature(name);
		
		result.setSequence(new ArrayList<FeatureGroup>(sortedPositions));
		result.direction = newDirection;
		return result;
		
	}
	
	public void straighten()
	{
		if (!sequence.isEmpty() && sequence.get(0) instanceof BlockFeatureGroup)
		{
			System.out.println("Old block value: " + ((BlockFeatureGroup)sequence.get(0)).getPointFeature()); 
			((BlockFeatureGroup)sequence.get(0)).setPointFeature(origin);
			System.out.println("New block value: " + ((BlockFeatureGroup)sequence.get(0)).getPointFeature());
		}
		else
		{
			System.out.println("Straightening sub feature groups");
			for (FeatureGroup fg : sequence)
			{
				if (fg instanceof TemporalSequenceFeature)
					((TemporalSequenceFeature)fg).straighten();
			}
		}
			
	}
	
	//Currently projects count features onto sequence of sequences
	public TemporalSequenceFeature projectOnto(TemporalSequenceFeature onto)
	{
		System.out.println("Projecting ");
		System.out.println(this);
		System.out.println(" onto ");
		System.out.println(onto);
		int index = 0;
		for (FeatureGroup fg : sequence)
		{
			if (fg instanceof CountFeature)
			{
				System.out.println("Projecting counts");
				CountFeature cf = (CountFeature)fg;
				if (index == onto.sequence.size())
					break;
				FeatureGroup ontoFg = onto.sequence.get(index);
				
				if (ontoFg instanceof TemporalSequenceFeature)
				{
					System.out.println("Extending to: " + cf.getValue());
					((TemporalSequenceFeature) ontoFg).extendToSize(cf.getValue(), Block.BLOCK_WIDTH);
				}
				
			}
			else if (fg instanceof TemporalSequenceFeature)
			{
				System.out.println("Projecting sequence" );
				

				PointFeature nextPosition = onto.getNextInDirection(Block.BLOCK_WIDTH);
				System.out.println("Next position: " + nextPosition.getValue());
				if (index < onto.sequence.size())
				{
					if (onto.sequence.get(index) instanceof TemporalSequenceFeature)
					{
						System.out.println("Old element:" + onto.sequence.get(index));
						PointFeature pf = ((TemporalSequenceFeature)onto.sequence.get(index)).getOrigin();
						
						onto.sequence.set(index, sequence.get(index));
						((TemporalSequenceFeature)sequence.get(index)).getOrigin().setValue(pf.getValue());
						System.out.println("New element:" + onto.sequence.get(index));
						
					}
				}
				else
				{
					((TemporalSequenceFeature)sequence.get(index)).getOrigin().setValue(nextPosition.getValue());
					onto.addToSequence(sequence.get(index));
					
					
				}
				
			}
			index++;
		}
		
		return onto;
	}
	
	
	public Collection<FeatureGroup> sortedPositions(DirectionFeature direction)
	{
		int maxAxis = direction.getMaxAxis();

		TreeMap<Double,FeatureGroup> toSort = new TreeMap<Double,FeatureGroup>();
		for (FeatureGroup fg : sequence)
		{
			if (fg instanceof BlockFeatureGroup)
			{
				BlockFeatureGroup bfg = (BlockFeatureGroup)fg;
				toSort.put(bfg.getPointFeature().getValue().get(maxAxis), bfg);
			}
			else if (fg instanceof TemporalSequenceFeature)
			{
				TemporalSequenceFeature tsf = (TemporalSequenceFeature)fg;
				toSort.put(tsf.getOrigin().getValue().get(maxAxis), tsf);
			}
		}
		if (direction.getValue().get(maxAxis) > 0)
			return toSort.values();
		else
			return toSort.descendingMap().values();
	}
	
	public void directionalCompare(DirectionFeature direction)
	{
		
	}
	
	public void setSequence(List<FeatureGroup> sequence)
	{
		this.sequence = sequence;
		this.count.setValue(sequence.size());
	}
	
	public PointFeature getNextInDirection(double interval)
	{
		System.out.println("Calling getNextInDirection");
		if (sequence.isEmpty())
		{
			return origin;
		}
		
		Collection<FeatureGroup> sortedPositions = sortedPositions(direction);
		int i = 0;
		FeatureGroup lastFg = null;
		for (FeatureGroup fg : sortedPositions)
		{
			System.out.println(fg.toString());
			if (i == sortedPositions.size() - 1)
				lastFg = fg;
			
			i++;
		}
		if (lastFg == null)
		{
			System.out.println("error getting next position: no last FG");
			return null;
		}
		if (lastFg instanceof TemporalSequenceFeature)
		{
			DoubleMatrix lastValue = ((TemporalSequenceFeature)lastFg).getOrigin().getValue();
			PointFeature newPF = new PointFeature("origin");
			DoubleMatrix newValue = lastValue.add(direction.getValue()
									.mul(new DoubleMatrix(new double[]{interval,interval,interval})));
			newPF.setValue(newValue);
			return newPF;
		}
		if (lastFg instanceof BlockFeatureGroup)
		{
			DoubleMatrix lastValue = ((BlockFeatureGroup)lastFg).getPointFeature().getValue();
			PointFeature newPF = new PointFeature("origin");
			DoubleMatrix newValue = lastValue.add(direction.getValue()
									.mul(new DoubleMatrix(new double[]{interval,interval,interval})));
			newPF.setValue(newValue);
			return newPF;
		}
		
		System.out.println("error getting next position");
		return null;
	}
	
	public void addToSequence(FeatureGroup fg)
	{
		sequence.add(fg);
	}
	
	public FeatureGroup getLastFeatureGroupInDirection()
	{
		Collection<FeatureGroup> sortedPositions = sortedPositions(direction);
		int i = 0;
		FeatureGroup lastFg = null;
		for (FeatureGroup fg : sortedPositions)
		{
			if (i == sortedPositions.size() - 1)
				lastFg = fg;
		}
		return lastFg;	
	}
	
	public boolean extendToSize(int newSize, double interval)
	{
		int difference = newSize - sequence.size();
		
		FeatureGroup lastFg = getLastFeatureGroupInDirection();
		System.out.println("Extending TSF by " + difference);
		for (int i = 0; i < difference; i++)
		{
			
			if (lastFg instanceof TemporalSequenceFeature)
			{
				return false;
			}
			if (lastFg instanceof BlockFeatureGroup)
			{
				DoubleMatrix lastValue = ((BlockFeatureGroup)lastFg).getPointFeature().getValue();
				PointFeature newPF = new PointFeature("origin");
				DoubleMatrix newValue = lastValue.add(direction.getValue()
										.mul(new DoubleMatrix(new double[]{interval,interval,interval})));
				
				System.out.println("Position of new block: " + newValue);
				Block b = new Block(newValue);
				BlockFeatureGroup bfg = new BlockFeatureGroup(b);
				addToSequence(bfg);
				lastFg = bfg;
			}
		}
		if (difference > 0)
			return true;
		
		return false;
	}
	
	public FeatureGroup getNextInDirection()
	{
		List<Double> intervals = new ArrayList<Double>();
		BlockFeatureGroup lastBfg = null;
		for (FeatureGroup fg : sequence)
		{

			System.out.println("Sequence element");
			if (fg instanceof BlockFeatureGroup)
			{
				
				BlockFeatureGroup bfg = (BlockFeatureGroup)fg;
				
				
				if (lastBfg != null)
				{
					System.out.println("Adding interval");
					intervals.add(Math.abs(bfg.getPointFeature().getValue().get(direction.getMaxAxis()) -
							lastBfg.getPointFeature().getValue().get(direction.getMaxAxis())));
				}
				else
				{
					lastBfg = bfg;
				}
				lastBfg = bfg;
			}
		}
		DoubleMatrix lastPosition;
		BlockFeatureGroup newBfg;
		if (sequence.size() == 0)
		{
			lastPosition = new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH/2});
			Block newBlock = new Block(lastPosition);
			newBfg = new BlockFeatureGroup(newBlock);
			System.out.println("Last in position: " + lastPosition.toString());
		}
		else
		{
			lastPosition = ((BlockFeatureGroup)(sequence.get(sequence.size()-1))).getPointFeature().getValue();
			DoubleMatrix interval = DoubleMatrix.zeros(3);
			interval.put(direction.getMaxAxis(), intervals.get(0));
			Block newBlock = new Block(lastPosition.add(interval));
			newBfg = new BlockFeatureGroup(newBlock);
			System.out.println("Last in position: " + lastPosition.toString());
		}
		
		return newBfg;
	}

	@Override
	public List getValue() {
		// TODO Auto-generated method stub
		return sequence;
	}
	
	public PointFeature getOrigin()
	{
		return origin;
	}


	@Override
	public void setValue(List newValue) {
		this.sequence = newValue;
		
	}
	
	public CountFeature getCountFeature()
	{
		count.setValue(sequence.size());
		return count;
	}
	
	public DirectionFeature getDirectionFeature()
	{
		return direction;
	}

	@Override
	public List<Feature> getFeatures() {
		// TODO Auto-generated method stub
		return null;
	}
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		
		sb.append("Temporal Sequence: " + name + "\n");
		sb.append("Direction: " + direction.getValue() + "\n");
		sb.append("Origin: " + origin + "\n");
		for (FeatureGroup fg : sequence)
		{
			sb.append("\t" + fg.toString() + "\n");
		}
		sb.append("\n");
		return sb.toString();
	}
	
	public List<BlockFeatureGroup> getBlockFeatureGroups()
	{
		List<BlockFeatureGroup> result = new ArrayList<BlockFeatureGroup>();
		for (FeatureGroup fg : sequence)
		{
			if (fg instanceof BlockFeatureGroup)
				result.add((BlockFeatureGroup)fg);
			else if (fg instanceof TemporalSequenceFeature)
				result.addAll(((TemporalSequenceFeature)fg).getBlockFeatureGroups());
		}
		
		return result;
	}
}
