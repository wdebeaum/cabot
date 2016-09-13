package features;

import java.util.*;

import org.jblas.DoubleMatrix;

import features.*;
import environment.Block;

public class TemporalSequenceFeature extends Feature<List> implements FeatureGroup{

	private List<FeatureGroup> sequence;
	private String name;
	private DirectionFeature direction;
	
	public TemporalSequenceFeature(String name) {
		super(name);
		sequence = new ArrayList<FeatureGroup>();
		direction = new DirectionFeature("xDirection");
		direction.setValue(new DoubleMatrix(new double[]{1,0,0}));
	}

	public TemporalSequenceFeature projectOnto(DirectionFeature original, DirectionFeature newDirection)
	{
		int originalMaxAxis = original.getMaxAxis();
		int newMaxAxis = newDirection.getMaxAxis();
		
		Collection<FeatureGroup> sortedPositions = sortedPositions(original);
		
		for (FeatureGroup fg : sortedPositions)
		{
			((BlockFeatureGroup)fg).getPointFeature().getValue().swapRows(originalMaxAxis, newMaxAxis);
		}
		
		TemporalSequenceFeature result = new TemporalSequenceFeature(name);
		
		result.setSequence(new ArrayList<FeatureGroup>(sortedPositions));
		result.direction = newDirection;
		return result;
		
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
	}
	
	public FeatureGroup getNextInDirection()
	{
		List<Double> intervals = new ArrayList<Double>();
		BlockFeatureGroup lastBfg = null;
		for (FeatureGroup fg : sequence)
		{
			if (fg instanceof BlockFeatureGroup)
			{
				BlockFeatureGroup bfg = (BlockFeatureGroup)fg;
				
				if (lastBfg != null)
				{
					intervals.add(bfg.getPointFeature().getValue().get(direction.getMaxAxis()) -
							lastBfg.getPointFeature().getValue().get(direction.getMaxAxis()));
				}
				lastBfg = bfg;
			}
		}
		DoubleMatrix lastPosition = ((BlockFeatureGroup)(sequence.get(sequence.size()-1))).getPointFeature().getValue();
		DoubleMatrix interval = DoubleMatrix.zeros(3);
		interval.put(direction.getMaxAxis(), intervals.get(0));
		Block newBlock = new Block(lastPosition.add(interval));
		BlockFeatureGroup newBfg = new BlockFeatureGroup(newBlock);
		
		return newBfg;
	}

	@Override
	public List getValue() {
		// TODO Auto-generated method stub
		return sequence;
	}

	@Override
	public void setValue(List newValue) {
		this.sequence = sequence;
		
	}

	@Override
	public List<Feature> getFeatures() {
		// TODO Auto-generated method stub
		return null;
	}
}
