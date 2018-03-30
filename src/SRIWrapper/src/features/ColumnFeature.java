package features;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jblas.DoubleMatrix;

import environment.Block;
import spatialreasoning.Predicate;

public class ColumnFeature extends UnorderedGroupingFeature {

	public ColumnFeature(String name) {
		super(name);
//		direction = new DirectionFeature(FeatureConstants.DIRECTION);
//		direction.setValue(new DoubleMatrix(new double[]{0,0,1}));
//		origin = new PointFeature(FeatureConstants.ORIGIN);
//		origin.setValue(new DoubleMatrix(new double[]{0,0,Block.BLOCK_WIDTH/2}));
	}
	
	public static List<ColumnFeature> columnsFromBlocks(Collection<Block> blocks)
	{
		Set<Block> unusedBlocks = new HashSet<Block>(blocks);
		ArrayList<ColumnFeature> columns = new ArrayList<ColumnFeature>();
		
		for (Block b1 : blocks)
		{
			
			if (!unusedBlocks.contains(b1))
				continue;
			
			System.out.println("Starting column with block " + b1.getId());
			ColumnFeature column = new ColumnFeature(FeatureConstants.COLUMN);
			
			unusedBlocks.remove(b1);
			column.add(new BlockFeatureGroup(b1));
			Iterator<Block> unusedBlocksIterator = unusedBlocks.iterator();
			
			while (unusedBlocksIterator.hasNext())
			{
				Block b2 = unusedBlocksIterator.next();
				if (Predicate.above(b1,b2) || Predicate.below(b1,b2))
				{
					column.add(new BlockFeatureGroup(b2));
					unusedBlocksIterator.remove();
					System.out.println("Added block " + b2.getId() + " to column");
				}
			}
			
			columns.add(column);
		}
		System.out.println(columns.size() + " columns extracted");
		return columns;
	}
	
	@Override
	public Map<String,Feature> getFeatures() {
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.putAll(super.getFeatures());
		return result;
	}

}
