package features;

import java.util.*;

import environment.Block;
import spatialreasoning.Predicate;


public class UnorderedRowFeature extends UnorderedGroupingFeature {

	public UnorderedRowFeature(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}
	
	
	public static List<UnorderedRowFeature> rowsFromBlocks(Collection<Block> blocks)
	{
		Set<Block> unusedBlocks = new HashSet<Block>(blocks);
		ArrayList<UnorderedRowFeature> rows = new ArrayList<UnorderedRowFeature>();
		
		for (Block b1 : blocks)
		{
			if (!unusedBlocks.contains(b1))
				continue;
			UnorderedRowFeature row = new UnorderedRowFeature(FeatureConstants.ROW);
			
			unusedBlocks.remove(b1);
			row.elements.add(new BlockFeatureGroup(b1));
			Iterator<Block> unusedBlocksIterator = unusedBlocks.iterator();
			
			while (unusedBlocksIterator.hasNext())
			{
				Block b2 = unusedBlocksIterator.next();
				if (Predicate.atSameY(b1,b2) && Predicate.atSameHeight(b1,b2))
				{
					row.elements.add(new BlockFeatureGroup(b2));
					unusedBlocksIterator.remove();
					System.out.println("Added block to row");
				}
			}
			
			rows.add(row);
		}
		
		return rows;
	}
	
	@Override
	public Map<String,Feature> getFeatures() {
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.putAll(super.getFeatures());
		return result;
	}
}
