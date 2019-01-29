package features;

import java.util.*;

import org.jblas.DoubleMatrix;

import environment.Block;
import spatialreasoning.Predicate;


public class UnorderedRowFeature extends UnorderedGroupingFeature {

	private UnorderedGroupingFeature endpointFeature;
	
	public UnorderedRowFeature(String name) {
		super(name);
		endpointFeature = new UnorderedGroupingFeature(FeatureConstants.END);
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
					//System.out.println("Added block to row");
				}
			}
			
			rows.add(row);
		}
		
		return rows;
	}
	
	private void generateEndpointFeature()
	{
		double maxX = Double.MIN_VALUE;
		double minX = Double.MAX_VALUE;
		
		// If there are less than 3 elements the endpoints are just the elements
		if (elements.size() < 3)
		{
			for (FeatureGroup element : elements)
				endpointFeature.add(element);
			return;
		}
		FeatureGroup startFg = null, endFg = null;
		for (FeatureGroup fg : elements)
		{

			double centerX = 0;

			DoubleMatrix center = (DoubleMatrix)fg.getFeatures()
									.get(FeatureConstants.LOCATION).getValue();
			if (center != null)
				centerX = center.get(0);

			
			if (centerX < minX)
			{
				minX = centerX;
				startFg = fg;
			}
			
			if (centerX > maxX)
			{
				maxX = centerX;
				endFg = fg;
			}
			
		}

		endpointFeature.add(startFg);
		endpointFeature.add(endFg);
		
		
		
		
	}
	
	public UnorderedGroupingFeature getEndpointFeature()
	{
		generateEndpointFeature();
		return endpointFeature;
	}
	
	@Override
	public Map<String,Feature> getFeatures() {
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.putAll(super.getFeatures());
		result.put(endpointFeature.getName(),endpointFeature);
		return result;
	}
}
