package features;

import java.util.*;
import features.BlockFeatureGroup;

import org.jblas.DoubleMatrix;

import environment.Block;
import environment.StructureInstance;


public class UnorderedGroupingFeature extends Feature<List> {

	protected List<FeatureGroup> elements;
	protected CountFeature count;
	protected DistanceFeature heightFeature;
	protected DistanceFeature widthFeature;
	protected PointFeature centerFeature;
	
	public UnorderedGroupingFeature(String name) {
		super(name);
		count = new CountFeature(FeatureConstants.NUMBER);
		heightFeature = new DistanceFeature(FeatureConstants.HEIGHT);
		widthFeature = new DistanceFeature(FeatureConstants.WIDTH);
		centerFeature = new PointFeature(FeatureConstants.CENTER);
		elements = new ArrayList<FeatureGroup>();
	}

	@Override
	public List<FeatureGroup> getValue() {
		// TODO Auto-generated method stub
		return elements;
	}

	@Override
	public void setValue(List newValue) {
		elements = newValue;
		generateFeatures();
	}
	
	
	public CountFeature getCountFeature()
	{
		count.setValue(elements.size());
		return count;
	}
	
	public StructureInstance getStructureInstance()
	{
		return new StructureInstance(name,getBlocks());
	}
	
	public List<BlockFeatureGroup> getBlockFeatureGroups()
	{
		List<BlockFeatureGroup> bfgs = new ArrayList<BlockFeatureGroup>();
		for (FeatureGroup fg : elements)
		{
			if (fg instanceof BlockFeatureGroup)
				bfgs.add((BlockFeatureGroup)fg);
			else if (fg instanceof UnorderedGroupingFeature)
				bfgs.addAll(((UnorderedGroupingFeature)fg).getBlockFeatureGroups());
		}
		
		return bfgs;
	}
	
	public List<Block> getBlocks()
	{
		List<BlockFeatureGroup> bfgs = getBlockFeatureGroups();
		List<Block> toReturn = new ArrayList<Block>();
		
		for (BlockFeatureGroup bfg : bfgs)
			toReturn.add(bfg.block);
		
		return toReturn;
	}
	
	public void add(FeatureGroup fg)
	{
		elements.add(fg);
		generateFeatures();
	}
	
	private void generateCenterFeature()
	{
		DoubleMatrix center = DoubleMatrix.zeros(3);
		List<Block> blocks = getBlocks();
		for (Block b : blocks)
		{
			center.addi(b.position);
		}
		
		center.divi(blocks.size());
		
		centerFeature.setValue(center);
	}
	
	private void generateHeightFeature()
	{
		double maxHeight = 0;
		for (BlockFeatureGroup fg : getBlockFeatureGroups())
		{
			double height = 0;

			Map<String, Feature> features = ((BlockFeatureGroup)fg).getFeatures();
			double originY = 0;
			double blockHeight = 0;
			
			if (features.containsKey(FeatureConstants.HEIGHT))
				blockHeight = (Double)features.get(FeatureConstants.HEIGHT).getValue();
			
			if (features.containsKey(FeatureConstants.LOCATION))
				originY = ((DoubleMatrix)features.get(FeatureConstants.LOCATION).getValue()).get(2);
			
			height = (originY / blockHeight) + .5;


			if (height > maxHeight)
				maxHeight = height;
		}

		heightFeature.setValue(maxHeight);
		
	}
	
	private void generateWidthFeature()
	{
		double maxX = Double.MIN_VALUE;
		double minX = Double.MAX_VALUE;
		for (BlockFeatureGroup fg : getBlockFeatureGroups())
		{
			double width = 0;
			double centerX = 0;

			
			Map<String, Feature> features = ((BlockFeatureGroup)fg).getFeatures();

			if (features.containsKey(FeatureConstants.WIDTH))
				width = (Double)features.get(FeatureConstants.WIDTH).getValue();
			
			if (features.containsKey(FeatureConstants.LOCATION))
				centerX = ((DoubleMatrix)features.get(FeatureConstants.LOCATION).getValue()).get(2);


			
			if (centerX - width / 2 < minX)
				minX = centerX - width / 2;
			
			if (centerX + width / 2 > maxX)
				maxX = centerX + width / 2;
			
		}

		widthFeature.setValue((maxX - minX) / Block.BLOCK_WIDTH );
		
	}
	
	public DistanceFeature getHeightFeature()
	{
		generateHeightFeature();
		return heightFeature;
	}
	
	public PointFeature getCenterFeature()
	{
		generateCenterFeature();
		return centerFeature;
	}
	
	public DistanceFeature getWidthFeature()
	{
		generateWidthFeature();
		return widthFeature;
	}
	
	public boolean isEmpty()
	{
		return elements.isEmpty();
	}
	
	public TemporalSequenceFeature projectOnto(TemporalSequenceFeature onto)
	{
		for (FeatureGroup element : elements)
			onto.add(element);
		
		return onto;
	}
	
	private void generateFeatures()
	{
		generateHeightFeature();
		generateWidthFeature();
		generateCenterFeature();
		generateCountFeature();
	}
	
	private void generateCountFeature() {
		
		count.setValue(elements.size());
		
	}

	public Map<String,Feature> getFeatures()
	{
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		generateFeatures();
		result.putAll(super.getFeatures());
		result.put(count.name, count);
		result.put(heightFeature.name, heightFeature);
		result.put(FeatureConstants.SIZE, count);
		return result;
	}

}
