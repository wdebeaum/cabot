package features;

import java.util.*;

import environment.Block;

public class BlockFeatureGroup implements FeatureGroup {
	
	public Block block;
	public Map<String,Feature> features;
	
	public BlockFeatureGroup(Block b) {
		block = b;
		features = new HashMap<String,Feature>();
		generateFeatures();
	}
	
	private void generateFeatures()
	{
		generatePointFeature();
		generateWidthFeature();
	}
	
	private void generatePointFeature()
	{
		PointFeature result = new PointFeature("position");
		result.setValue(block.position);
		setFeature(result);	
	}
	
	public void generateWidthFeature()
	{
		DistanceFeature result = new DistanceFeature("width");
		result.setValue(Block.BLOCK_WIDTH);
		setFeature(result);	
	}
	
	public PointFeature getPointFeature()
	{
		return (PointFeature)features.get("position");
	}

	@Override
	public Collection<Feature> getFeatures() {
		// TODO Auto-generated method stub
		return null;
	}
	
	private void setFeature(Feature feature)
	{
		features.put(feature.getName(),feature);
	}
	
	public void setPointFeature(PointFeature pf)
	{
		features.put("position", pf);
		block.position = pf.getValue();
	}
	
	public String toString()
	{
		return "Block FG at: " + block.position;
	}

}
