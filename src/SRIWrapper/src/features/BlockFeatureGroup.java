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
		generateHeightFeature();
	}
	
	private void generatePointFeature()
	{
		PointFeature result = new PointFeature("position");
		result.setValue(block.position);
		setFeature(result);	
	}
	
	private void generateWidthFeature()
	{
		DistanceFeature result = new DistanceFeature("ONT::WIDTH");
		DistanceFeature resultScale = new DistanceFeature("ONT::WIDTH-SCALE");
		result.setValue(Block.BLOCK_WIDTH);
		resultScale.setValue(Block.BLOCK_WIDTH);
		setFeature(result);
		setFeature(resultScale);
	}
	
	private void generateHeightFeature()
	{
		DistanceFeature result = new DistanceFeature("ONT::HEIGHT-SCALE");
		result.setValue(Block.BLOCK_WIDTH);
		setFeature(result);	
	}
	
	public PointFeature getPointFeature()
	{
		return (PointFeature)features.get("ONT::LOCATION");
	}

	@Override
	public Map<String,Feature> getFeatures() {
		// TODO Auto-generated method stub
		return features;
	}
	
	private void setFeature(Feature feature)
	{
		features.put(feature.getName(),feature);
	}
	
	public void setPointFeature(PointFeature pf)
	{
		features.put("ONT::LOCATION", pf);
		block.position = pf.getValue();
	}
	
	public String toString()
	{
		return "Block FG at: " + block.position;
	}

}
