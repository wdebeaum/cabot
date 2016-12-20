package environment;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.jblas.DoubleMatrix;

import features.*;

public class StructureInstance implements FeatureGroup {

	public String name;
	public Set<Block> blocks;
	private HashMap<String,Feature> features;
	
	public StructureInstance(String name, Collection<Block> blocks)
	{
		this.blocks = new HashSet<Block>();
		this.blocks.addAll(blocks);
		features = new HashMap<String, Feature>();
		generateFeatures();
	}
	
	public void generateFeatures()
	{
		generateShapeRatioFeature();
		generateBlockNumberFeature();
		generateLinearityFeature();
		generateTemporalSequenceFeature();
		generateDimensionFeatures();
	}
	
	// Return height / width
	private void generateShapeRatioFeature()
	{
		double minX = Double.POSITIVE_INFINITY;
		double maxX = Double.NEGATIVE_INFINITY;
		double minZ = Double.POSITIVE_INFINITY;
		double maxZ = Double.NEGATIVE_INFINITY;
		
		for (Block b : blocks)
		{
			if (b.getX() < minX)
				minX = b.getX();
			if (b.getX() > maxX)
				maxX = b.getX();
			if (b.getZ() < minZ)
				minZ = b.getZ();
			if (b.getZ() > maxZ)
				maxZ = b.getZ();
		}
		
		maxZ += Block.BLOCK_WIDTH / 2;
		minZ -= Block.BLOCK_WIDTH / 2;
		maxX += Block.BLOCK_WIDTH / 2;
		minX -= Block.BLOCK_WIDTH / 2;
		
		double ratio = (maxZ - minZ) / (maxX - minX);
		DecimalFeature ratioFeature = new DecimalFeature("shaperatio");
		ratioFeature.setValue(ratio);
		setFeature(ratioFeature);
	}
	
	private void generateBlockNumberFeature()
	{
		CountFeature countFeature = new CountFeature("number");
		countFeature.setValue(blocks.size());
		setFeature(countFeature);
	}
	
	private void generateLinearityFeature()
	{
		double Z = 0;
		double averageX = 0;
		double averageY = 0;
		double averageZ = 0;
		
		for (Block b : blocks)
		{
			averageX += b.getX();
			averageY += b.getY();
			averageZ += b.getZ();
		}
		
		averageX /= blocks.size();
		averageY /= blocks.size();
		averageZ /= blocks.size();
		
		double varianceX = 0;
		double varianceY = 0;
		double varianceZ = 0;
		
		for (Block b : blocks)
		{
			varianceX += (b.getX() - averageX) *  (b.getX() - averageX);
			varianceY += (b.getY() - averageY) *  (b.getY() - averageY);
			varianceZ += (b.getZ() - averageZ) *  (b.getZ() - averageZ);
		}
		
		DecimalFeature xLinearity = new DecimalFeature("xlinearity");
		xLinearity.setValue(1.0 / Math.min(varianceY, varianceZ));
		setFeature(xLinearity);
		DecimalFeature yLinearity = new DecimalFeature("ylinearity");
		yLinearity.setValue(1.0 / Math.min(varianceX, varianceZ));
		setFeature(yLinearity);
		DecimalFeature zLinearity = new DecimalFeature("zlinearity");
		zLinearity.setValue(1.0 / Math.min(varianceX, varianceY));
		setFeature(zLinearity);
		
	}
	
	private void setFeature(Feature feature)
	{
		Feature existingFeature = features.get(feature.getName());
		if (existingFeature == null)
			features.put(feature.getName(), feature);
		else
			existingFeature.setValue(feature.getValue());
	}
	
	private void generateDimensionFeatures()
	{
		DistanceFeature heightFeature = new DistanceFeature("ONT::HEIGHT-SCALE");
		DistanceFeature widthScaleFeature = new DistanceFeature("ONT::WIDTH-SCALE");
		DistanceFeature widthFeature = new DistanceFeature("ONT::WIDTH");
		DistanceFeature radiusFeature = new DistanceFeature("W::RADIUS");
		DistanceFeature diameterFeature = new DistanceFeature("W::DIAMETER");
		double maxHeight = 0;
		double maxDistanceFromCenter = 0;
		
		DoubleMatrix averagePosition = getAveragePosition();
		averagePosition.muli(new DoubleMatrix(new double[]{1,1,0}));
		for (Block b : blocks)
		{
			if (b.position.get(1) > maxHeight)
				maxHeight = b.position.get(1);
			
			DoubleMatrix groundPosition = b.position.mul(new DoubleMatrix(new double[]{1,1,0}));
			double distance = averagePosition.distance2(groundPosition);
			if (distance > maxDistanceFromCenter)
				maxDistanceFromCenter = distance;
		}
		widthFeature.setValue(maxDistanceFromCenter * 2);
		widthScaleFeature.setValue(maxDistanceFromCenter * 2);
		radiusFeature.setValue(maxDistanceFromCenter);
		diameterFeature.setValue(maxDistanceFromCenter * 2);
		heightFeature.setValue(maxHeight + Block.BLOCK_WIDTH / 2);
		
		setFeature(heightFeature);
		setFeature(widthFeature);
		setFeature(widthScaleFeature);
		setFeature(radiusFeature);
		setFeature(diameterFeature);
	}
	
	public DoubleMatrix getAveragePosition()
	{
		DoubleMatrix result = DoubleMatrix.zeros(3);
		
		for (Block b : blocks)
		{
			result.addi(b.position);
		}
		
		result.divi(blocks.size());
		
		return result;
	}
	
	public void generateTemporalSequenceFeature()
	{
		TemporalSequenceFeature sequenceFeature = new TemporalSequenceFeature("sequence");
		List<FeatureGroup> featureGroupSequence = new ArrayList<FeatureGroup>();
		
		for (Block b : blocks)
		{
			featureGroupSequence.add(new BlockFeatureGroup(b));
		}
		
		sequenceFeature.setSequence(featureGroupSequence);
		
		setFeature(sequenceFeature);
	}
	
	public Feature getFeature(String featureName)
	{
		return features.get(featureName);
	}
	
	public void setBlocks(Set<Block> newBlocks)
	{
		this.blocks = newBlocks;
	}

	@Override
	public Collection<Feature> getFeatures() {
		// TODO Auto-generated method stub
		return features.values();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	
}
