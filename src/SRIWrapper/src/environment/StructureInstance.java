package environment;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
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
		//for (Block b : blocks)
		//	System.out.println(b.position);
		generateFeatures();
	}
	
	public StructureInstance(StructureInstance toCopy)
	{
		this.blocks = new HashSet<Block>();
		for (Block b : toCopy.blocks)
			this.blocks.add(new Block(b));
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
		generateUnorderedGroupingFeature();
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
		
		double horizontalityValue = (ratio > 1) ? 0.0 : 1.0;
		double verticalityValue = (ratio > 1) ? 1.0 : 0.0;
		DecimalFeature horizontality = new DecimalFeature(FeatureConstants.HORIZONTAL);
		horizontality.setValue(horizontalityValue);
		horizontality.setPrettyName("horizontality");
		setFeature(horizontality);
		
		DecimalFeature verticality = new DecimalFeature(FeatureConstants.VERTICAL);
		horizontality.setValue(verticalityValue);
		horizontality.setPrettyName("verticality");
		setFeature(verticality);
		
	}
	
	private void generateBlockNumberFeature()
	{
		CountFeature countFeature = new CountFeature(FeatureConstants.NUMBER);
		countFeature.setValue(blocks.size());
		setFeature(countFeature);
		CountFeature quantityFeature = new CountFeature(FeatureConstants.QUANTITY);
		quantityFeature.setValue(blocks.size());
		setFeature(quantityFeature);
		features.put(FeatureConstants.SIZE, countFeature);
	}
	
	private void generateLinearityFeature()
	{
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
		
		List<Double> sortedLinearity = new ArrayList<Double>();
		sortedLinearity.add(xLinearity.getValue());
		sortedLinearity.add(yLinearity.getValue());
		sortedLinearity.add(zLinearity.getValue());
		Collections.sort(sortedLinearity);
		
		DecimalFeature linearity = new DecimalFeature(FeatureConstants.LINE);
		// Last value (largest) will be the principal axis, second value 
		// will be the largest deviation from the axis
		if (blocks.size() < 3)
			linearity.setValue(0.0);
		else
			linearity.setValue(sortedLinearity.get(1));
		linearity.setPrettyName("linearity");
		setFeature(linearity);
		
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
		DistanceFeature heightFeature = new DistanceFeature(FeatureConstants.HEIGHT);
		DistanceFeature widthScaleFeature = new DistanceFeature(FeatureConstants.WIDTH);
		DistanceFeature widthFeature = new DistanceFeature("ONT::WIDTH");
		DistanceFeature lengthFeature = new DistanceFeature(FeatureConstants.LENGTH);
		DistanceFeature linearExtentFeature = new DistanceFeature(FeatureConstants.LINEAREXTENT);
		DistanceFeature radiusFeature = new DistanceFeature("W::RADIUS");
		DistanceFeature diameterFeature = new DistanceFeature("W::DIAMETER");
		double maxHeight = 0;
		double maxDistanceFromCenter = 0;
		
		DoubleMatrix averagePosition = getAveragePosition();
		averagePosition.muli(new DoubleMatrix(new double[]{1,1,0}));
		for (Block b : blocks)
		{

			if (b.position.get(2) > maxHeight)
				maxHeight = b.position.get(2);
			
			if (b.getZ() < Block.MAX_GROUND_HEIGHT) {
				
				DoubleMatrix groundPosition = b.position.mul(new DoubleMatrix(new double[]{1,1,0}));
				
				double distance = averagePosition.distance2(groundPosition);
				if (distance > maxDistanceFromCenter)
					maxDistanceFromCenter = distance;
			}
		}
		//System.out.println("Widthscale before: " + widthScaleFeature.getValue());
		widthFeature.setValue(generateWidthFeature());
		widthScaleFeature.setValue(generateWidthFeature());
		lengthFeature.setValue(generateWidthFeature());
		linearExtentFeature.setValue(generateWidthFeature());
		radiusFeature.setValue(maxDistanceFromCenter / Block.BLOCK_WIDTH + .5);
		diameterFeature.setValue(maxDistanceFromCenter * 2 / Block.BLOCK_WIDTH + 1);
		heightFeature.setValue((maxHeight + Block.BLOCK_WIDTH / 2) / Block.BLOCK_WIDTH);
		//System.out.println("Widthscale after: " + widthScaleFeature.getValue());
		//System.out.println("Heightscale after: " + heightFeature.getValue());
		setFeature(heightFeature);
		setFeature(widthFeature);
		setFeature(widthScaleFeature);
		setFeature(lengthFeature);
		setFeature(linearExtentFeature);
		setFeature(radiusFeature);
		setFeature(diameterFeature);
	}
	

	
	private double generateWidthFeature()
	{
		double maxX = Double.MIN_VALUE;
		double minX = Double.MAX_VALUE;
		for (Block b : blocks)
		{

			
			if (b.getX() - Block.BLOCK_WIDTH / 2 < minX)
				minX = b.getX() - Block.BLOCK_WIDTH / 2;
			
			if (b.getX() + Block.BLOCK_WIDTH / 2 > maxX)
				maxX = b.getX() + Block.BLOCK_WIDTH / 2;
			
		}

		return (maxX - minX) / Block.BLOCK_WIDTH;
		
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
		TemporalSequenceFeature sequenceFeature = 
					new TemporalSequenceFeature(FeatureConstants.SEQUENCE);
		List<FeatureGroup> featureGroupSequence = new ArrayList<FeatureGroup>();
		
		for (Block b : blocks)
		{
			featureGroupSequence.add(new BlockFeatureGroup(b));
		}
		
		sequenceFeature.setSequence(featureGroupSequence);
		
		setFeature(sequenceFeature);
	}
	
	private void generateUnorderedGroupingFeature()
	{
		UnorderedGroupingFeature groupingFeature = 
				new UnorderedGroupingFeature(FeatureConstants.GROUPING);
		List<FeatureGroup> featureGroupSequence = new ArrayList<FeatureGroup>();
		
		for (Block b : blocks)
		{
			featureGroupSequence.add(new BlockFeatureGroup(b));
		}
	
		groupingFeature.setValue(featureGroupSequence);
	
		setFeature(groupingFeature);		
	}
	
	public Feature getFeature(String featureName)
	{
		return features.get(featureName);
	}
	
	public boolean hasFeature(String featureName)
	{
		return features.containsKey(featureName);
	}
	
	public void setBlocks(Collection<Block> newBlocks)
	{
		this.blocks.clear();
		this.blocks.addAll(newBlocks);
		generateFeatures();
	}

	@Override
	public Map<String,Feature> getFeatures() {
		// TODO Auto-generated method stub
		return features;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	
}
