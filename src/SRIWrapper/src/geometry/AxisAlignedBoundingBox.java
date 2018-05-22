package geometry;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.jblas.DoubleMatrix;

import environment.Block;
import features.BlockFeatureGroup;
import spatialreasoning.Predicate;

public class AxisAlignedBoundingBox {

	public double minX;
	public double maxX;
	public double minY;
	public double maxY;
	public double minZ;
	public double maxZ;
	
	public AxisAlignedBoundingBox() {
		minX = 0;
		maxX = 0;
		minY = 0;
		maxY = 0;
		minZ = 0;
		maxZ = 0;
	}
	
	public DoubleMatrix getCenter()
	{
		return new DoubleMatrix(new double[] {(maxX+minX)/2,
												(maxY+minY)/2,
												(maxZ+minZ)/2});
	}
	
	// This is not actually right, just an approximation
	public static AxisAlignedBoundingBox fromBlock(Block b)
	{
		AxisAlignedBoundingBox result = new AxisAlignedBoundingBox();
		DoubleMatrix center = b.position;
		result.minX = center.get(0) - Block.BLOCK_WIDTH / 2;
		result.maxX = center.get(0) + Block.BLOCK_WIDTH / 2;
		result.minY = center.get(1) - Block.BLOCK_WIDTH / 2;
		result.maxY = center.get(1) + Block.BLOCK_WIDTH / 2;
		result.minZ = center.get(2) - Block.BLOCK_WIDTH / 2;
		result.maxZ = center.get(2) + Block.BLOCK_WIDTH / 2;
		
		return result;
		
	}
	
	public static AxisAlignedBoundingBox fromBlockFeatureGroups(List<BlockFeatureGroup> bfgs)
	{
		List<Block> blocks = new ArrayList<Block>();
		
		for (BlockFeatureGroup bfg : bfgs)
		{
			blocks.add(bfg.block);
		}
		
		return AxisAlignedBoundingBox.fromBlocks(blocks);
	}
	
	public boolean containsX(AxisAlignedBoundingBox other)
	{
		return (other.minX + Predicate.HALF_SIDE_LENGTH > minX && 
				other.maxX < maxX + Predicate.HALF_SIDE_LENGTH);
	}
	
	public boolean containsY(AxisAlignedBoundingBox other)
	{
		return (other.minY + Predicate.HALF_SIDE_LENGTH > minY && 
				other.maxY < maxY + Predicate.HALF_SIDE_LENGTH);
	}
	
	public boolean containsZ(AxisAlignedBoundingBox other)
	{
		return (other.minZ + Predicate.HALF_SIDE_LENGTH > minZ && 
				other.maxZ < maxZ + Predicate.HALF_SIDE_LENGTH);
	}
	
	public boolean intersectsX(AxisAlignedBoundingBox other)
	{
		return (other.maxX > minX && other.minX < maxX);
	}
	
	public boolean intersectsY(AxisAlignedBoundingBox other)
	{
		return (other.maxY > minY && other.minY < maxY);
	}
	
	public boolean intersectsZ(AxisAlignedBoundingBox other)
	{
		return (other.maxZ > minZ && other.minZ < maxZ);
	}
	
	public static AxisAlignedBoundingBox fromBlocks(List<Block> blocks)
	{
		if (blocks.isEmpty())
			return null;
		
		List<AxisAlignedBoundingBox> boundingBoxes = new ArrayList<AxisAlignedBoundingBox>();
		List<Double> xValues = new ArrayList<Double>();
		List<Double> yValues = new ArrayList<Double>();
		List<Double> zValues = new ArrayList<Double>();
		for (Block b : blocks)
		{
			AxisAlignedBoundingBox temp = fromBlock(b);
			boundingBoxes.add(temp);
			xValues.add(temp.minX);
			xValues.add(temp.maxX);
			yValues.add(temp.minY);
			yValues.add(temp.maxY);
			zValues.add(temp.minZ);
			zValues.add(temp.maxZ);
		}
		
		AxisAlignedBoundingBox result = new AxisAlignedBoundingBox();
		result.minX = Collections.min(xValues);
		result.maxX = Collections.max(xValues);
		result.minY = Collections.min(yValues);
		result.maxY = Collections.max(yValues);
		result.minZ = Collections.min(zValues);
		result.maxZ = Collections.max(zValues);
		
		return result;
		
	}

}
