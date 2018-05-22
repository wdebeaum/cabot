package models;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import org.jblas.DoubleMatrix;

import environment.*;

public class GridModel2D {

	private Block[][] grid;
	public static int DEFAULT_WIDTH = 3;
	public static int DEFAULT_HEIGHT = 3;
	private List<Block> blocks;
	int width, height;
	
	public GridModel2D(int width, int height) {
		this.width = width;
		this.height = height;
		grid = new Block[height][width];
		blocks = new LinkedList<Block>();
	}
	
	public GridModel2D() {
		this(DEFAULT_WIDTH,DEFAULT_HEIGHT);
	}
	
	private void addBlock(Block b, int x, int z)
	{
		grid[z][x] = b;
		blocks.add(b);
	}
	

	public void clearBlocks()
	{
		grid = new Block[height][width];
	}
	
	public DoubleMatrix getLocationAtGrid(int x, int z)
	{
		int centerX = width / 2;
		int cellXDifference = x - centerX;
		
		double xValue = -cellXDifference * Block.BLOCK_WIDTH;
		double zValue = z * Block.BLOCK_WIDTH + (Block.BLOCK_WIDTH / 2);
		
		return new DoubleMatrix(new double[] {xValue,0,zValue});
	}
	
	public List<Block> getBlocks()
	{
		return blocks;
	}
	
	public boolean dropBlockAt(int x)
	{
		for (int z = 0; z < height; z++)
		{
			if (grid[z][x] == null)
			{
				addBlock(new Block(getLocationAtGrid(x,z)),x,z);
				return true;
			}
		}
		return false;
	}
	
	public static GridModel2D randomSample(int numberOfBlocks)
	{
		GridModel2D toReturn = new GridModel2D();
		
		for (int i = 0; i < numberOfBlocks; i++)
		{
			Random random = new Random();
			int x = random.nextInt(toReturn.width);
			toReturn.dropBlockAt(x);
		}
		return toReturn;
	}
}
