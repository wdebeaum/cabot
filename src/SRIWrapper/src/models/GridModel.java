package models;

import java.util.Collection;

import environment.*;

public class GridModel {

	private Block[][][] grid;
	int width, height, depth;
	
	public GridModel(int width, int height, int depth) {
		this.width = width;
		this.height = height;
		this.depth = depth;
		grid = new Block[width][height][depth];
	}
	
	public void addBlocks(Collection<Block> blocks)
	{
	}

	public void clearBlocks()
	{
		grid = new Block[width][height][depth];
	}
	
}
