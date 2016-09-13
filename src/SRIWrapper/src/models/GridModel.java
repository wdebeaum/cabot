package models;

import environment.*;

public class GridModel {

	private Block[][][] grid;
	
	public GridModel(int width, int height, int depth) {
		grid = new Block[width][height][depth];
	}

}
