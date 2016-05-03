package spatialreasoning;

import environment.Block;

public class Predicate {

	public static final double MAX_TOUCHING_DISTANCE = .16;
	public static final double MAX_NEAR_DISTANCE = .4;
	public static final double MAX_2D_NEAR_DISTANCE = .4;
	public static final double MAX_SAME_HEIGHT_DISTANCE = .05;
	public static final double HALF_SIDE_LENGTH = .09;
	
	
	PredicateType predicateType;
	
	public Predicate(PredicateType predicateType)
	{
		this.predicateType = predicateType;
	}
	
	public boolean evaluate(Block b)
	{
		switch (predicateType)
		{
		case ONGROUND:
			return b.onGround();
		case ABOVE:
		case BELOW:
		case NEXTTO:
		case TOUCHING:
		case ONTOPOF:
			return false;
		default:
			return false;
		}
	}
	
	public boolean evaluate(Block b1, Block b2)
	{
		switch (predicateType)
		{
		case ONGROUND:
			return b1.onGround() && b2.onGround();
		case ABOVE:
			return above(b1,b2);
		case BELOW:
			return below(b1,b2);
		case NEXTTO:
			return isNextTo(b1,b2);
		case TOUCHING:
			return isTouching(b1,b2);
		case ONTOPOF:
			return above(b1,b2) && isTouching(b1,b2);
		default:
			return false;
		}
	}
	
	private boolean isTouching(Block b1, Block b2)
	{
		return b1.position.distance2(b2.position) < MAX_TOUCHING_DISTANCE;
	}
	
	private boolean isNextTo(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < 
				MAX_2D_NEAR_DISTANCE && 
				Math.abs(b1.getZ() - b2.getZ()) < MAX_SAME_HEIGHT_DISTANCE;
	}
	
	private boolean above(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < HALF_SIDE_LENGTH &&
				b1.getZ() > b2.getZ(); 
	}
	
	private boolean below(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < HALF_SIDE_LENGTH &&
				b1.getZ() < b2.getZ(); 
	}
	
	public String getActionString()
	{
		switch (predicateType)
		{
		case ONGROUND:
			return " on the table.";
		case ABOVE:
			return " above ";
		case BELOW:
			return " below ";
		case NEXTTO:
			return " next to ";
		case TOUCHING:
			return " so that it touches ";
		case ONTOPOF:
			return " on top of ";
		default:
			return "";
		}
	}
}
