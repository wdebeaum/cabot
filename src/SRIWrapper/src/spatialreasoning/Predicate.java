package spatialreasoning;

import environment.Block;
import environment.Scene;
import environment.StructureInstance;
import features.BlockFeatureGroup;
import features.FeatureConstants;
import features.UnorderedGroupingFeature;
import geometry.AxisAlignedBoundingBox;
import utilities.KQMLUtilities;

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
		UnorderedGroupingFeature sceneComplement = (UnorderedGroupingFeature)Scene.currentScene
									.getComplementStructureInstance(b)
									.getFeature(FeatureConstants.GROUPING);

		UnorderedGroupingFeature blockUGF = new UnorderedGroupingFeature("block");
		blockUGF.add(new BlockFeatureGroup(b));
		switch (predicateType)
		{
		case ONGROUND:
		case BOTTOM:
			return b.onGround();
		case ABOVE:
		case BELOW:
		case NEXTTO:
		case TOUCHING:
		case ONTOPOF:
			return false;
		case TOP:
			return higher(blockUGF, sceneComplement);
		case LEFT:
		case LEFTLOC:
			return left(blockUGF, sceneComplement);
		case RIGHT:
		case RIGHTLOC:
			return right(blockUGF, sceneComplement);
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
		case ATSAMEHEIGHT:
			return atSameHeight(b1,b2);
		case ATSAMEY:
			return atSameY(b1,b2);
		default:
			return false;
		}
	}
	
	public boolean evaluate(UnorderedGroupingFeature s)
	{
		StructureInstance si = s.getStructureInstance();
		UnorderedGroupingFeature sceneComplement = (UnorderedGroupingFeature)Scene.currentScene
									.getComplementStructureInstance(si)
									.getFeature(FeatureConstants.GROUPING);
		
		System.out.println("Blocks in group: ");
		for (Block b : s.getBlocks())
			System.out.println("Block " + b.getId());
		System.out.println("Blocks in complement: ");
		for (Block b : sceneComplement.getBlocks())
			System.out.println("Block " + b.getId());
		switch (predicateType)
		{
		case ONGROUND:
		case BOTTOM:
			return onGround(s);
		case ABOVE:
		case BELOW:
		case NEXTTO:
		case TOUCHING:
		case ONTOPOF:
			return false;
		case TOP:
			return higher(s, sceneComplement);
		case LEFT:
		case LEFTLOC:
			return left(s, sceneComplement);
		case RIGHT:
		case RIGHTLOC:
			return right(s, sceneComplement);
		default:
			return false;
		}		
	}
	
	public boolean evaluate(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		switch (predicateType)
		{
		case ONGROUND:
			return onGround(s1) && onGround(s2);
		case ABOVE:
			return above(s1,s2);
		case BELOW:
			return below(s1,s2);
		case NEXTTO:
			return isNextTo(s1,s2);
		case TOUCHING:
			return isTouching(s1,s2);
		case ONTOPOF:
			return above(s1,s2) && isTouching(s1,s2);
		case ATSAMEY:
			return atSameY(s1,s2);
		case LOWER:
			return lower(s1,s2);
		case HIGHER:
			return higher(s1,s2);
		default:
			return false;
		}
	}
	
	public static boolean atSameY(Block b1, Block b2)
	{
		return Math.abs(b1.getY() - b2.getY()) < MAX_SAME_HEIGHT_DISTANCE;
	}
	
	public static boolean atSameY(UnorderedGroupingFeature s1, 
									UnorderedGroupingFeature s2)
	{
		for (BlockFeatureGroup b1 : s1.getBlockFeatureGroups())
		{
			for (BlockFeatureGroup b2 : s2.getBlockFeatureGroups())
			{
				if (!atSameY(b1.block,b2.block))
					return false;
			}
		}
		return true;
	}
	
	public static boolean atSameHeight(Block b1, Block b2)
	{
		return Math.abs(b1.getZ() - b2.getZ()) < MAX_SAME_HEIGHT_DISTANCE;
		
	}
	private static boolean isTouching(Block b1, Block b2)
	{
		return b1.position.distance2(b2.position) < MAX_TOUCHING_DISTANCE;
	}
	
	private static boolean isNextTo(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < 
				MAX_2D_NEAR_DISTANCE && 
				atSameHeight(b1,b2);
	}
	
	private static boolean isNextTo(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		for (BlockFeatureGroup b1 : s1.getBlockFeatureGroups())
		{
			for (BlockFeatureGroup b2 : s2.getBlockFeatureGroups())
			{
				if (isNextTo(b1.block,b2.block))
					return true;
			}
		}
		return false;
	}
	
	private static boolean isTouching(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		for (BlockFeatureGroup b1 : s1.getBlockFeatureGroups())
		{
			for (BlockFeatureGroup b2 : s2.getBlockFeatureGroups())
			{
				if (isTouching(b1.block,b2.block))
					return true;
			}
		}
		return false;
	}
	
	public static boolean above(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < HALF_SIDE_LENGTH &&
				b1.getZ() > b2.getZ(); 
	}
	
	public static boolean above(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		return (aabb1.intersectsX(aabb2) && 
				aabb1.intersectsY(aabb2) && 
				aabb1.minZ + HALF_SIDE_LENGTH > aabb2.maxZ);
	}
	
	public static boolean higher(Block b1, Block b2)
	{
		return b1.getZ() > b2.getZ();
	}
	
	public static boolean lower(Block b1, Block b2)
	{
		return b1.getZ() < b2.getZ();
	}
	
	public static boolean higher(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		return aabb1.minZ + HALF_SIDE_LENGTH > aabb2.maxZ;
	}
	
	public static boolean left(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		System.out.println("Group 1 center: " + s1.getCenterFeature().getValue());
		System.out.println("Group 2 center: " + s2.getCenterFeature().getValue());
		return s1.getCenterFeature().getValue().get(0) < 
				s2.getCenterFeature().getValue().get(0);
	}
	
	public static boolean right(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		return s1.getCenterFeature().getValue().get(0) > 
				s2.getCenterFeature().getValue().get(0);
	}
	
	public static boolean lower(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		return (aabb1.intersectsX(aabb2) && 
				aabb1.intersectsY(aabb2) && 
				aabb1.maxZ - HALF_SIDE_LENGTH < aabb2.minZ);
	}
	
	public static boolean below(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		return (aabb1.intersectsX(aabb2) && 
				aabb1.intersectsY(aabb2) && 
				aabb1.maxZ - HALF_SIDE_LENGTH < aabb2.minZ);
	}
	
	public static boolean below(Block b1, Block b2)
	{
		return b1.getXY().distance2(b2.getXY()) < HALF_SIDE_LENGTH &&
				b1.getZ() < b2.getZ(); 
	}
	
	private static boolean onGround(UnorderedGroupingFeature structure)
	{
		for (BlockFeatureGroup bfg : structure.getBlockFeatureGroups())
		{
			if (bfg.block.onGround())
				return true;
		}
		return false;
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
	
	public String toString()
	{
		return predicateType.toString();
	}

}
