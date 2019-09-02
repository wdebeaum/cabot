package spatialreasoning;

import TRIPS.KQML.KQMLList;
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
	public static final double HALF_SIDE_LENGTH = .07;
	
	
	
	private PredicateType predicateType;
	
	public Predicate(PredicateType predicateType)
	{
		this.predicateType = predicateType;
	}
	
	public boolean evaluate(Block b)
	{
		return evaluate(b,Scene.currentScene);
	}
	
	public boolean evaluate(Block b, Scene scene)
	{
		UnorderedGroupingFeature sceneComplement = (UnorderedGroupingFeature)scene
									.getComplementStructureInstance(b)
									.getFeature(FeatureConstants.GROUPING);

		UnorderedGroupingFeature blockUGF = new UnorderedGroupingFeature("block");
		blockUGF.add(new BlockFeatureGroup(b));
		switch (predicateType)
		{
		case ANYWHERE:
			return true;
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
			return top(blockUGF, sceneComplement);
		case LEFT:
		case LEFTLOC:
			return left(blockUGF, sceneComplement);
		case LEFTMOST:
			return left(blockUGF, sceneComplement) && !middle(blockUGF, sceneComplement); 
		case RIGHT:
		case RIGHTLOC:
		case CORRECT: // Hack for parser
			return right(blockUGF, sceneComplement);
		case RIGHTMOST:
			return right(blockUGF, sceneComplement) && !middle(blockUGF, sceneComplement); 
		case SIDE:
		case SIDELOC:
		case SIDEPRED:
		case OUTER:
			return (right(blockUGF, sceneComplement) || left(blockUGF, sceneComplement)) && !middle(blockUGF, sceneComplement);
		case MIDDLE:
		case CENTER:
		case BETWEEN:
		case INNER:
			return middle(blockUGF, sceneComplement);
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
		case ONTOPOF:
			return above(b1,b2) && isTouching(b1,b2);
		case ATSAMEHEIGHT:
			return atSameHeight(b1,b2);
		case ATSAMEY:
			return atSameY(b1,b2);
		case ANYWHERE:
			return true;
		case TOGETHER:
		case TOUCHING:
		case ATTACHED:
			return isTouching(b1,b2);
		default:
			return false;
		}
	}
	
	public boolean evaluate(UnorderedGroupingFeature s)
	{
		return evaluate(s,Scene.currentScene);
	}
	
	public boolean evaluate(UnorderedGroupingFeature s, Scene scene)
	{
		StructureInstance si = s.getStructureInstance();
		UnorderedGroupingFeature sceneComplement = (UnorderedGroupingFeature)scene
									.getComplementStructureInstance(si)
									.getFeature(FeatureConstants.GROUPING);
		
//		System.out.println("Blocks in group: ");
//		for (Block b : s.getBlocks())
//			System.out.println("Block " + b.getId());
//		System.out.println("Blocks in complement: ");
//		for (Block b : sceneComplement.getBlocks())
//			System.out.println("Block " + b.getId());
		switch (predicateType)
		{
		case ONGROUND:
		case BOTTOM:
			return onGround(s);
		case ABOVE:
		case BELOW:
		case NEXTTO:
		case ONTOPOF:
			return above(s, sceneComplement) && isTouching(s, sceneComplement);
		case TOP:
			return top(s, sceneComplement);
		case LEFT:
		case LEFTLOC:
			return left(s, sceneComplement);
		case LEFTMOST:
			return left(s, sceneComplement) || !middle(s, sceneComplement);			
		case RIGHT:
		case RIGHTLOC:
		case CORRECT:
			return right(s, sceneComplement);
		case RIGHTMOST:
			return right(s, sceneComplement) || !middle(s, sceneComplement);
		case ANYWHERE:
			return true;
		case TOGETHER:
		case FILLED:
		case TOUCHING:
		case ATTACHED:
			return isTogether(s);
		case SIDE:
		case SIDELOC:
		case SIDEPRED:
		case OUTER:
			return (left(s, sceneComplement) || right(s, sceneComplement)) && !middle(s, sceneComplement);
		case MIDDLE:
		case CENTER:
		case BETWEEN:
		case INNER:
			return middle(s,sceneComplement);
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
		case ANYWHERE:
			return true;
		case MIDDLE:
		case BETWEEN:
		case INNER:
			return middle(s1,s2);
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
	
	public static boolean together(Block b1, Block b2)
	{
		return isTouching(b1,b2); 
	}
	
	public static boolean isTogether(UnorderedGroupingFeature s1)
	{
		if (s1.getBlocks().size() == 1)
			return true;
		for (BlockFeatureGroup b1 : s1.getBlockFeatureGroups())
		{
			int neighbors = 0;
			for (BlockFeatureGroup b2 : s1.getBlockFeatureGroups())
			{
				if (b1 == b2)
					continue;
				if (isTouching(b1.block,b2.block))
					neighbors++;
			}
			if (neighbors == 0)
				return false;
		}
		return true;
	}
	
	public static boolean middle(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		int verticalMiddles = 0;
		int horizontalMiddles = 0;
		for (BlockFeatureGroup b1 : s1.getBlockFeatureGroups())
		{
			boolean blockOnLeft = false;
			boolean blockOnRight = false;
			boolean blockBelow = false;
			boolean blockAbove = false;
			for (BlockFeatureGroup b2 : s2.getBlockFeatureGroups())
			{
				
				if (leftOf(b1.block,b2.block))
					blockOnLeft = true;
				if (rightOf(b1.block,b2.block))
					blockOnRight = true;
				if (higher(b1.block,b2.block))
					blockBelow = true;
				if (lower(b1.block,b2.block))
					blockAbove = true;
			}
			if (blockOnLeft && blockOnRight)
				horizontalMiddles++;
			if (blockBelow && blockAbove)
				verticalMiddles++;
		}
		
		return ((horizontalMiddles == s1.getBlocks().size()) || 
				(verticalMiddles == s1.getBlocks().size()));
	}
	
	public static boolean above(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
		return (aabb1.intersectsX(aabb2) && 
				aabb1.intersectsY(aabb2) && 
				aabb1.minZ + HALF_SIDE_LENGTH > aabb2.maxZ);
	}
	
	public static boolean higher(Block b1, Block b2)
	{
		return b1.getZ() > b2.getZ() + HALF_SIDE_LENGTH;
	}
	
	public static boolean lower(Block b1, Block b2)
	{
		return b1.getZ() < b2.getZ() - HALF_SIDE_LENGTH;
	}
	
	public static boolean higher(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
		return aabb1.minZ + HALF_SIDE_LENGTH > aabb2.maxZ;
	}
	
	public static boolean left(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
		return aabb1.getCenter().get(0) < aabb2.getCenter().get(0) - HALF_SIDE_LENGTH;
	}
	
	public static boolean right(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
		return aabb1.getCenter().get(0) > aabb2.getCenter().get(0) + HALF_SIDE_LENGTH;
	}
	
	public static boolean leftOf(Block b1, Block b2)
	{
		return b1.position.get(0) < b2.position.get(0) - HALF_SIDE_LENGTH;
	}
	
	public static boolean rightOf(Block b1, Block b2)
	{
		return b1.position.get(0) > b2.position.get(0) + HALF_SIDE_LENGTH;
	}
	
	public static boolean top(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		for (Block b: s2.getBlocks())
		{
			UnorderedGroupingFeature blockUGF = new UnorderedGroupingFeature("block");
			blockUGF.add(new BlockFeatureGroup(b));
			if (higher(blockUGF,s1))
				return false;
		}
		
		return true;
	}
	
	public static boolean lower(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
		return (aabb1.intersectsX(aabb2) && 
				aabb1.intersectsY(aabb2) && 
				aabb1.maxZ - HALF_SIDE_LENGTH < aabb2.minZ);
	}
	
	public static boolean below(UnorderedGroupingFeature s1, UnorderedGroupingFeature s2)
	{
		AxisAlignedBoundingBox aabb1 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s1.getBlockFeatureGroups());
		AxisAlignedBoundingBox aabb2 = AxisAlignedBoundingBox.fromBlockFeatureGroups(s2.getBlockFeatureGroups());
		
		if (aabb1 == null || aabb2 == null)
			return false;
		
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
	
	public PredicateType getPredicateType()
	{
		return predicateType;
	}
	
	public String toString()
	{
		return predicateType.toString();
	}
	
	public static Predicate predicateFromTerm(KQMLList term)
	{
		String ontType = "";
		String lex = "*";
		if (term.getKeywordArg(":INSTANCE-OF") != null)
			ontType = term.getKeywordArg(":INSTANCE-OF").stringValue();
		if (term.getKeywordArg(":LEX") != null)
			lex = term.getKeywordArg(":LEX").stringValue();
		
		PredicateType result = PredicateType.fromString(ontType, lex);
		
		if (result == null)
			return null;
		
		return new Predicate(result);
	}
	
	public String prettyString()
	{
		switch (predicateType)
		{
		case ONGROUND:
			return "on the table";
		case ABOVE:
			return "above";
		case BELOW:
			return "below";
		case NEXTTO:
			return "next to";
		case TOUCHING:
			return "touching";
		case ONTOPOF:
			return "on top of";
		case LEFTLOC:
			return "on the left";
		case RIGHTLOC:
			return "on the right";
		case TOP:
			return "on top";
		case FILLED:
			return "filled";
		default:
			return KQMLUtilities.cleanOnt(predicateType.name());
		}
	}

}
