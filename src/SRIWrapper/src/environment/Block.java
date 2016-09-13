package environment;

import org.jblas.DoubleMatrix;
import org.json.simple.JSONObject;

import utilities.FormatConversion;
import TRIPS.KQML.KQMLList;



public class Block {

	public final static double MAX_GROUND_HEIGHT = .09;
	public final static double BLOCK_WIDTH = 0.171;
	int id;
	public DoubleMatrix position;
	public DoubleMatrix rotation;
	double confidence;
	public String label;
	private static final double POSITION_EPSILON = .01;
	private static final double ROTATION_EPSILON = .01;
	private static final double DISTANCE_EPSILON = .13;
	private boolean proxyInstructed; // Did the proxy say to put this here?
	private boolean userOwned;
	private boolean moved;
	
	public Block(JSONObject jsonState)
	{
		id = ((Long)jsonState.get("ID")).intValue();
		confidence = (double)jsonState.get("Confidence");
		String positionString = (String)jsonState.get("Position");
		String[] positionElements = positionString.split(",");
		double[] positionElementDoubles = new double[3];
		for(int i = 0; i < 3; i++) 
			positionElementDoubles[i] = Double.parseDouble(positionElements[i]); 
		position = new DoubleMatrix(positionElementDoubles);
		
		String rotationString = (String)jsonState.get("Rotation");
		String[] rotationElements = rotationString.split(",");
		double[] rotationElementDoubles = new double[4];
		for(int i = 0; i < 4; i++) 
			rotationElementDoubles[i] = Double.parseDouble(rotationElements[i]); 
		rotation = new DoubleMatrix(rotationElementDoubles);
		proxyInstructed = false;
		userOwned = false;
	}
	
	public Block(String positionString)
	{
		id = 1;
		confidence = 1;
		String[] positionElements = positionString.split(",");
		double[] positionElementDoubles = new double[3];
		for(int i = 0; i < 3; i++) 
			positionElementDoubles[i] = Double.parseDouble(positionElements[i]); 
		position = new DoubleMatrix(positionElementDoubles);
		
		
		rotation = DoubleMatrix.zeros(4);
		rotation.put(3,1);
		proxyInstructed = false;
		userOwned = false;
	}
	
	public Block(DoubleMatrix position)
	{
		id = 1;
		confidence = 1;	
		this.position = position;
		
		
		rotation = DoubleMatrix.zeros(4);
		rotation.put(3,1);
		proxyInstructed = false;
		userOwned = false;
	}
	
	public boolean onGround()
	{
		return position.get(2) < MAX_GROUND_HEIGHT;
	}
	
	public double getX()
	{
		return position.get(0);
	}
	
	public double getY()
	{
		return position.get(1);
	}
	
	// Z is pointing UP away from the table
	public double getZ()
	{
		return position.get(2);
	}
	
	public DoubleMatrix getXY()
	{
		return position.get(new int[]{0,1});
	}
	
	public boolean isSimilarTo(Block b)
	{
		for (int i = 0; i < 3; i++)
		{
			if (Math.abs(b.position.get(i) - position.get(i)) > POSITION_EPSILON)
				return false;
		}
		
		for (int i = 0; i < 4; i++)
		{
			if (Math.abs(b.rotation.get(i) - rotation.get(i)) > ROTATION_EPSILON)
				return false;
		}
		
		return true;
			
	}
	
	public boolean hasSimilarPosition(Block b)
	{
		return position.distance2(b.position) < DISTANCE_EPSILON;
	}
	
	public KQMLList getKQMLRepresentation()
	{
		KQMLList blockContent = new KQMLList();
		KQMLList classContent = new KQMLList();
		KQMLList shapeParams = new KQMLList();
		
		classContent.add(":*");
		classContent.add("ONT::REFERENTIAL-SEM");
		classContent.add("W::BLOCK");
		
		shapeParams.add("SHAPE-PARAMS");
		shapeParams.add(":SIDE-LENGTH");
		shapeParams.add(".1524");
		shapeParams.add(":FACES");
		shapeParams.add(new KQMLList());
		shapeParams.add(":ORIENTATION");
		shapeParams.add(new KQMLList());
		
		blockContent.add("OBJECT");
		blockContent.add("ONT::V0000");
		blockContent.add(":CLASS");
		blockContent.add(classContent);
		
		blockContent.add(":PROPERTIES");
		blockContent.add(new KQMLList());
		
		blockContent.add(":NAME");
		blockContent.add("\"" + label + "\"");
		
		blockContent.add(":SHAPE");
		blockContent.add(shapeParams);
		
		blockContent.add(":EXAPHOR");
		blockContent.add("ONT::V0000");
		
		blockContent.add(":POSITION");
		blockContent.add(FormatConversion.doubleMatrixToKQMLList(position));
		
		blockContent.add(":ROTATION");
		blockContent.add(FormatConversion.doubleMatrixToKQMLList(rotation));
		
		blockContent.add(":CONFIDENCE");
		blockContent.add("" + confidence);
		
		blockContent.add(":VISUAL-ID");
		blockContent.add("\"dummy1234\"");
		
		blockContent.add(":TOPLEFT");
		KQMLList topLeft = new KQMLList();
		topLeft.add("0");
		topLeft.add("0");
		blockContent.add(topLeft);
		
		blockContent.add(":BOTTOMRIGHT");
		KQMLList bottomRight = new KQMLList();
		bottomRight.add("0");
		bottomRight.add("0");
		blockContent.add(bottomRight);
		
		return blockContent;
	}
	
	public String getJSONRepresentation()
	{
		JSONObject jsonBlock = new JSONObject();
		DoubleMatrix yInvertPosition = position.mul(new DoubleMatrix(new double[]{1,-1,1}));
		jsonBlock.put("id", "" + id);
		jsonBlock.put("position", FormatConversion.doubleMatrixToJSONString(yInvertPosition));
		jsonBlock.put("rotation", FormatConversion.doubleMatrixToJSONString(rotation));
		jsonBlock.put("confidence", "" + confidence);
		
		
		return jsonBlock.toString();
	}

	public boolean isProxyInstructed() {
		return proxyInstructed;
	}

	public void setProxyInstructed(boolean proxyInstructed) {
		this.proxyInstructed = proxyInstructed;
	}

	public boolean isUserOwned() {
		return userOwned;
	}

	public void setUserOwned(boolean userOwned) {
		this.userOwned = userOwned;
	}

	public boolean isMoved() {
		return moved;
	}

	public void setMoved(boolean moved) {
		this.moved = moved;
	}
	
	
}


