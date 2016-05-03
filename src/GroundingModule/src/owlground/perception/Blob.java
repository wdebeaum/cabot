package owlground.perception;

import java.util.ArrayList;
import java.util.List;

import javax.xml.stream.XMLStreamReader;

import org.jblas.DoubleMatrix;

import owlground.spaces.EuclideanSpace;

/**
 * A blob of pixels.
 */
public class Blob {
	private PerceptCluster perceptCluster;
	/** id lets us identify if this is "the same" blob as one in an adjacent frame. */
	private int x, y, width, height;
	private int id;
	private double rightHandDistance;
	private double leftHandDistance;
	private List<Blob> subBlobs;
	
	final static double NEW_BLOB_SPEED = 100.0f;
	final static int VIDEO_WIDTH = 640;
	final static int VIDEO_HEIGHT = 480;

	public Blob(PerceptCluster pc, int x, int y, int w, int h, double rhd, double lhd)
	{
		perceptCluster = pc;
		this.x = x; this.y = y; this.width = w; this.height = h;
		double ratio = (double)(Math.max(height, width)) / Math.min(height, width);
		
		// Add the shape ratio feature
		perceptCluster.getSession().getSpaceManager().addFeatureSpace(new EuclideanSpace("shaperatio",1));
		perceptCluster.addPercept(new Percept("shaperatio",DoubleMatrix.ones(1).mul(ratio),perceptCluster));
		rightHandDistance = rhd;
		leftHandDistance = lhd;
		id = -1;
		subBlobs = new ArrayList<Blob>();
	}

	/**
	 * Read a &lt;blob&gt; start tag from an XMLStreamReader and return a new
	 * Blob object. Does not read the children of the blob element, or the end
	 * tag.
	 */
	public static Blob fromXML(XMLStreamReader r, PerceptCluster pc)
	{
		double rightHandDistance = 100;
		double leftHandDistance = 100;
		try {
			String rightHandDistString = r.getAttributeValue(null,"right_hand_dist");
			String leftHandDistString = r.getAttributeValue(null,"left_hand_dist");
			if (rightHandDistString != null)
				rightHandDistance = Double.parseDouble(rightHandDistString);
			if (leftHandDistString != null)
				leftHandDistance = Double.parseDouble(leftHandDistString);
		}
		catch (NumberFormatException ne)
		{
			
		}
		return new Blob(
			pc,
			Integer.parseInt(r.getAttributeValue(null,"x")),
			Integer.parseInt(r.getAttributeValue(null,"y")),
			Integer.parseInt(r.getAttributeValue(null,"width")),
			Integer.parseInt(r.getAttributeValue(null,"height")),
			rightHandDistance,
			leftHandDistance
		);
	}

	public double getMinHandDistance()
	{
		return (rightHandDistance < leftHandDistance ? rightHandDistance : leftHandDistance);
	}
	
	public void addSubBlob(Blob b)
	{
		subBlobs.add(b);
	}
	
	public double getDistanceFrom(Blob other)
	{
		// get center coords of this and other
		double tx,ty,ox,oy;
		tx = x + width / 2.0f;
		ty = y + height / 2.0f;
		ox = other.getX() + other.getWidth()/2.0f;
		oy = other.getY() + other.getHeight()/2.0f;
		// get differences
		double dx = tx - ox;
		double dy = ty - oy;
		
		// euclidean distance
		return (double)Math.sqrt(dx*dx + dy*dy);
	}
	
	public String getWorldLocationWord()
	{
		if (x > (VIDEO_WIDTH * (2.0/3.0)))
			return "right";
		if (x < (VIDEO_WIDTH * (1.0/3.0)))
			return "left";
		return "center";
	}
	

	/**
	 * Get the distance the given blob moved since the last frame, or
	 * NEW_BLOB_SPEED if the blob wasn't there last frame.
	 */
	public double getBlobSpeed(Blob prev)
	{
		return (prev == null ? NEW_BLOB_SPEED : getDistanceFrom(prev));
	}

	// accessors
	
	public PerceptCluster getPerceptCluster() {
		return perceptCluster;
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}
	
	public int getCenterX()
	{
		return x + width / 2;
	}
	
	public int getCenterY()
	{
		return y + height / 2;
	}

	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}


	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public double getRightHandDistance() {
		return rightHandDistance;
	}

	public double getLeftHandDistance() {
		return leftHandDistance;
	}

	public List<Blob> getSubBlobs() {
		return subBlobs;
	}

}
