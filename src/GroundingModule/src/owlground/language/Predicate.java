package owlground.language;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jblas.DoubleMatrix;

import owlground.WorldManager;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.perception.Blob;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;

public class Predicate {
	private String name;
	String[] variables;
	private boolean containsSetVariable;
	
	private static int OBJECT_MODEL_PROPERTY_K = 2;
	private static int OBJECT_MODEL_OBJECT_K = 2;
	private static int SIZE_DIFFERENCE_THRESHOLD = 25;
	
	public Predicate(String name) {
		this(name,new String[0]);
		
	}
	
	public Predicate(String name, List<String> variables) {
		this(name,variables.toArray(new String[0]));
		
	}
	
	public Predicate(String name, String[] variables) {
		this.name = name;
		this.variables = variables;
		
		this.containsSetVariable = false;
		for (String variable: variables)
		{
			if (variable.equals(variable.toUpperCase()))
			{
				containsSetVariable = true;
				break;
			}
		}
	}
	
	public double getDistance(Blob blob, List<Blob> subblobs)
	{
		double distance = Double.MAX_VALUE;
		//System.out.println("Getting distance for ." + name + ".");
		WorldManager wm = WorldManager.getInstance();
		if (wm.getProperties().containsKey(name))
		{
			//System.out.println(name + "Found in properties");
			Property p = wm.getProperties().get(name);
			//System.out.println("Subblob: " + subblobs.get(0));
			//System.out.println("Percept cluster: " + subblobs.get(0).getPerceptCluster());
			//System.out.println("Percept map: " + subblobs.get(0).getPerceptCluster());
			
			// If there are no arguments, check the whole object
			Blob blobToCheck;
			if (subblobs.size() == 0)
				blobToCheck = blob;
			else
				blobToCheck = subblobs.get(0);
			
			List<Double> distances = p.getKNearestMahalanobis(
					blobToCheck.getPerceptCluster().toPerceptMap(), OBJECT_MODEL_PROPERTY_K);
			double sum = 0.0;
			for (Double d : distances)
				sum += d;
			
			distance = sum / distances.size();
			
			//distance = p.getSquaredMahalanobisToMean(p.getRepresentativeFeature(), subblobs.get(0).getPerceptCluster().toPerceptMap().get(p.getRepresentativeFeature()));
			
		}
		else if (wm.getObjectClasses().containsKey(name))
		{
			//System.out.println("Found in objects");
			ObjectClass oc = wm.getObjectClasses().get(name);
			// If the subblob is null, your object model definitions are probably referencing
			// undefined variables
			Blob subblob = subblobs.get(0);
			PerceptCluster pc = subblob.getPerceptCluster();
			Map<FeatureSpace, DoubleMatrix> perceptMap = pc.toPerceptMap();
			
			List<Double> distances = oc.getKNearestMahalanobis(perceptMap, OBJECT_MODEL_OBJECT_K);
			double sum = 0.0;
			for (Double d : distances)
				sum += d;
			
			distance = sum / distances.size();
		}
		else	
		{
			if (subblobs.size() < 2)
			{
				//System.out.println("Not enough subblobs");
				return Double.MAX_VALUE;
			}
			
			Blob first = subblobs.get(0);
			Blob second = subblobs.get(1);
			
//			System.out.println("First X: " + first.getX());
//			System.out.println("First Y: " + first.getY());
//			System.out.println("First width: " + first.getWidth());
//			System.out.println("First height: " + first.getHeight());
//			System.out.println("Second X: " + second.getX());
//			System.out.println("Second Y: " + second.getY());
//			System.out.println("Second width: " + second.getWidth());
//			System.out.println("Second height: " + second.getHeight());
			
			if (name.equalsIgnoreCase("ABOVE"))
			{
				distance = aboveDistance(first, second);
			}
			else if (name.equalsIgnoreCase("NEXTTO"))
			{
				distance = nextToDistance(first, second);
			}
			else if (name.equalsIgnoreCase("WIDER"))
			{
				distance = widerDistance(first, second);
			}
		}

		//System.out.println("Distance for predicate " + predicate + ": " + distance);
		
		return distance;
	}

	public static double widerDistance(Blob first, Blob second)
	{
		double distance;
		
		if (first.getWidth() - SIZE_DIFFERENCE_THRESHOLD > second.getWidth())
			distance = 0.0;
		else
			distance = Double.MAX_VALUE;
		
		return distance;
	}
	
	public static double aboveDistance(Blob first, Blob second)
	{

		boolean xAligned = false;
		double distance;
		
		if ((first.getCenterX() > second.getX() && 
				first.getCenterX() < second.getX() + second.getWidth()))
			xAligned = true;
		
		if (second.getCenterX() > first.getX() && 
				second.getCenterX() < first.getX() + first.getWidth())
			xAligned = true;
		
		// Remember this is from top-left being 0, so it's reversed from what you'd expect
		if (xAligned && first.getCenterY() < second.getCenterY())
			distance = 0.0;
		else
			distance = Double.MAX_VALUE;
		
		//System.out.println("Above distance: " + distance);
		
		return distance;
	}
	
	public List<Predicate> expandSets(int numberOfVariablesToExpand)
	{
		List<Predicate> result = new ArrayList<Predicate>();

		if (containsSetVariable)
		{
			for (int instanceNumber = 0; instanceNumber < numberOfVariablesToExpand; instanceNumber++)
			{
				String[] newVariableList = new String[variables.length];
				
				for (int i = 0; i < newVariableList.length; i++)
				{
					String variable = variables[i];
					if (variable.equals(variable.toUpperCase()))
						newVariableList[i] = variable + instanceNumber;
					else
						newVariableList[i] = variable;
				}
				
				result.add(new Predicate(name,newVariableList));
			}
		}
		else
			result.add(this);

		
		return result;
	}
	
	public static double nextToDistance(Blob first, Blob second)
	{
		double distance;
		
		boolean yAligned = false;
		
		if ((first.getCenterY() > second.getY() && 
				first.getCenterY() < second.getY() + second.getHeight()))
			yAligned = true;
		
		if (second.getCenterY() > first.getY() && 
				second.getCenterY() < first.getY() + first.getHeight())
			yAligned = true;
		
		if (yAligned)
			distance = 0.0;
		else
			distance = Double.MAX_VALUE;
		
		//System.out.println("Next-to distance: " + distance);
		
		return distance;
	}
	
	public String[] getVariables()
	{
		return variables;
	}
	
	public String getName()
	{
		return name;
	}

	public boolean containsSetVariable() {
		return containsSetVariable;
	}
}
