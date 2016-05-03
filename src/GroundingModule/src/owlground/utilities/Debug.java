package owlground.utilities;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Map;

import owlground.WorldManager;
import owlground.language.Word;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.spaces.FeatureSpace;

public class Debug {
	public static void debugWords(String filename, WorldManager wm)
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(filename);
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for Word debug.");
			e.printStackTrace();
			return;
		}
		
		for (Word w : wm.getWords().values())
		{
			outputFile.println(w);
		}
		outputFile.close();
	}
	
	public static void debugObjectClasses(String filename, WorldManager wm)
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(filename);
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for ObjectClass debug.");
			e.printStackTrace();
			return;
		}
		
		Map<FeatureSpace,Double> scaledSumOfSquares = wm.getScaledSumOfSquares();
		outputFile.println("Average variance:");
		for (FeatureSpace featureSpace : scaledSumOfSquares.keySet())
		{
			outputFile.println(featureSpace.getName() + ": " + scaledSumOfSquares.get(featureSpace));
		}
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			outputFile.println(oc);
		}
		outputFile.println("Number of object classes: " + wm.getObjectClasses().size());
		outputFile.close();
	}
	
	public static void debugProperties(String filename, WorldManager wm)
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(filename);
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for Property debug.");
			e.printStackTrace();
			return;
		}
		
		Map<FeatureSpace,Double> scaledSumOfSquares = wm.getScaledSumOfSquares();
		outputFile.println("Average variance:");
		for (FeatureSpace featureSpace : scaledSumOfSquares.keySet())
		{
			outputFile.println(featureSpace.getName() + ": " + scaledSumOfSquares.get(featureSpace));
		}
		for (Property p : wm.getProperties().values())
		{
			outputFile.println(p);
		}
		outputFile.println("Number of properties: " + wm.getProperties().size());
		outputFile.close();
	}
}
