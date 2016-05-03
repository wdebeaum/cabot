package owlground.geometry;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.HashSet;
import java.util.Map.Entry;
import java.util.HashMap;
import java.util.Set;

import org.jblas.DoubleMatrix;

import owlground.WorldManager;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;

public class VidaExpertFileCreator {

	public static void createDatFiles(WorldManager wm)
	{
		for (FeatureSpace fs : wm.getSpaceManager().getFeatureSpaces().values())
			createPropertyDatFile(fs, wm);
		
		createObjectDatFile("zernike", wm);
		//createObjectDatFile("normalhistogram", wm);
			
	}
	
	public static void createObjectDatFile(String featureSpaceName, WorldManager wm)
	{
		createObjectDatFile(wm.getSpaceManager().getFeatureSpaceByName(featureSpaceName), wm);
	}
	
	public static void createObjectDatFile(FeatureSpace fs, WorldManager wm)
	{
		String filename = fs.getName() + "-object.dat";
		PrintWriter pw = null;
		try {
			pw = new PrintWriter(filename, "UTF-8");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		HashMap<ObjectClass, Set<DoubleMatrix>> dataPoints = new HashMap<ObjectClass, Set<DoubleMatrix>>();
		
		int totalPoints = 0;
		
		for (ObjectClass oc : wm.getObjectClasses().values())
		{
			dataPoints.put(oc, new HashSet<DoubleMatrix>());
			for (PerceptCluster pc : oc.getPerceptClusters().values())
			{
				if (!pc.getPercepts().containsKey(fs))
				{
					System.out.println("Missing feature data for featurespace: " +
										fs.getName());
					return;
				}
				DoubleMatrix value = pc.getPercepts().get(fs).getValue();
				dataPoints.get(oc).add(value);
				totalPoints++;
			}
		}
		
		pw.print((fs.getDimension() + 1) + " " + totalPoints + "\r\n"); //+ 1 for label
		
		pw.print("label" + " STRING" + "\r\n");
		for (int i = 0; i < fs.getDimension(); i++)
		{
			pw.print("V" + i + " FLOAT" + "\r\n");
		}
		
		for (Entry<ObjectClass,Set<DoubleMatrix>> entry : dataPoints.entrySet())
		{
			
			for (DoubleMatrix dm : entry.getValue())
			{
				pw.print("\"" + entry.getKey().getName() + "\"");
				
				for (double d : dm.data)
				{
					pw.print(" " + (float)d);
				}
				pw.print("\r\n");
			}
		}
		pw.close();
	}
	
	public static void createPropertyDatFile(FeatureSpace fs, WorldManager wm)
	{
		
		String filename = fs.getName() + ".dat";
		PrintWriter pw = null;
		
		try {
			pw = new PrintWriter(filename, "UTF-8");
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		HashMap<Property, Set<DoubleMatrix>> dataPoints = new HashMap<Property, Set<DoubleMatrix>>();
		
		int totalPoints = 0;
		
		for (Property p : wm.getProperties().values())
		{
			if (Property.representativeFeatureMap.containsKey(p.getName()) && 
					Property.representativeFeatureMap.get(p.getName()).equals(fs.getName()))
			{
				dataPoints.put(p, new HashSet<DoubleMatrix>());
				for (PerceptCluster pc : p.getPerceptClusters().values())
				{
					DoubleMatrix value = pc.getPercepts().get(fs).getValue();
					dataPoints.get(p).add(value);
					totalPoints++;
				}
			}
		}
		
		pw.println(filename);
		pw.println((fs.getDimension() + 1) + " " + totalPoints); //+ 1 for label
		
		pw.println("label" + " STRING");
		for (int i = 0; i < fs.getDimension(); i++)
		{
			pw.println("V" + i + " FLOAT");
		}
		
		for (Entry<Property,Set<DoubleMatrix>> entry : dataPoints.entrySet())
		{
			
			for (DoubleMatrix dm : entry.getValue())
			{
				pw.print("\"" + entry.getKey().getName() + "\"");
				
				for (double d : dm.data)
				{
					pw.print(" " + (float)d);
				}
				pw.println();
			}
		}
		pw.close();
	}
	
	
}
