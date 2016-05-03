package owlground.reference;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import owlground.FeatureManager;
import owlground.language.Utterance;
import owlground.objects.WorldObject;

public class ReferenceTester {

	Map<Utterance, List<Integer>> locationIndexMapping; // For each session, a map from
	
	public ReferenceTester() {
		locationIndexMapping = new HashMap<Utterance, List<Integer>>();
	}

	/*
	 * Read in a file containing the objects referenced for that utterance, 
	 * in order from left to right, with commas separating multiple objects
	 * as referents, and each utterance on a separate line. The FeatureManager
	 * is the 
	 */
	public boolean readGroundTruthFile(String file, FeatureManager session)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(file);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return false;
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				
				int uttNum = 1;
			    while ((line = reader.readLine()) != null) {
			    	String[] partitions = line.split(":");
			    	String[] objects = partitions[0].split(",");
			    	//System.out.println("Object list:");
			    	List<Integer> objectsAsInts = new ArrayList<Integer>();
			    	for (String object : objects)
			    	{
			    		int objectAsInt = Integer.parseInt(object);
			    		System.out.print(objectAsInt + "-");
			    		objectsAsInts.add(objectAsInt);
			    	}
			    	Utterance currentUtterance = session.getUtterance(uttNum);
			    	if (currentUtterance != null)
			    	{
			    		locationIndexMapping.put(currentUtterance,objectsAsInts);
			    		currentUtterance.setGroundTruthReferences(objectsAsInts);
			    	}
			    	
			    	uttNum++;
			    	
			    }
			    in.close();
			    return true;
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open " + file.toString() + " for reading.");
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		return false;
	}
	
	public void runTest()
	{
		int truePositives = 0;
		int falsePositives = 0;
		int falseNegatives = 0;
		// Take out examples where there was no data
		int cleanTruePositives = 0;
		int cleanFalsePositives = 0;
		int cleanFalseNegatives = 0;

		boolean dataPresent = true;
		for (Utterance u : locationIndexMapping.keySet())
		{
			if (u == null)
			{
				//System.out.println("Utterance is null");
				continue;
			}
			//System.out.println();
			//System.out.println("Utterance: " + u.hashCode() + " " + u);
			List<Integer> groundTruthIndices = locationIndexMapping.get(u);
			List<WorldObject> sortedWorldObjects = 
					sortWorldObjectsAccordingToPosition(u.getWorldObjects());
			//System.out.print("Ground truth: ");
			//for (Integer i : groundTruthIndices)
			//	System.out.print(i + " ");
			//System.out.println();
			//System.out.print("System output(" + sortedWorldObjects.size() + " objects): ");
			if (sortedWorldObjects.size() == 0)
				dataPresent = false;
			else
				dataPresent = true;
			int truePositivesForThisUtterance = 0;
			int cleanTruePositivesForThisUtterance = 0;
			for (int i = 0; i < sortedWorldObjects.size(); i++)
			{
				WorldObject wo = sortedWorldObjects.get(i);
				//System.out.println("World Object: " + wo);
				//if (wo.isFocus())
				//	System.out.print(i + " ");
				if (wo.isFocus() && groundTruthIndices.contains(i))
				{
					truePositives++;
					truePositivesForThisUtterance++;
					if (dataPresent)
					{
						cleanTruePositives++;
						cleanTruePositivesForThisUtterance++;
					}
				}
				else if (wo.isFocus() && !groundTruthIndices.contains(i))
				{
					falsePositives++;
					if (dataPresent)
						cleanFalsePositives++;
				}
			}
				falseNegatives += (groundTruthIndices.size() - truePositivesForThisUtterance);
				if (dataPresent)
					cleanFalseNegatives += (groundTruthIndices.size() - cleanTruePositivesForThisUtterance);
			//System.out.println();
		}
		DecimalFormat df = new DecimalFormat("#.##");
		double precision = ((double)truePositives) / (truePositives + falsePositives);
		double recall = ((double)truePositives) / (truePositives + falseNegatives);
		double fscore = 2 * ((precision * recall) / (precision + recall));
		System.out.println("Precision: " + df.format(precision));
		//System.out.println("Recall: " + recall);
		//System.out.println("F1: " + fscore);
		double cleanPrecision = ((double)cleanTruePositives) / (cleanTruePositives + cleanFalsePositives);
		double cleanRecall = ((double)cleanTruePositives) / (cleanTruePositives + cleanFalseNegatives);
		double cleanFscore = 2 * ((cleanPrecision * cleanRecall) / (cleanPrecision + cleanRecall));
		//System.out.println("Clean Precision: " + cleanPrecision);
		System.out.println("Clean Recall: " + df.format(cleanRecall));
		System.out.println("Clean F1: " + df.format(cleanFscore));
		System.out.println("Overspecification prob: " + ReferenceLattice.OVERSPECIFICATION_PROBABILITY);
	}
	
	public static Set<WorldObject> groundTruthReferenceObjects(Utterance u)
	{
		Set<WorldObject> groundTruthReferenceObjects = new HashSet<WorldObject>();
		List<Integer> groundTruthIndices = u.getGroundTruthReferences();
		
		List<WorldObject> nonNullWorldObjects = new ArrayList<WorldObject>();
		for (WorldObject wo : u.getWorldObjects())
		{
			System.out.println("WorldObject " + wo);
			System.out.println("blob chain: " + wo.getCurrentBlobChain());
			if (wo.getCurrentBlobChain() != null)
			{
				nonNullWorldObjects.add(wo);
				System.out.println("Average X: " + wo.getCurrentBlobChain().getAverageX());
			}
		}
		List<WorldObject> sortedWorldObjects = 
				sortWorldObjectsAccordingToPosition(nonNullWorldObjects);
		
		for (Integer i: groundTruthIndices)
		{
			if (i >= sortedWorldObjects.size())
				continue;
			groundTruthReferenceObjects.add(sortedWorldObjects.get(i));
		}
		return groundTruthReferenceObjects;
	}
	
	public static boolean matchesGroundTruth(Utterance u)
	{
		List<Integer> groundTruthIndices = u.getGroundTruthReferences();
		System.out.println("Utterance has " + u.getWorldObjects().size() + " objects");
		List<WorldObject> nonNullWorldObjects = new ArrayList<WorldObject>();
		for (WorldObject wo : u.getWorldObjects())
		{
			System.out.println("WorldObject " + wo);
			System.out.println("blob chain: " + wo.getCurrentBlobChain());
			if (wo.getCurrentBlobChain() != null)
			{
				nonNullWorldObjects.add(wo);
				System.out.println("Average X: " + wo.getCurrentBlobChain().getAverageX());
			}
		}
		List<WorldObject> sortedWorldObjects = 
				sortWorldObjectsAccordingToPosition(nonNullWorldObjects);
		System.out.print("Ground truth: ");
		for (Integer i : groundTruthIndices)
			System.out.print(i + " ");
		System.out.println();
		System.out.print("System output(" + sortedWorldObjects.size() + " objects): ");
		
		for (int i = 0; i < sortedWorldObjects.size(); i++)
		{
			WorldObject wo = sortedWorldObjects.get(i);
			if (wo.isFocus() && !groundTruthIndices.contains(i))
				return false;
		}
		
		for (int i = 0; i < groundTruthIndices.size(); i++)
		{
			if (i >= sortedWorldObjects.size())
				return false;

			if (!sortedWorldObjects.get(groundTruthIndices.get(i)).isFocus())
				return false;
		}
		
		return true;

	}
	
	private static 
	List<WorldObject> sortWorldObjectsAccordingToPosition(Collection<WorldObject> c) {
	  List<WorldObject> list = new ArrayList<WorldObject>(c);
	  java.util.Collections.sort(list,new PositionComparator());
	  return list;
	}
	
	static class PositionComparator implements Comparator<WorldObject> {
	    @Override
	    public int compare(WorldObject a, WorldObject b) {
	    	int aX = a.getCurrentBlobChain().getAverageX();
	    	int bX = b.getCurrentBlobChain().getAverageX();
	        
	    	if (aX < bX)
	    		return -1;
	    	if (aX == bX)
	    		return 0;
	    	else
	    		return 1;
	    }
	}
	
}
