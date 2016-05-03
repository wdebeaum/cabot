package owlground.objects;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

import org.jblas.DoubleMatrix;

import owlground.WorldManager;
import owlground.language.Predicate;
import owlground.perception.Blob;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;
import owlground.utilities.Permute;



public class ObjectModel {
	private List<String> variables;
	private List<Predicate> predicates;
	private boolean containsSetVariable;

	
	
	public ObjectModel()
	{
		predicates = new ArrayList<Predicate>();
		variables = new ArrayList<String>();
		containsSetVariable = false;
	}
	
	public void addPredicate(String[] arguments)
	{
		Predicate predicate;
		if (arguments.length == 1)
			predicate = new Predicate(arguments[0]);
		else
			predicate = new Predicate(arguments[0],Arrays.copyOfRange(arguments, 1, arguments.length));
		predicates.add(predicate);
		if (predicate.containsSetVariable())
			containsSetVariable = true;
		
	}
	
	public double getDistance(WorldObject wo)
	{
		List<Blob> closestBlobs = wo.getClosestBlobToHandListInAllUtterances();
		double distanceSum = 0.0;
		int count = 0;
		int medianSubblobs = wo.getMedianSubblobs();
		
		//System.out.println(closestBlobs + " blobs closest to hand");
		for (Blob b : closestBlobs)
		{
			if (b == null)
				continue;
			
			List<Blob> subblobs = b.getSubBlobs();
			if (subblobs.size() > medianSubblobs + 1)
				continue;
			//System.out.println("Blob " + b);
			//System.out.println(subblobs.size() + " sublobs ");
			//System.out.println(variables.size() + " variables ");
			
			// Remove null subblobs
			// Probably don't need this
//			for (Iterator<Blob> i = subblobs.iterator(); i.hasNext();)
//			{
//				if (i.next() == null)
//					i.remove();
//			}
			
			// Need enough subblobs to match
			if (subblobs.size() < variables.size())
				continue;

			//System.out.println("Enough subblobs, calculating...");
			double minDistance = Double.MAX_VALUE;
			// Iterate over all possible assignments of variables to blobs
			for (Iterator i = new Permute(subblobs.toArray(new Blob[0])); i.hasNext();)
			{
				Blob[] subblobArrangement = (Blob[])(i.next());
				//System.out.println("Arr subblob " + subblobArrangement.length);
				double distance = 
						getDistanceForConfiguration(wo, b, subblobArrangement);
				if (distance < minDistance)
					minDistance = distance;
			}
			distanceSum += minDistance;
			count++;
			
//			for (Iterator i = new Permute(variables.toArray(new String[0])); i.hasNext();)
//			{
//				String[] arrangement = (String[])(i.next());
//				
//				double distance = 
//						getDistanceForConfiguration(wo, 
//								new ArrayList<String>(Arrays.asList(arrangement)), subblobs);
//				if (distance < minDistance)
//					minDistance = distance;
//			}
		}
		if (count > 0)
			return distanceSum / count;
		else
			return Double.MAX_VALUE;
	}
	
	public List<String> getExpandedSetVariableList(int numberOfBlobs)
	{
		List<String> result = new ArrayList<String>();
		
		int numberOfVariablesToExpand = numberOfBlobs - getNumberOfSingletonVariables();
		
		for (String variable: variables)
		{
			if (variable.equals(variable.toUpperCase()))
			{
				// Expand the set variables to individual variables
				for (int i = 0; i < numberOfVariablesToExpand; i++)
				{
					result.add(variable + i);
				}
			}
			else
				result.add(variable);
		}
		
		return result;
	}
	
	public List<Predicate> getExpandedPredicateList(int numberOfBlobs)
	{
		List<Predicate> result = new ArrayList<Predicate>();
		
		int numberOfVariablesToExpand = numberOfBlobs - getNumberOfSingletonVariables();
		
		for (Predicate predicate: predicates)
		{
			result.addAll(predicate.expandSets(numberOfVariablesToExpand));
		}
		
		return result;
	}
	
	public double getDistanceForConfiguration(WorldObject wo, Blob blob, Blob[] subblobs)
	{
		double maxDistance = 0.0;
		Map<String, Blob> arrangementMap = new HashMap<String, Blob>();
		List<String> expandedVariableList = getExpandedSetVariableList(subblobs.length);
		List<Predicate> expandedPredicateList = getExpandedPredicateList(subblobs.length);
		
		
//		System.out.println("Variables:");
//		for (String s : expandedVariableList)
//			System.out.print(s + " ");
//		System.out.println("Predicates:");
//		for (Predicate p : expandedPredicateList)
//		{
//			System.out.print(p.getName() + ": ");
//			for (String var : p.getVariables())
//				System.out.print(var + " ");
//		}
		
		for (int i = 0; i < expandedVariableList.size(); i++)
		{
			//System.out.println("Storing variable " + variables.get(i) + " and blob: " + blobs[i]);
			arrangementMap.put(expandedVariableList.get(i), subblobs[i]);
		}
		
		for (Predicate predicate : expandedPredicateList)
		{
			List<Blob> subblobArgumentList = new ArrayList<Blob>();
			
			for (String variable : predicate.getVariables())
			{
				//System.out.println("Retrieving var ." + variable + ".");
				//System.out.println("Subblobs.add " + arrangementMap.get(variable));
				subblobArgumentList.add(arrangementMap.get(variable));
			}
			//System.out.println("Number of subblobs: " + subblobs.size());
			double distance = predicate.getDistance(blob, subblobArgumentList);
			if (distance > maxDistance)
				maxDistance = distance;
			
			//System.out.println("Distance for " + predicate.getName() + ": " + distance);
		}
		return maxDistance;
	}
	
	public static void loadObjectModels(WorldManager wm, String filename)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				boolean readName = false;
				boolean readVariables = false;
				ObjectModel om = new ObjectModel();
			    while ((line = reader.readLine()) != null) {
			    	if (readName == false)
			    	{
			    		String objectName = line.trim().toUpperCase();
			    		if (!wm.getObjectClasses().containsKey(objectName))
			    			wm.addObjectClass(new ObjectClass(objectName));
			    			
			    		wm.getObjectClasses().get(objectName).addObjectModel(om);
			    		readName = true;
			    	}
			    	else if (readName && !readVariables)
			    	{
			    		String[] variables = line.split(",");
			    		for (String variable : variables)
			    		{
			    			om.addVariable(variable.trim());
			    		}
			    		readVariables = true;
			    	}
			    	else if (line.equals(""))
			    	{
			    		om = new ObjectModel();
			    		readName = false;
			    		readVariables = false;
			    	}
			    	else
			    	{
			    		String[] lineSplit;
			    		if (line.contains(")"))
			    		{
			    			lineSplit = line.split("\\) ");
			    			// The parentheses gets lost if there are variables
			    			if (!lineSplit[0].contains(")"))
			    				lineSplit[0] = lineSplit[0] + ")";
			    			om.addPredicate(lineSplit);
			    		}
			    		else
			    		{
			    			lineSplit = line.split(" ");
			    			om.addPredicate(lineSplit);
			    		}
			    		
			    	}
			    	
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open " + filename.toString() + " for reading.");
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
	}
	
	public void addVariable(String variable)
	{
		variables.add(variable);
	}



	public List<String> getVariables() {
		return variables;
	}

	/**
	 * If all variables are singletons, then return the number of variables. If there is at least
	 * one set variable, then this returns -1.
	 * @return the number of variables (or -1 if there is a set variable)
	 */
	public int getNumberOfVariables()
	{
		
		for (String variable : variables)
		{
			if (variable.equals(variable.toUpperCase()))
				return -1;
		}
		
		return variables.size();
	}
	
	public int getNumberOfSingletonVariables()
	{
		int count = 0;
		for (String variable: variables)
		{
			if (variable.equals(variable.toLowerCase()))
				count++;
		}
		return count;
	}
	
	public int getMinimumNumberOfVariables()
	{
		return variables.size();
	}
		

	public List<Predicate> getPredicates() {
		return predicates;
	}
}
