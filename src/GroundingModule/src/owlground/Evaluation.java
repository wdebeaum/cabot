package owlground;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import owlground.language.Query;
import owlground.objects.WorldObject;

public class Evaluation {
	
	// List of ground truth objects
	private List<String> groundTruthDescriptions;

	public static Map<String,String> recallEquivalent = new HashMap<String,String>() {{
		put("block", "box blocks");
		put("box","block");
		put("blocks","box block");
		put("rectangle","box square cube");
		put("square","cube");
		put("cube","square");
}};

	public static Map<String,String> precisionEquivalent = new HashMap<String,String>() {{
		put("block", "box triangle cube rectangle bridge square blocks");
		put("box","block");
		put("blocks", "box triangle cube rectangle bridge square block");
		put("cube","square");
		put("square", "cube");
}};

	public Evaluation() {
		this.groundTruthDescriptions = new ArrayList<String>();
	}
	
	/*
	 * Read in a file containing the true description of the objects to be described
	 */
	public void readGroundTruthFile(String file)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(file);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return;
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				
			    while ((line = reader.readLine()) != null) {
			    	groundTruthDescriptions.add(line);
			    	
			    }
			    in.close();
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
	}

	public String getStatistics(List<Query> queries)
	{
		System.out.println("STATISTICS");
		int precisionCount = 0;
		int recallCount = 0;
		int totalGroundTruthWords = 0;
		int totalPerceivedDescriptors = 0;
		for (int i = 0; i < groundTruthDescriptions.size(); i++)
		{

			String groundTruthDescription = groundTruthDescriptions.get(i);
			System.out.print("Ground truth: " + groundTruthDescription + ",");
			WorldObject wo = queries.get(i).getNonNullWorldObjects().get(0);
			System.out.print("System output: ");
			for (String propertyWord : wo.getPlainTextDescription())
				System.out.print(propertyWord + " ");
			
			System.out.println();
			foundPrecisionMatch:
			for (String propertyWord : wo.getPlainTextDescription())
			{
				
				totalPerceivedDescriptors++;
				if (groundTruthDescription.contains(propertyWord))
				{
					precisionCount++;
					//System.out.println("Precision match");
				}
				else if (precisionEquivalent.containsKey(propertyWord))
				{
					for (String alternate : precisionEquivalent.get(propertyWord).split(" "))
					{
						if (groundTruthDescription.contains(alternate))
						{
							precisionCount++;
							//System.out.println("Precision match");
							break foundPrecisionMatch;
						}
					}
				}
			}
			
			for (String groundTruthWord : groundTruthDescription.split(" "))
			{
				totalGroundTruthWords++;
				foundRecallMatch:
				for (String propertyWord : wo.getPlainTextDescription())
				{
					if (propertyWord.contains(groundTruthWord))
					{
						recallCount++;
						//System.out.println("Recall match");
						break;
					}
					else if (recallEquivalent.containsKey(propertyWord))
					{
						for (String alternate : recallEquivalent.get(propertyWord).split(" "))
						{
							if (alternate.contains(groundTruthWord))
							{
								recallCount++;
								//System.out.println("Recall match");
								break foundRecallMatch;
							}
						}
					}
				}
			}
		}
		DecimalFormat df = new DecimalFormat("#.##");
		float precision = (float)precisionCount / totalPerceivedDescriptors;
		float recall = (float)recallCount / totalGroundTruthWords;
		float f1 = 2 * (precision * recall) / (precision + recall);
		
		return "Name Precision: " + df.format(precision) + "\nName Recall: " + df.format(recall) +
				"\nName F1: " + df.format(f1);
	}
}
