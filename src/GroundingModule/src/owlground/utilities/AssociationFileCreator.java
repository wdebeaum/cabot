package owlground.utilities;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Map;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.WorldManager;
import owlground.language.Utterance;
import owlground.spaces.FeatureSpace;

public class AssociationFileCreator {

	WorldManager wm;
	
	public AssociationFileCreator(WorldManager wm)
	{
		this.wm = wm;
	}
	
	public void createTranscriptFile(String filename)
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(filename);
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for labeled transcript write.");
			e.printStackTrace();
			return;
		}
		
		for (FeatureManager session : wm.getSessions())
		{
			int sessionId = session.getSessionId();
			for (Map.Entry<Integer, Utterance> entry : session.getNumberedUtterances().entrySet())
			{
				outputFile.print(sessionId + "-" + entry.getKey());
				for (String word : entry.getValue().getLabeledWords())
				{
					outputFile.print(" " + word);
				}
				outputFile.println();
			}
		}
		outputFile.close();
	}
	
	public void createFeatureFile(String filename)
	{
		PrintWriter outputFile = null;
		try {
			outputFile = new PrintWriter(filename);
		} catch (FileNotFoundException e) {
			System.out.println("Cannot write file for feature file write.");
			e.printStackTrace();
			return;
		}
		
		for (FeatureManager session : wm.getSessions())
		{
			int sessionId = session.getSessionId();
			for (Map.Entry<Integer, Utterance> entry : session.getNumberedUtterances().entrySet())
			{
				outputFile.print(sessionId + "-" + entry.getKey() + " ");
				for (Map.Entry<FeatureSpace, DoubleMatrix> perceptEntry : entry.getValue().getAverageFeatureValues().entrySet())
				{
					double [] data = perceptEntry.getValue().data;
					for (int i = 0; i < data.length; i++)
					{
						outputFile.print(data[i]);
						
						if (i < data.length - 1)
							outputFile.print(",");
					}
					outputFile.print(";");
				}
				outputFile.println();
			}
		}
		outputFile.close();		
	}
}
