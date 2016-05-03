package owlground.reference;

import java.io.File;
import java.io.FileFilter;

public class TUNATester {

	private final String BASE_DIRECTORY = "../../src/Systems/bolt/";
	private final String TRAINING_DIRECTORY = "TUNATraining/";
	private final String TESTING_DIRECTORY = "TUNATraining/";
	
	LatticeLearner latticeLearner;
	int totalDemonstrations;
	int overspecifiedDemonstrations;
	int correctDemonstrations;
	int trainingTotalDemonstrations;
	int trainingOverspecifiedDemonstrations;

	public TUNATester(LatticeLearner ll) {
		this.latticeLearner = ll;
		totalDemonstrations = 0;
		correctDemonstrations = 0;
		overspecifiedDemonstrations = 0;
		trainingTotalDemonstrations = 0;
		trainingOverspecifiedDemonstrations = 0;
	}
	
	public void train()
	{
		File[] files = new File(BASE_DIRECTORY+TRAINING_DIRECTORY).listFiles(
				new FileFilter() {
		            public boolean accept(File pathname) {
		                return pathname.getName().endsWith(".xml");}});
		
		for (File file : files)
		{
			TUNAReader reader = new TUNAReader();
			System.out.println("Loading file: " + file);
			reader.loadTUNAFile(file);
			System.out.println("Adding file to latticelearner");
			reader.addToLatticeLearner(latticeLearner);
			if (reader.isOverspecified())
				trainingOverspecifiedDemonstrations++;
			trainingTotalDemonstrations++;
		}
	}
	
	public void test()
	{
		File[] files = new File(BASE_DIRECTORY+TESTING_DIRECTORY).listFiles(
				new FileFilter() {
		            public boolean accept(File pathname) {
		                return pathname.getName().endsWith(".xml");}});
		
		int correctEdges = 0;
		int totalEdges = 0;
		int overspecifiedEdges = 0;
		for (File file : files)
		{
			TUNAReader reader = new TUNAReader();
			reader.loadTUNAFile(file);
			totalEdges += reader.descriptors.size();
			correctEdges += reader.test(latticeLearner);
			overspecifiedEdges += reader.overspecifiedDescriptors.size();
			
//				correctDemonstrations++;
//			totalDemonstrations++;
			//if (reader.isOverspecified())
			//	overspecifiedDemonstrations++;
		}
		//System.out.println("Accuracy: " + (double)correctDemonstrations / totalDemonstrations);
		System.out.println("Accuracy: " + (double)correctEdges / totalEdges);
		//System.out.println("Baseline: " + Math.max((double)overspecifiedDemonstrations / totalDemonstrations,
		//		1.0 - ((double)overspecifiedDemonstrations / totalDemonstrations)));
		//System.out.println("Training proportion: " + (double)trainingOverspecifiedDemonstrations / 
		//										trainingTotalDemonstrations);
		System.out.println(overspecifiedEdges + " overspecified edges");
		System.out.println(totalEdges + " total edges");
	}

}
