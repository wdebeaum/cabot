package javawrapper;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.HashMap;

public class WordNetMapper implements Serializable {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -1706343390994805393L;
	HashMap<String, String> wordNetSenseNumMapping;			// Mapping from WordNet sense keys to WordNet sense nums
	HashMap<String, String> wordNetMapping;					// Mapping from variable names to WordNet sense keys
	// Components for running the Perl process to do the WordNet sense key - sense num conversion
	private static String conversionScriptDirectory = "";
	private static String conversionScriptName = "";
	
	private static boolean conversionScriptAvailable = false;
	private transient ProcessBuilder conversionProcessBuilder = null;
	private transient Process conversionProcess = null;
	private transient PrintWriter conversionWriter = null;
	private transient BufferedReader conversionReader = null;
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("dialogue.conversion.scriptdirectory"))
			conversionScriptDirectory = properties.get("dialogue.conversion.scriptdirectory");
		if (properties.containsKey("dialogue.conversion.scriptname"))
			conversionScriptName = properties.get("dialogue.conversion.scriptname"); 
		if (properties.containsKey("dialogue.conversion.scriptavailable"))
			conversionScriptAvailable = Boolean.parseBoolean(properties.get("dialogue.conversion.scriptavailable").toLowerCase());
	
	}
	
	/**
	 * Get the WordNet sense key for the given variable
	 * @param variableName The name of the variable (e.g. ONT::V12345)
	 * @return The sense key of the variable, or an empty string if none exists
	 */
	public String getVariableSense(String variableName)
	{
		if (wordNetMapping == null)
			wordNetMapping = new HashMap<String, String>();
		
		if (wordNetMapping.containsKey(variableName))
			return wordNetMapping.get(variableName);
		
		return "";
	}
	
	/**
	 * Get the WordNet sense num for the given WordNet sense key 
	 * @param wordSenseKey 
	 * @return
	 */
	public String getWordSenseNum(String wordSenseKey)
	{
		if (wordNetSenseNumMapping == null)
			wordNetSenseNumMapping = new HashMap<String, String>();
		
		if (wordNetSenseNumMapping.containsKey(wordSenseKey) && !wordNetSenseNumMapping.get(wordSenseKey).equals(""))
			return wordNetSenseNumMapping.get(wordSenseKey);
		else
			return generateWordSenseNum(wordSenseKey);
	}
	
	
	/**
	 * For each WordNet sense key, generate the corresponding WordNet sense num using the Perl script
	 */
	private void generateWordSenseNums()
	{
		if (wordNetSenseNumMapping == null)
			wordNetSenseNumMapping = new HashMap<String, String>();
		
		for (String wordSenseKey : wordNetMapping.values())
		{
			wordNetSenseNumMapping.put(wordSenseKey, getWordSenseNum(wordSenseKey));
		}
	}
	
	
	private String generateWordSenseNum(String wordSenseKey)
	{
		if (!conversionScriptAvailable)
			return "";
		if (conversionWriter == null || conversionReader == null)
		{
			try {
				initConversionProcess();
			}
			catch (IOException e)
			{
				System.out.println("Failed to load conversion script: continuing without WordNet senses");
				conversionScriptAvailable = false;
				return "";
			}
			
		}
		
		String nextLine = null;
		
		try {
			// Write the sense key to the process
			conversionWriter.println(wordSenseKey);
			// Read the result
			nextLine = conversionReader.readLine();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// Formatting
		if (nextLine != null)
		{
			String[] splitString = nextLine.split(" ");
			if (splitString.length < 2)
				return "";
			
			//go#v#1
			String[] wordSenseNumSplit = splitString[1].split("#");
			String word = wordSenseNumSplit[0];
			String pos = wordSenseNumSplit[1];
			String senseNum = wordSenseNumSplit[2];
			
			if (pos.equals("v"))
				pos = "VB";
			else if (pos.equals("n"))
				pos = "NN";
			else if (pos.equals("a"))
				pos = "JJ";
			else if (pos.equals("r"))
				pos = "RB";
			
			String convertedSenseNumString = "WN30WordSense-" + word.toLowerCase() + "_" + pos + "_" + senseNum;
			
			return convertedSenseNumString;
		}
		return "";
	}
	
	/**
	 * Initializes the Perl process to convert WordNet sense keys to WordNet sense nums
	 */
	private void initConversionProcess() throws IOException
	{

		if (!conversionScriptAvailable)
			return;
		conversionProcessBuilder = new ProcessBuilder(conversionScriptName);
		conversionProcessBuilder.directory(new File(conversionScriptDirectory));
		System.out.println("Working directory: " + System.getProperty("user.dir"));
		conversionProcess = conversionProcessBuilder.start();
		conversionWriter = new PrintWriter(new OutputStreamWriter(conversionProcess.getOutputStream()), true);
		conversionReader = new BufferedReader(new InputStreamReader(conversionProcess.getInputStream()));

	
	}
	
	/**
	 * Add a mapping from a variable name to a WordNet sense key
	 * @param variableName
	 * @param wordNetSenseKey
	 */
	public void addWordNetSenseMapping(String variableName, String wordNetSenseKey)
	{
		if (wordNetMapping == null)
			wordNetMapping = new HashMap<String, String>();
		
		if (wordNetSenseNumMapping == null)
			wordNetSenseNumMapping = new HashMap<String, String>();
		
		if (!wordNetMapping.containsKey(variableName))
		{
			wordNetMapping.put(variableName, wordNetSenseKey);
			wordNetSenseNumMapping.put(wordNetSenseKey, getWordSenseNum(wordNetSenseKey));
		}
		
	}
	
	public void cleanUp()
	{
		if (conversionWriter != null)
			conversionWriter.close();

		if (conversionProcess != null)
			conversionProcess.destroy();
	}
	
	/**
	 * Get the WordNet Sense num mapping, and generate it if it doesn't exist
	 * @return a mapping from WordNet sense keys to WordNet sense nums
	 */
	public HashMap<String, String> getWordNetSenseNumMapping() {
		if (wordNetSenseNumMapping == null || wordNetSenseNumMapping.size() < wordNetMapping.size())
			generateWordSenseNums();
		
		return wordNetSenseNumMapping;
	}

	
	/**
	 * @return a mapping from variable names to WordNet sense keys
	 */
	public HashMap<String, String> getWordNetMapping() {
		if (wordNetMapping == null)
			wordNetMapping = new HashMap<String, String>();
		
		return wordNetMapping;
	}
	
}
