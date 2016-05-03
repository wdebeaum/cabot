package owlground.language.dialogue;

import java.io.*;
import java.util.*;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLEncoder;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/*import org.semanticweb.owlapi.io.OWLXMLOntologyFormat;
import org.semanticweb.owlapi.io.StreamDocumentTarget;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;*/

import TRIPS.KQML.*;
//import TRIPS.OWLAgent.owlbackend.LFRefConverter;


/**
 * @author iperera
 *
 */
/**
 * @author iperera
 *
 */
/**
 * @author iperera
 *
 */
/**
 * @author iperera
 *
 */
/**
 * @author iperera
 *
 */
public class Dialogue implements Serializable {
	/**
	 * Represents an input dialogue file and stores the results from the parser. 
	 */
	private static final long serialVersionUID = 4L;
	public static String baseOntologyUrl = "";
	public TreeMap<Integer,Turn> turns;
	private String filename;								// The filename the Dialogue came from
	private int dialogueNumber;
	private HashMap<String,LFTerm> variableTermMapping;		// Maps variable name to its corresponding logical form
	private boolean parsed;									// Whether this dialogue has received a message for every turn it has sent out
	private int numberOfTurnsParsed;						// How many turns have received a response for at least one sentence
	private transient Iterator<Map.Entry<Integer, Turn>> turnEntryIterator;
	private Turn currentTurn;
	private int numberOfParseFailures;						// Number of errors the dialogue has received
	private transient PrintWriter lfWriter;
	private transient PrintWriter tripleWriter;
	private transient PrintWriter expWriter;

//	private transient LFRefConverter lfRefConverter = null;
	private boolean waitingOnLastTurn;
	
	
	HashMap<String, String> wordNetSenseNumMapping;			// Mapping from WordNet sense keys to WordNet sense nums
	HashMap<String, String> wordNetMapping;					// Mapping from variable names to WordNet sense keys
	// Components for running the Perl process to do the WordNet sense key - sense num conversion
	private static String conversionScriptDirectory = "";
	private static String conversionScriptName = "";
	private static boolean sendOntology = true;
	
	private static String prologServerBaseUrl = "";

	private static String prologServerAuthToken= ""; 
	private static boolean sendProlog = true;
	
	// Server address for sending OWL ontology files
	private static String ontologyServerBaseUrl = "";
	// Authentication token for server
	private static String ontologyServerAuthToken = "";
	// Default directory to store the ontologies
	private static final String ONTOLOGY_DIRECTORY = "../../data/ont/";
	// Default directory to write Gate triples
	private static final String TRIPLE_DIRECTORY = "../../data/tpl/";
	// Default directory to write Prolog-style TRIPS data
	private static final String EXP_DIRECTORY = "../../data/exp/";
	private static final String DEBUG_DIRECTORY = "../../log/";
	private static boolean conversionScriptAvailable = false;
	private transient ProcessBuilder conversionProcessBuilder = null;
	private transient Process conversionProcess = null;
	private transient PrintWriter conversionWriter = null;
	private transient BufferedReader conversionReader = null;
	
	public Dialogue()
	{
		this("",0);	
	}
	
	public Dialogue(String filename, int dialogueNumber)
	{
		this.filename = filename;
		this.dialogueNumber = dialogueNumber;
		turns = new TreeMap<Integer,Turn>();
		variableTermMapping = new HashMap<String,LFTerm>();
		parsed = false;
		numberOfTurnsParsed = 0;
		turnEntryIterator = null;
		currentTurn = null;
		numberOfParseFailures = 0;
		waitingOnLastTurn = false;
	}
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("dialogue.conversion.scriptdirectory"))
			conversionScriptDirectory = properties.get("dialogue.conversion.scriptdirectory");
		if (properties.containsKey("dialogue.conversion.scriptname"))
			conversionScriptName = properties.get("dialogue.conversion.scriptname"); 
		if (properties.containsKey("ontology.server.baseurl"))
			ontologyServerBaseUrl = properties.get("ontology.server.baseurl"); 
		if (properties.containsKey("ontology.server.auth_token"))
			ontologyServerAuthToken = properties.get("ontology.server.auth_token"); 
		if (properties.containsKey("ontology.server.sendontology"))
			sendOntology = Boolean.parseBoolean(properties.get("ontology.server.sendontology").toLowerCase()); 
		if (properties.containsKey("dialogue.conversion.scriptavailable"))
			conversionScriptAvailable = Boolean.parseBoolean(properties.get("dialogue.conversion.scriptavailable").toLowerCase());
		
		if (properties.containsKey("prolog.server.baseurl"))
			prologServerBaseUrl = properties.get("prolog.server.baseurl"); 
		if (properties.containsKey("prolog.server.auth_token"))
			prologServerAuthToken = properties.get("prolog.server.auth_token"); 
		if (properties.containsKey("prolog.server.sendprolog"))
			sendProlog = Boolean.parseBoolean(properties.get("prolog.server.sendprolog").toLowerCase()); 
	}
	
	public void initDebugFile()
	{
		initDebugFile(DEBUG_DIRECTORY);
	}
	
	/**
	 * Initializes the log files for looking at the input sentences with the output logical forms and OWL expressions
	 * @param directory
	 */
	public void initDebugFile(String directory)
	{
        BufferedWriter bufOut;
		try {
			File debugFile = new File(directory + getBaseName() + ".log");
			debugFile.getAbsoluteFile().getParentFile().mkdirs();
			bufOut = new BufferedWriter(new FileWriter(debugFile));
	        lfWriter = new PrintWriter(bufOut);
	        System.out.println("Init debug file to: " + debugFile.getAbsoluteFile().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}		
	}
	
	public void initTripleFile()
	{
		initTripleFile(TRIPLE_DIRECTORY);
	}
	
	public void initExpFile()
	{
		initExpFile(EXP_DIRECTORY);		
	}
	
	public void initExpFile(String directory)
	{
		BufferedWriter bufOut;
		try {
			File expFile = new File(directory + getBaseName() + ".exp");
			expFile.getAbsoluteFile().getParentFile().mkdirs();
			
			bufOut = new BufferedWriter(new FileWriter(expFile));
	        expWriter = new PrintWriter(bufOut);
	        System.out.println("Init Exp file to: " + expFile.getAbsoluteFile().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				
	}
	
	
	/**
	 * Initializes the triple file for the GATE triples output
	 * @param directory
	 */
	public void initTripleFile(String directory)
	{
		BufferedWriter bufOut;
		try {
			File tripleFile = new File(directory + getBaseName() + ".tpl");
			tripleFile.getAbsoluteFile().getParentFile().mkdirs();
			bufOut = new BufferedWriter(new FileWriter(tripleFile));
	        tripleWriter = new PrintWriter(bufOut);
	        System.out.println("Init triple file to: " + tripleFile.getAbsoluteFile().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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
	
/*	private void initLFRefConverter()
	{
		lfRefConverter = new LFRefConverter();
	}*/
	
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
	
	
    /**
     * Read a Dialogue into this object from a serialized Dialogue in the ObjectInputStream
     * @param inputStream - the inputstream containing the serialized data for a Dialogue object
     * @throws IOException - if the object cannot be read from the inputstream
     * @throws ClassNotFoundException - if the class in the serialized file is not found
     */
    private void readObject(ObjectInputStream inputStream)
            throws IOException, ClassNotFoundException
    {
    	inputStream.defaultReadObject();
        //initDebugFile();
    	lfWriter = null;
        turnEntryIterator = turns.entrySet().iterator();
    }
	
	/**
	 * Adds a new Turn and resets the Turn iterator
	 * @param t - A new turn
	 */
	public void addTurn(Turn t)
	{
		turns.put(t.getTurnNumber(), t);
		turnEntryIterator = turns.entrySet().iterator();
	}
	
	public LFTerm getLFTerm(String variableName)
	{
		return variableTermMapping.get(variableName);
	}
	
	
	/**
	 * Adds a parse failure for this dialogue and returns whether this makes the dialogue finished
	 * @return true if the dialogue has been completed now that an additional parse failure has been logged, false otherwise
	 */
	public boolean addParseFailure()
	{
		numberOfParseFailures++;
		
		return isCompleted();
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
	 * @return true if the Dialogue has received a parse or failure for every turn, false otherwise
	 */
	public boolean isCompleted()
	{
		System.out.println("Parse failures: " + numberOfParseFailures);
		System.out.println("Number of turns parsed: " + numberOfTurnsParsed);
		System.out.println("Number of turns: " + turns.size());
		
		if (numberOfParseFailures + numberOfTurnsParsed >= turns.size() && !hasNextTurn())
		{
			parsed = true;
			return true;
		}
		
		return false;
	}
	
	/**
	 * Read a serialized Dialogue file
	 * @param file
	 * @return the generated Dialogue
	 */
	public static Dialogue fromFile(File file)
	{
		Dialogue d = null;
	    try{
	        //use buffering
	        InputStream fis = new FileInputStream(file);
	        InputStream buffer = new BufferedInputStream(fis);
	        ObjectInput input = new ObjectInputStream (buffer);
	        try{
	          //deserialize the List
	          d = (Dialogue)(input.readObject());
	          
	          System.out.println("Recovered dialogue " + d.getFilename() + " with " + d.getTurns().size() + " turns.");
	        }
	        finally{
	          input.close();
	        }
	      }
	      catch(ClassNotFoundException e){
	        e.printStackTrace();
	      }
	      catch(IOException e){
	        e.printStackTrace();
	      }
	
		return d;
		
	}
	
	/**
	 * Serialize the Dialogue file
	 * @param filename
	 */
	public void writeToFile(String filename)
	{
		try{
			File dialogueFile = new File(filename);
			dialogueFile.getParentFile().mkdirs();
	    	
	        OutputStream file = new FileOutputStream(dialogueFile);
	        OutputStream buffer = new BufferedOutputStream(file);
	        ObjectOutput output = new ObjectOutputStream(buffer);
	        try{
	        	output.writeObject(this);
	        }
	        finally{
	          output.close();
	          cleanUp();
	        }  
		}
        catch (IOException e){
    	  e.printStackTrace();
        }
	}
	
	
/*	public void printTurnsToFile()
	{
		for (Turn t : turns.values())
			printTurnToFile(t);
	}*/
	
/*	public void printTurnToFile(Turn t)
	{
		if (lfWriter == null)
			initDebugFile();
		
		if (tripleWriter == null)
			initTripleFile();
		
		if (expWriter == null)
			initExpFile();
		
		lfWriter.println(t.toString());
		lfWriter.println(t.getPrettyPrintLogicalForm());
		lfWriter.println(t.getExpression());
		List<LFTerm> terms = t.generateLFTerms();
		lfWriter.println(terms.size() + " terms");
		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		for (LFTerm lft : t.generateLFTerms())
		{
			lfWriter.println(lft.toString());
			lfWriter.println(lft.getOWLString());
			
			lfWriter.println();
			
			axioms.addAll(lfRefConverter.getAxiomsForLastTerm());
			
		}
		lfWriter.println("Axioms:");
		for (OWLAxiom oa : axioms)
			lfWriter.println(oa);
		lfWriter.println();
		
		if (t.getExpression() != null && !t.getExpression().equals(""))
			expWriter.println(t.getExpression());
		expWriter.flush();
		lfWriter.flush();
		tripleWriter.flush();
	}*/
	
	public void printDebugFile()
	{
		try{
	    	
	        BufferedWriter bufOut = new BufferedWriter(new FileWriter(getBaseName() + ".log"));
	        PrintWriter out = new PrintWriter(bufOut);
	        try{
	        	for (Turn t : turns.values())
	        	{
	        		out.println(t.toString());
	        		out.println(t.getPrettyPrintLogicalForm());
	        		out.println(t.getSyntaxTree());
	        	}
	        	
	        }
	        finally{
	          out.close();
	        }  
		}
        catch (IOException e){
    	  e.printStackTrace();
        }
	}
	
	// Returns true if the last turn was parsed
	public boolean turnParsed(int turnNumber)
	{
		if (turnNumber > turns.size())
			return false;
		
		if (turns.get(turnNumber).setParsed(true) && turnNumber >= numberOfTurnsParsed)
		{
			numberOfTurnsParsed = turnNumber;
			numberOfParseFailures = 0;
			//printTurnToFile(turns.get(turnNumber));
		}
		
		return isCompleted();
	}
	
	public Turn nextTurn()
	{
		if (turnEntryIterator.hasNext())
		{
			currentTurn = turnEntryIterator.next().getValue();
			
			return currentTurn;
		}
		
		return null;
	}
	
	public boolean hasNextTurn()
	{
		return turnEntryIterator.hasNext();
	}
	
	public Turn getCurrentTurn()
	{
		return currentTurn;
	}
	
	/**
	 * Load the dialogue and Turns from an XML file
	 * @param reader - the XMLStreamReader containing the XML information
	 * @throws NumberFormatException if the integer for turnNumber cannot be parsed
	 * @throws XMLStreamException if the data cannot be read from the stream
	 */
	public void fromXML(XMLStreamReader reader) throws NumberFormatException, XMLStreamException
	{
		Turn t = null;
		while (reader.hasNext()) {
			switch (reader.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					
					if (reader.getLocalName().equals("turn"))
					{
						//System.out.println(reader.getLocalName());
						String startTime = reader.getAttributeValue(null,"start");
						String speaker = reader.getAttributeValue(null,"speaker");
						int turnNumber = Integer.parseInt(reader.getAttributeValue(null,"turn_no"));
						t = new Turn(this, turnNumber, startTime, speaker);
						t.processText(reader);
						this.addTurn(t);
						//System.out.println(t);
					}
					break;
				case XMLStreamConstants.END_ELEMENT:
					if (reader.getLocalName().equals("dialogue"))
					{
						return;
					}
					break;
			}
		}
	}

	public Map<Integer,Turn> getTurns() {
		return Collections.unmodifiableMap(turns);
	}

	public String getFilename() {
		return filename;
	}

	public int getDialogueNumber() {
		return dialogueNumber;
	}
	
	public void addLFTerm(LFTerm l)
	{
		variableTermMapping.put(l.getVariableName(), l);
	}

	public boolean isParsed() {
		return parsed;
	}

	public void setParsed(boolean parsed) {
		this.parsed = parsed;
	}
	
	public String getBaseName()
	{
		return filename.substring(0,filename.length() - 4);
	}
	
	public void debugLogicalForms()
	{
		debugLogicalForms(getBaseName() + ".lfs");
	}
	
	public void debugLogicalForms(String filename)
	{
		
	}
	
	/**
	 * Perform operations after waiting for remaining parses. No further parses will be added
	 */
	private void cleanUp()
	{
		System.out.println("Cleaning up");
		
		if (lfWriter != null)
			lfWriter.close();
		
		if (conversionWriter != null)
			conversionWriter.close();

		if (conversionProcess != null)
			conversionProcess.destroy();
		
		

		if (sendOntology)
		{
			sendOntologyToServer();
		}
		
		if (sendProlog)
		{
			sendPrologToServer();
		}
		
		parsed = true;
	}
	
	public void sendOntologyToServer()
	{
		//lfRefConverter.writeOntology(ONTOLOGY_DIRECTORY + getBaseName() + ".owl");
		String url = "";
		try {
			url = ontologyServerBaseUrl + getBaseName() + "?auth_token=" + 
						URLEncoder.encode(ontologyServerAuthToken, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		//lfRefConverter.sendOntology(url);		
	}
	
	public void sendPrologToServer()
	{
		String url = "";
		
		try {
			url = prologServerBaseUrl + getBaseName() + "?auth_token=" + 
						URLEncoder.encode(prologServerAuthToken, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		String prologData = getExpressionsForTurns();
		sendDataToServer(prologData, url);
	}

	/**
	 * 
	 * @return true if all sentences have been sent to parser but not all parses have been received
	 */
	public boolean isWaitingOnLastTurn() {
		return waitingOnLastTurn;
	}

	public String getExpressionsForTurns()
	{
		StringBuilder sb = new StringBuilder();
		
		for (Turn t : turns.values())
		{
			if (t.getExpression() != null && !t.getExpression().equals(""))
			{
				sb.append(t.getExpression());
				sb.append("\n");
			}
		}
		return sb.toString();
	}
	
	public void setWaitingOnLastTurn(boolean waitingOnTurn) {
		this.waitingOnLastTurn = waitingOnTurn;
	}

	public int getNumberOfTurnsParsed() {
		return numberOfTurnsParsed;
	}

	public void setNumberOfTurnsParsed(int numberOfTurnsParsed) {
		this.numberOfTurnsParsed = numberOfTurnsParsed;
	}
	
	public int getNumberOfParseFailures() {
		return numberOfParseFailures;
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
	
/*	public LFRefConverter getLFRefConverter()
	{
		if (lfRefConverter == null)
			initLFRefConverter();
		
		return lfRefConverter;
	}*/
	
	// Paraphrased from http://www.mkyong.com/java/how-to-send-http-request-getpost-in-java/
	/**
	 * Send the ontology to the specified URL
	 * @param url
	 */
	public void sendDataToServer(String data, String url)
	{	
		System.out.println("URL: " + url);
		URL urlObject;
		try {
			urlObject = new URL(url);
		} catch (MalformedURLException e1) {
			System.err.println("Malformed URL for Mercury file sending");
			e1.printStackTrace();
			return;
		}
		System.out.println("URL is: " + urlObject.toString());
		
		HttpURLConnection connection = null;
		try {
			connection = (HttpURLConnection)urlObject.openConnection();
			connection.setInstanceFollowRedirects(false);
			try {
				connection.setRequestMethod("PUT");
			} catch (ProtocolException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
				return;
			}
			//System.out.println("Initial response code: " + connection.getResponseCode());
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			return;
		}
		
		connection.setRequestProperty("Accept-Language", "en-US,en;q=0.5");
		connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
		connection.setDoOutput(true);
		System.out.println("Connection URL is: " + connection.getURL());
		StringBuffer response;
		try {
			DataOutputStream writeStream = new DataOutputStream(connection.getOutputStream());
			writeStream.writeBytes("conversation[prolog]=");
			writeStream.writeBytes(data);
			
			writeStream.flush();
			writeStream.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}
		
		try
		{
			int responseCode = connection.getResponseCode();
			System.out.println("Response code:" + responseCode);
			
			BufferedReader in = new BufferedReader(new InputStreamReader(
									connection.getInputStream()));
			String inputLine;
			response = new StringBuffer();
			
			while ((inputLine = in.readLine()) != null) {
				response.append(inputLine);
			}
			in.close();
			//print result
			System.out.println(response.toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return;
		}
		
		connection.disconnect();
	}
}
