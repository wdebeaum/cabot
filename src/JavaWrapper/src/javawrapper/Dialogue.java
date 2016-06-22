package javawrapper;

import java.io.*;
import java.util.*;


import corenlpwrapper.CoreNLPSentence;


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
	private List<CoreNLPSentence> coreNLPSentences;
	private String filename;								// The filename the Dialogue came from
	private int dialogueNumber;
	private HashMap<String,LFTerm> variableTermMapping;		// Maps variable name to its corresponding logical form
	private boolean parsed;									// Whether this dialogue has received a message for every turn it has sent out
	private int numberOfTurnsParsed;						// How many turns have received a response for at least one sentence
	private transient Iterator<Map.Entry<Integer, Turn>> turnEntryIterator;
	private Turn currentTurn;
	private int numberOfParseFailures;						// Number of errors the dialogue has received
	protected transient PrintWriter lfWriter;
	private WordNetMapper wordNetMapper;

	private boolean waitingOnLastTurn;

	private static final String DEBUG_DIRECTORY = "../../log/";

	
	public Dialogue()
	{
		this("",0);	
	}
	
	public Dialogue(int dialogueNumber)
	{
		this("null",dialogueNumber);
	}
	
	public Dialogue(String filename, int dialogueNumber)
	{
		this.filename = filename;
		this.dialogueNumber = dialogueNumber;
		turns = new TreeMap<Integer,Turn>();
		variableTermMapping = new HashMap<String,LFTerm>();
		this.coreNLPSentences = new ArrayList<CoreNLPSentence>();
		parsed = false;
		numberOfTurnsParsed = 0;
		turnEntryIterator = null;
		currentTurn = null;
		numberOfParseFailures = 0;
		waitingOnLastTurn = false;
		wordNetMapper = new WordNetMapper();
	}
	
	public static void readProperties(HashMap<String, String> properties)
	{

		WordNetMapper.readProperties(properties);
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
	          
	          System.out.println("Recovered dialogue " + d.getFilename() + 
	        		  " with " + d.getTurns().size() + " turns.");
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
	
	
	public void printTurnsToFile()
	{
		for (Turn t : turns.values())
			printTurnToFile(t);
	}
	
	public void printTermsToLfFile(Turn t, List<LFTerm> terms)
	{
		if (lfWriter == null)
			initDebugFile();		
		
		lfWriter.println(t.toString());
		lfWriter.println(t.getPrettyPrintLogicalForm());
		lfWriter.println(t.getExpression());	
		lfWriter.println(terms.size() + " terms");
		
		lfWriter.println(terms.size() + " terms");
		for (LFTerm lft : terms)
		{
			lfWriter.println(lft.toString());
			lfWriter.println();

		}
	}
	
	public void printTurnToFile(Turn t)
	{

		if (lfWriter == null)
			initDebugFile();
		
		lfWriter.println(t.toString());
		lfWriter.println(t.getPrettyPrintLogicalForm());
		lfWriter.println(t.getExpression());
		List<LFTerm> terms = t.getLFTerms();
		lfWriter.println(terms.size() + " terms");

		lfWriter.println();
		
		lfWriter.flush();
	}
	
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
			printTurnToFile(turns.get(turnNumber));
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
		return filename.substring(0,Math.max(0,filename.length() - 4));
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
	protected void cleanUp()
	{
		System.out.println("Cleaning up");
		
		if (lfWriter != null)
			lfWriter.close();
		
		wordNetMapper.cleanUp();
		
		parsed = true;
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
	
	public WordNetMapper getWordNetMapper() {
		return wordNetMapper;
	}

	public void addCoreNLPSentence(CoreNLPSentence sentence)
	{
		coreNLPSentences.add(sentence);
	}
}
