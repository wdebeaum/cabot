/*
 * File: DEFTXmlReader.java
 * Creator: George Ferguson
 * Created: Thu Jul 19 11:01:04 2012
 * Time-stamp: <Thu Jul 19 15:33:14 EDT 2012 ferguson>
 */
//
// This file demonstrates how to develop a TRIPS system module
// (or component) in Java.
//

//
// 1. Your module should live in its own package. By convention, the
//    name of the package is the name of the module and the name of
//    this main class for the module.
//
package TRIPS.JavaWrapper;

import java.io.*;

import javawrapper.*;
import corenlpwrapper.*;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import java.util.*;
import java.util.concurrent.*;
import java.math.*;

import TRIPS.KQML.*;
//import TRIPS.OWLAgent.owlbackend.LFRefConverter;
import TRIPS.TripsModule.StandardTripsModule;


/**
 * Module for reading XML transcript files for DEFT project
 */
public class JavaWrapper extends StandardTripsModule  {

	// The directory containing the source and bin files
	private final String BASE_DIRECTORY = "../../";
	
	// The directory for storing the serialization of the Dialogues
	private final String SERIAL_DIRECTORY = "../../data/ser/";

	// Number of failures before the parser is probably down from heap overflow and 
	// the system quits
	private final int MAX_FAILURES = 5;

	private int callingProcess = -1;
	
	// Time to wait before moving to next dialogue when finished (should be in 
	// seconds but seems shorter)
	private final int END_DIALOGUE_DELAY = 15;
	// Time to wait when no response has been received from the parser because
	// of an internal error - next sentence will be sent
	private final int PARSE_FAILURE_DELAY = 25;
	// Whether to read in parsed dialogues and write out the triple files
	private boolean writeTriples = false;
	// Used to keep track of timeout
	final ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);
	ScheduledFuture<?> timeoutFuture = null;
	
	private HashMap<String,String> properties = new HashMap<String,String>();
	
	private DialogueIndexer dialogueIndexer;
	private CoreNLPXmlProcessor xmlProcessor;
	
	private List<String> sentencesToSendToParser = new LinkedList<String>();

	
    //
    // 3. You should provide the following two constructors,
    //    both of which accept an array of String parameters. Usually
    //    you just invoke the superclass constructor here (but Java
    //    doesn't inherit constructors so you have to define them yourself).
    //

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters.
     */
    public JavaWrapper(String argv[], boolean isApplication) {
    	super(argv, isApplication);

    }

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters (default is not be a standalone
     * application).
     */
    public JavaWrapper(String argv[]) {
    	this(argv, false);
    }

    //
    // 4. Override the superclass init() method to setup your
    //    module. At least you ought to change the name that it uses to
    //    register with the facilitator, as in the example below.
    //    You also typically define a method to parse whatever
    //    module-specific parameters you might have, and call it from here.
    //    If your module has a UI, you would create it here.
    //    And you do any facilitator subscriptions here also.
    //    End by calling ready() to tell the facilitator that the module
    //    is ready to receive messages.
    //

    /**
     * StandardTripsModule initialization method.
     */
    @Override
    public void init() {
	// Unless overriden by parameters...
	name = "JavaWrapper";
	// Perform standard initializations
	super.init();
	dialogueIndexer = new DialogueIndexer(SERIAL_DIRECTORY);
	xmlProcessor = new CoreNLPXmlProcessor(dialogueIndexer,SERIAL_DIRECTORY,BASE_DIRECTORY);
	

	// Subscriptions
	// These tell the module to listen for messages of these types, which are then handled in the 
	// receiveRequest and receiveTell methods

	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-directory . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-file . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-write-files . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (load-properties-file . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (quit . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-calling-process . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (tell &key :content (CPS-ACT-HYPS . *)))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
//	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (tell &key :content (NEW-SPEECH-ACT . *)))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
//	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (tell &key :content (NEW-SPEECH-ACT-HYPS . *)))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (LOG-SPEECHACT . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
//	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (tell &key :content (INTERPRETATION-FAILED . *)))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
//	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (tell &key :content (END-OF-TURN . *)))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
//	
//	try {
//	    KQMLPerformative perf =
//		KQMLPerformative.fromString("(subscribe :content (sorry &key))");
//	    send(perf);
//	} catch (IOException ex) {
//	    error("Yow! Subscription failed: " + ex);
//	}
	
	System.out.println("Java module ready");
	// Tell the Facilitator we are ready
	ready();
    }

    //
    // 5. You need handlers for any messages you expect to receive.
    //    Look at TRIPS.TripsModule.TripsModule for a list of these
    //    methods, which correspond to the KQML performatives.
    //    The StandardTripsModule defaults for these methods reply with
    //    an error message (since you didn't handle it yourself).
    //    The following is a simple example of picking apart a TELL
    //    message (actually matches the example subscription in init()).
    //    See the javadoc for the TRIPS.KQML classes for more info.
    //

    /**
     * Receive a TELL message from the Facilitator.
     */
    @Override
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
    	
    	if (!(contentobj instanceof KQMLList)) 
    	{
		    errorReply(msg, "content was not a list");
		    return;
		}
    	
		KQMLList content = (KQMLList)contentobj;
		String contentName = content.get(0).toString();

		//KQMLObject id = content.getKeywordArg(":id");
		//String idString = id.stringValue();
		
		// Contains the logical form from the interpretation manager (IM) - we use the semantics from
		// here
		if (contentName.equalsIgnoreCase("CPS-ACT-HYPS"))
		{
			int uttnum = Integer.parseInt(content.getKeywordArg(":UTTNUM").stringValue());
			
			if (dialogueIndexer.getCurrentDialogue().getTurns().containsKey(uttnum))
				dialogueIndexer.getCurrentDialogue().getTurns().get(uttnum).addLogicalForm(content);

		}
		// Used to get the syntax tree, but needs additional processing to handle 
		// compound expressions
		else if (contentName.equalsIgnoreCase("NEW-SPEECH-ACT"))
		{
			KQMLList utt = (KQMLList)(content.get(1));
			
			if (utt == null)
				return;
			
			if (utt.getKeywordArg(":TREE") != null)
				dialogueIndexer.getCurrentDialogue().getCurrentTurn().setSyntaxTree(utt.getKeywordArg(":TREE").stringValue());
		}
		// Used to get the WordNet senses, which come from the parser but not the IM
		// NOTE: This shouldn't be needed anymore
		else if (contentName.equalsIgnoreCase("NEW-SPEECH-ACT-HYPS"))
		{
			KQMLList uttList = (KQMLList)(content.get(1));
			
			if (uttList == null)
				return;
			
			ArrayList<Integer> utteranceNumbers = new ArrayList<Integer>();
			
			dialogueIndexer.getCurrentDialogue().getCurrentTurn().addSpeechActHypsLogicalForm(uttList);
		}
		// Sometimes the IM sends its interpretation through this 
		// (currently this seems to be the case)
		else if (contentName.equalsIgnoreCase("LOG-SPEECHACT"))
		{
			KQMLObject uttnumObject = content.getKeywordArg(":UTTNUM");
			
			if (uttnumObject == null)
				return;
			
			int uttnum = Integer.parseInt(uttnumObject.stringValue());
			
			if (dialogueIndexer.getCurrentDialogue().getTurns().containsKey(uttnum))
				dialogueIndexer.getCurrentDialogue().getTurns().get(uttnum).addSpeechActLogicalForm(content);
			else
			{
				KQMLObject inputObject = content.getKeywordArg(":INPUT");
				if (!(inputObject instanceof KQMLList))
					return;
				String sentence = Utilities.getSentenceFromList((KQMLList)inputObject);
				Turn interactiveTurn = new Turn(dialogueIndexer.getInteractiveDialogue());
				interactiveTurn.setPlainText(sentence);
				interactiveTurn.addSpeechActLogicalForm(content);
				interactiveTurn.generateLFTermsFromLogSpeechAct();
				dialogueIndexer.getInteractiveDialogue().addTurn(interactiveTurn);
				dialogueIndexer.getInteractiveDialogue().printTurnToFile(interactiveTurn);
			}
		}
		// This turn has been completed, so the next turn can be sent
		else if (contentName.equalsIgnoreCase("END-OF-TURN") || contentName.equalsIgnoreCase("INTERPRETATION-FAILED"))
		{
			int uttnum = Integer.parseInt(content.getKeywordArg(":UTTNUM").stringValue());
			
			if (!dialogueIndexer.getCurrentDialogue().getCurrentTurn().isParsed() && dialogueIndexer.getCurrentDialogue().getCurrentTurn().getTurnNumber() <= uttnum)
			{
				if (dialogueIndexer.getCurrentDialogue().isWaitingOnLastTurn())
				{
					System.out.println("Waiting on messages");
					return;
				}
				
				// This turn is parsed - is the Dialogue finished?
				if (dialogueIndexer.getCurrentDialogue().turnParsed(uttnum))
				{
					System.out.println("Completed dialogue");
					System.out.println("Waiting for remaining messages");
					dialogueIndexer.getCurrentDialogue().setWaitingOnLastTurn(true);
					startTimeout(END_DIALOGUE_DELAY);
					return;
				}
				
				if (!dialogueIndexer.getCurrentDialogue().hasNextTurn())
				{
					System.out.println("Waiting on last turn");
					dialogueIndexer.getCurrentDialogue().setWaitingOnLastTurn(true);
				}
				else
				{		
					timeoutFuture.cancel(false);
					Turn turnToSend = dialogueIndexer.getCurrentDialogue().nextTurn();
					sendTurnToParser(turnToSend);
				}	
			}
		}
    }

    @Override
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
		
    	if (!(contentobj instanceof KQMLList)) 
    	{
		    errorReply(msg, "content was not a list");
		    return;
		}
		
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();
		
		// Read the XML files in the given directory
		if (content0.equalsIgnoreCase("read-directory"))
		{
			
			String directory = content.getKeywordArg(":directory").stringValue();
			sentencesToSendToParser.addAll(xmlProcessor.readSentencesFromFilesInDirectory(directory));
			
			int uttnum = 1;
			for (String sentence : sentencesToSendToParser)
			{
				sendTextToParser(sentence, uttnum);
				uttnum++;
			}
		}
		
		if (content0.equalsIgnoreCase("load-properties-file"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
			readPropertiesFile(filename);
		}
		
		// Set whether Triples should be written from already parsed Dialogues
		if (content0.equalsIgnoreCase("set-write-files"))
		{
			String value = content.getKeywordArg(":value").stringValue();
			if (value.equalsIgnoreCase("true"))
				writeTriples = true;
			else
				writeTriples = false;
		}
		
		// Read XML file
		if (content0.equalsIgnoreCase("read-file"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
			if (filename == null)
			{
				errorReply(msg, "no file specified");
				return;
			}
			List<String> sentenceList = new LinkedList<String>();
			try
			{
				sentenceList.addAll(xmlProcessor.readSentencesFromFile(new File(filename)));
			}
			catch (FileNotFoundException e)
			{
				System.err.println("File not found");
			}
			int uttnum = 1;
			for (String sentence : sentenceList)
			{
				sendTextToParser(sentence, uttnum);
			}
			
			//dialogueIndexer.startParsingTurns();
		}
		
		if (content0.equalsIgnoreCase("quit"))
		{
			shutdown();
		}
		
		if (content0.equalsIgnoreCase("set-calling-process"))
		{
			String pid = content.getKeywordArg(":pid").stringValue();
			if (pid == null)
			{
				errorReply(msg, "no pid specified");
				return;
			}
			callingProcess = Integer.parseInt(pid);
		}
    }

    // There was an error in the parsing
    @Override
    public void receiveSorry(KQMLPerformative msg) {
		
		System.out.println("Received sorry");
		
		if (dialogueIndexer.getCurrentDialogue() == null)
			return;
		
    	dialogueIndexer.getCurrentDialogue().addParseFailure();
    	Turn currentTurn = dialogueIndexer.getCurrentDialogue().nextTurn();
		if (currentTurn != null)
		{
			timeoutFuture.cancel(false);
			sendTurnToParser(currentTurn);
		}
			
    }

	
	/**
	 * Sends a restart request to the parser - this is sent between Dialogues to reset the entities
	 * so that they will not be coref'd in a different dialogue
	 **/
	public void sendRestartRequest()
	{
		KQMLPerformative response = new KQMLPerformative("request");
		KQMLList responseContent = new KQMLList();
		responseContent.add("restart");
		response.setParameter(":content", responseContent);
		send(response);
	}
	
	/**
	 * Construct a message for sending the given Turn to the parser
	 **/
	public void sendTurnToParser(Turn t)
	{
		if (t == null)
		{
			System.out.println("Null turn");
			System.out.println("Current dialogue: " + 
					dialogueIndexer.getCurrentDialogue().getBaseName());
			throw new NullPointerException();
			
		}
		
		for (String sentence : t.getCleanAndSplitText())
		{
			KQMLPerformative response = new KQMLPerformative("tell");
			KQMLList responseContent = new KQMLList();
			responseContent.add("started-speaking");
			responseContent.add(":channel");
			responseContent.add("Desktop");
			responseContent.add(":direction");
			responseContent.add("input");
			responseContent.add(":mode");
			responseContent.add("text");
			responseContent.add(":uttnum");
			responseContent.add("" + t.getTurnNumber());
	
			response.setParameter(":SENDER", "JAVAWRAPPER");
			response.setParameter(":content", responseContent);
			send(response);
			
			response = new KQMLPerformative("tell");
			responseContent = new KQMLList();
			responseContent.add("word");
			responseContent.add("\"" + quoteEscaped(sentence) + "\"");
			responseContent.add(":index");
			responseContent.add("1");
			responseContent.add(":uttnum");
			responseContent.add("" + t.getTurnNumber());
			responseContent.add(":frame");
			responseContent.add("(0 " + quoteEscaped(sentence).length() + ")");
			responseContent.add(":channel");
			responseContent.add("Desktop");
			responseContent.add(":direction");
			responseContent.add("input");
			response.setParameter(":content", responseContent);
			send(response);
			
			response = new KQMLPerformative("tell");
			responseContent = new KQMLList();
			responseContent.add("utterance");
			responseContent.add(":channel");
			responseContent.add("Desktop");
			responseContent.add(":direction");
			responseContent.add("input");
			responseContent.add(":mode");
			responseContent.add("text");
			responseContent.add(":uttnum");
			responseContent.add("" + t.getTurnNumber());
			responseContent.add(":text");
			responseContent.add("\"" + quoteEscaped(sentence) + "\"");
			
			response.setParameter(":content", responseContent);
			response.setParameter(":SENDER", "JAVAWRAPPER");
			send(response);
			
			response = new KQMLPerformative("tell");
			responseContent = new KQMLList();
			responseContent.add("stopped-speaking");
			responseContent.add(":channel");
			responseContent.add("Desktop");
			responseContent.add(":direction");
			responseContent.add("input");
			responseContent.add(":mode");
			responseContent.add("text");
			responseContent.add(":uttnum");
			responseContent.add("" + t.getTurnNumber());
	
			response.setParameter(":SENDER", "JAVAWRAPPER");
			response.setParameter(":content", responseContent);
			send(response);
		}
		
		startTimeout(PARSE_FAILURE_DELAY);
	}
	
	public void sendTextToParser(String sentence, int uttnum)
	{
		String quoteEscapedSentence = Utilities.quoteEscaped(sentence);

		KQMLPerformative response = new KQMLPerformative("tell");
		KQMLList responseContent = new KQMLList();
		responseContent.add("started-speaking");
		responseContent.add(":channel");
		responseContent.add("Desktop");
		responseContent.add(":direction");
		responseContent.add("input");
		responseContent.add(":mode");
		responseContent.add("text");
		responseContent.add(":uttnum");
		responseContent.add("" + uttnum);

		response.setParameter(":SENDER", "JAVAWRAPPER");
		response.setParameter(":content", responseContent);
		send(response);
		
		response = new KQMLPerformative("tell");
		responseContent = new KQMLList();
		responseContent.add("word");
		responseContent.add("\"" + quoteEscapedSentence + "\"");
		responseContent.add(":index");
		responseContent.add("1");
		responseContent.add(":uttnum");
		responseContent.add("" + uttnum);
		responseContent.add(":frame");
		responseContent.add("(0 " + quoteEscapedSentence.length() + ")");
		responseContent.add(":channel");
		responseContent.add("Desktop");
		responseContent.add(":direction");
		responseContent.add("input");
		response.setParameter(":content", responseContent);
		send(response);
		
		response = new KQMLPerformative("tell");
		responseContent = new KQMLList();
		responseContent.add("utterance");
		responseContent.add(":channel");
		responseContent.add("Desktop");
		responseContent.add(":direction");
		responseContent.add("input");
		responseContent.add(":mode");
		responseContent.add("text");
		responseContent.add(":uttnum");
		responseContent.add("" + uttnum);
		responseContent.add(":text");
		responseContent.add("\"" + quoteEscapedSentence + "\"");
		
		response.setParameter(":content", responseContent);
		response.setParameter(":SENDER", "JAVAWRAPPER");
		send(response);
		
		response = new KQMLPerformative("tell");
		responseContent = new KQMLList();
		responseContent.add("stopped-speaking");
		responseContent.add(":channel");
		responseContent.add("Desktop");
		responseContent.add(":direction");
		responseContent.add("input");
		responseContent.add(":mode");
		responseContent.add("text");
		responseContent.add(":uttnum");
		responseContent.add("" + uttnum);

		response.setParameter(":SENDER", "JAVAWRAPPER");
		response.setParameter(":content", responseContent);
		send(response);
		
		try{
			Thread.sleep(1000);
		}
		catch (InterruptedException e)
		{
			e.printStackTrace();
		}
		//startTimeout(PARSE_FAILURE_DELAY);
	}
	
	public void shutdown()
	{
		if (executor != null)
			executor.shutdownNow();
		
		System.out.println("Calling process: " + callingProcess);
		if (callingProcess > 0)
		{
			try{
				Runtime.getRuntime().exec("kill " + callingProcess);
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		System.exit(0);
	}
	

	
	public static String quoteEscaped(String input)
	{
		return input.replace("\\","\\\\").replace("\"", "\\\"").
				replaceAll("[\\p{Z}\\s]", " ").trim();
	}
	
    private class TimeoutTask implements Runnable
    {
		@Override
		public void run()
		{
			System.out.println("Timeout called");
			dialogueIndexer.getCurrentDialogue().addParseFailure();
			if (dialogueIndexer.getCurrentDialogue().getNumberOfParseFailures() > MAX_FAILURES)
			{
				System.out.println("Parser is probably down.");
				System.exit(0);
			}
			if (dialogueIndexer.getCurrentDialogue().isCompleted() || 
					dialogueIndexer.getCurrentDialogue().isWaitingOnLastTurn())
			{
				timeoutFuture.cancel(false);
				boolean nextDialogueExists = dialogueIndexer.setCurrentDialogueToNext();
				
				if (nextDialogueExists)
					sendRestartRequest();
				else
					shutdown();
			}

			Turn currentTurn = dialogueIndexer.getCurrentDialogue().nextTurn();
			if (currentTurn != null)
				sendTurnToParser(currentTurn);
		}    	
    }
	
	private void startTimeout(int seconds)
	{
		if (timeoutFuture != null)
			timeoutFuture.cancel(false);
		timeoutFuture = executor.schedule(new TimeoutTask(), seconds, TimeUnit.SECONDS);
	}
	
	private void resetTimeout(int seconds)
	{
		if (timeoutFuture != null)
			timeoutFuture.cancel(false);
		startTimeout(seconds);
	}
	
	/**
	 * Read the properties file from the given filename, and send results to the corresponding
	 * classes
	 * @param filename
	 */
	public void readPropertiesFile(String filename)
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
				System.out.println("Reading properties...");
				
			    while ((line = reader.readLine()) != null) {
			    	String commentRemoved = line.split("#")[0];
			    	String[] propertySplit = commentRemoved.split("=",2);

			    	// Any ='s in the property value should be okay with this method
			    	String propertyName = propertySplit[0];
			    	String propertyValue = propertySplit[1];
			    	
			    	properties.put(propertyName, propertyValue);
			    	System.out.println(propertyName + ":" + propertyValue);
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open properties file " + filename.toString() + 
						" for reading.");
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		
		this.readProperties(properties);
		Dialogue.readProperties(properties);
		//LFRefConverter.readProperties(properties);
	}
	
	public void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("parser.writefiles"))
			writeTriples = Boolean.parseBoolean(properties.get("parser.writefiles"));
	}
	
	
    //
    // 6. For debugging and testing, you probably want to define
    //    a main() method that launches an instance of your module.
    //    Most modules are launched internally by the facilitator as part
    //    of launching the entire TRIPS system.
    //
	
    /**
     * When run as an application, start an instance of this module.
     */
    public static void main(String argv[]) {
    	new JavaWrapper(argv, true).run();
    }
}
