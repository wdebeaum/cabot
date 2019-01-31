/*
 * File: SRIWrapper.java
 * Creator: Ian Perera
 */
//


//
// 1. Your module should live in its own package. By convention, the
//    name of the package is the name of the module and the name of
//    this main class for the module.
//
package TRIPS.SRIWrapper;

import java.io.*;
import java.util.*;
import java.math.*;

import spatialreasoning.Plan;
import spatialreasoning.Step;
import utilities.JsonReader;
import utilities.KQMLUtilities;
import utilities.TextToSpeech;
import messages.BlockMessagePuller;
import messages.BlockMessageReader;
import messages.BlockMessageSender;
import messages.CommDataReader;
import messages.EvaluateHandler;
import messages.NetworkConfiguration;
import models.*;
import environment.*;
import features.*;
import goals.*;

import org.json.simple.JSONObject;

import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;


/**
 * Grounding Module for learning properties
 */
public class SRIWrapper extends StandardTripsModule  {

	private String hostName = "localhost";
	private boolean localFeatureExtraction = true;
    private boolean speechEnabled;
    private boolean listenForMessages = false;
    private int callingProcess;
    private BlockMessagePuller blockMessagePuller;
    private CommDataReader commDataReader;
    private BlockMessageReader blockMessageReader;
    private EvaluateHandler evaluateHandler;
    private GoalStateHandler goalStateHandler;
    private Thread blockMessagePullerThread;
    private Thread commDataReaderThread;
    private Thread blockMessageReaderThread;
    private Plan plan;
    private ModelBuilder modelBuilder;
    private int uttnum = 1;
    
    private boolean connectToApparatus = true;
	
	
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
    public SRIWrapper(String argv[], boolean isApplication) {
	    	super(argv, isApplication);
	    	BlockMessageSender.ENABLED = true;
	    	TextToSpeech.APPARATUS_ENABLED = true;
	    	if (argv.length > 2)
	    	{
	    		if (argv[2].contains("t"))
	    		{
	    			System.out.println("Apparatus disabled");
	    			connectToApparatus = false;
	    			BlockMessageSender.ENABLED = false;
	    			TextToSpeech.APPARATUS_ENABLED = false;
	    		}
	    	}
	    	if (argv.length > 3)
	    	{
	    		if (argv[3].contains("t"))
	    		{
	    			System.out.println("Messages enabled");
		    	    	BlockMessageSender.ENABLED = true;
	    			listenForMessages = true;
	    		}
	    	}
	    	if (argv.length > 4)
	    	{
			System.out.println("Apparatus IP specified as: |" + argv[4] + "|");
	    		NetworkConfiguration.apiIp = argv[4];
	    	}
    }

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters (default is not be a standalone
     * application).
     */
    public SRIWrapper(String argv[]) {
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
	name = "SRIWrapper";
	// Perform standard initializations
	super.init();
	TextToSpeech.SRIWRAPPER = this;

	//TextToSpeech tts = new TextToSpeech("");
//	try {
//		BlockMessageSender.sendPostRequest(new Block("0.1,0.1,0.1"));
//	}
//	catch (IOException e)
//	{
//		e.printStackTrace();
//	}
//	
	//LatticeDemo.test(new String[0]);
	modelBuilder = new ModelBuilder();
	goalStateHandler = new GoalStateHandler(modelBuilder, this);
	evaluateHandler = new EvaluateHandler(goalStateHandler,modelBuilder);
	
	plan = new Plan(modelBuilder);
	plan.steps.add(new Step("getresponse",""));
	plan.steps.add(new Step("say", "Hello. Can you show me what a row is?"));
	plan.steps.add(new Step("getresponse","")); // Here is a row.
	//plan.steps.add(new Step("placeblock", new String[]{"relative","0,.17,0"}));
	//plan.steps.add(new Step("say", "Is this a row?"));
	//plan.steps.add(new Step("getresponse","")); // No, they have to be in a line.
	plan.steps.add(new Step("say", "I see. How about this?"));
	//plan.steps.add(new Step("placeblock", new String[]{"relative","0,-.10,0"}));
	plan.steps.add(new Step("placeblock", "linear"));
	//plan.steps.add(new Step("say", "Is this a row?"));
	plan.steps.add(new Step("getresponse","")); // Yes, that's a row. 
	plan.steps.add(new Step("getresponse","")); // Let's build it upwards this time.
	plan.steps.add(new Step("placeblock", new String[]{"linearY","0"}));
	plan.steps.add(new Step("placeblock", new String[]{"linearY","1"}));
	plan.steps.add(new Step("placeblock", new String[]{"linearY","2"}));
	plan.steps.add(new Step("getresponse",""));
	//plan.steps.add(new Step("placeblock", new String[]{"absolute","-0.22,-0.32,0.085"}));
	//plan.steps.add(new Step("placeblock", new String[]{"absolute","-0.22,-0.32,0.17"}));
	//plan.steps.add(new Step("placeblock", new String[]{"absolute","-0.22,-0.32,0.26"}));
	
//	plan.steps.add(new Step("checkpredicate", new String[]{"ONGROUND", "2"}));
//	plan.steps.add(new Step("say", "Good, can you move them so they are right up against each other?"));
//	plan.steps.add(new Step("checkpredicates", new String[]{"TOUCHING", "NEXTTO"}));
//	plan.steps.add(new Step("say", "Great! Lastly, we need to put one block on top of one of those two."));
//	plan.steps.add(new Step("checkpredicates", new String[]{"ONTOPOF", "1"}));
	

	// Subscriptions
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
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (component-status . *) ))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (relate . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (described . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (TURN-FINISHED . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (START-CONVERSATION . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (evaluate . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-transcript . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-parameters . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (init-speech . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (load-json-files . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (shutdown-speech . *)))");
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
		KQMLPerformative.fromString("(subscribe :content (request &key :content (what-next . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (commit . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (SPOKEN . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	blockMessagePuller = new BlockMessagePuller(this, goalStateHandler, plan, 
			NetworkConfiguration.apiIp);
	blockMessagePullerThread = new Thread(blockMessagePuller);

	if (connectToApparatus)
	{
		System.out.println("Connecting to block messages from apparatus...");
		blockMessagePullerThread.start();
	}
	
    commDataReader = new CommDataReader(NetworkConfiguration.apiIp, goalStateHandler);
    commDataReaderThread = new Thread(commDataReader);
    
    if (connectToApparatus)
    {
    	System.out.println("Connecting to comm messages from apparatus...");
    	commDataReaderThread.start();
    }
    
    blockMessageReaderThread = null;
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
		if (!(contentobj instanceof KQMLList)) {
		    errorReply(msg, "content was not a list");
		    return;
		}
		
		if (!listenForMessages)
			return;
		
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();

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
		else if (content0.equalsIgnoreCase("component-status"))
		{
			KQMLObject whatObj = content.getKeywordArg(":what");
			if (whatObj != null)
			{
				if (whatObj instanceof KQMLList)
				{
					KQMLList whatList = (KQMLList)whatObj;
					if (!whatList.isEmpty() && 
							whatList.get(0).toString().equalsIgnoreCase("TESTING"))
					{
						listenForMessages = false;
						System.out.println("Disabled message receiving for SRIWrapper"
								+ " because of testing.");
					}
				}
			}
		}
		else if (content0.equalsIgnoreCase("started-speaking") && speechEnabled)
		{
			KQMLObject uttNum = content.getKeywordArg(":uttnum");
			if (uttNum == null)
			{
				errorReply(msg, "no uttnum in started-speaking message");
				return;
			}
			int uttNumValue = Integer.parseInt(uttNum.stringValue());
			
			
		}
		else if (content0.equalsIgnoreCase("stopped-speaking") && speechEnabled)
		{
			KQMLObject uttNum = content.getKeywordArg(":uttnum");
			if (uttNum == null)
			{
				errorReply(msg, "no uttnum in started-speaking message");
				return;
			}
			int uttNumValue = Integer.parseInt(uttNum.stringValue());
			
		}
		else if (content0.equalsIgnoreCase("utterance")) {
		    KQMLObject text = content.getKeywordArg(":text");
		    KQMLObject uttNum = content.getKeywordArg(":uttnum");
		    
		    if (text == null || uttNum == null) {
		    	errorReply(msg, "malformed UTTERANCE message");
		    } 
		    else {
		    	// Store the utterance to be referenced later and matched with the demonstration
		    	
		    	int uttNumValue = Integer.parseInt(uttNum.stringValue());
		    	
		    	// UNCOMMENT THIS FOR STEP DEMO
		    	//plan.processUtterance(text.stringValue());
		    }
		}
		else if (content0.equalsIgnoreCase("start-conversation")) {
	    	speechEnabled = true;

		}
		else if (content0.equalsIgnoreCase("stop-conversation")) {
	    	speechEnabled = false;
		}
		else if (content0.equalsIgnoreCase("TURN-FINISHED"))
		{
			KQMLObject sender = msg.getParameter(":sender");
			KQMLObject uttnum = content.getKeywordArg(":uttnum");
			
		}
		else if (content0.equalsIgnoreCase("SPOKEN"))
		{
			
			KQMLObject text = content.getKeywordArg(":WHAT");
			System.out.println("Received SPOKEN message with text: " + text.toString() );
			//if (text != null)
			//	TextToSpeech.sayWithoutRepeating(text.stringValue());
			
		}
		else {
		    errorReply(msg, "bad tell: " + content0);
		}		
		
    }

    @Override
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
		if (!(contentobj instanceof KQMLList)) {
		    errorReply(msg, "content was not a list");
		    return;
		}
		
		if (!listenForMessages)
			return;
		
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();
		String replyWith = "NIL";
		KQMLObject replyWithObject = msg.getParameter(":REPLY-WITH");
		if (replyWithObject != null)
			replyWith = replyWithObject.stringValue();
		
		if (content0.equalsIgnoreCase("quit"))
		{
			System.out.println("Shutting down");
			shutdown();
		}
		else if (content0.equalsIgnoreCase("what-next"))
		{
			
			KQMLObject goalName = content.getKeywordArg(":active-goal");
			KQMLObject contextObj = content.getKeywordArg(":context");
			KQMLList context = new KQMLList();
			if (contextObj != null && contextObj instanceof KQMLList)
				context = (KQMLList)contextObj;
			System.out.println("Received what-next");
			goalStateHandler.lastReplyWith = replyWith;
			KQMLList responseContent = goalStateHandler.whatNext(context,goalName.stringValue());
			if (responseContent == null)
			{
				System.out.println("Null response content for what-next");
				return;
			}
			else
			{
				System.out.println("Sending " + responseContent.stringValue());
			}
			KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
			performativeToSend.setParameter(":RECEIVER", "DAGENT");
			System.out.println("Response content: " + responseContent);
			performativeToSend.setParameter(":CONTENT",responseContent);
			performativeToSend.setParameter(":IN-REPLY-TO", replyWith);
			System.out.println("Reply-with: " + replyWith);
			sendKQMLPerformative(performativeToSend);
			
		}
		else if (content0.equalsIgnoreCase("evaluate"))
		{
			System.out.println("Received evaluate");
			KQMLList innerContent = (KQMLList)content.getKeywordArg(":CONTENT");
			KQMLList context = (KQMLList)(content.getKeywordArg(":context"));
			//plan.processKQML(innerContent, context);
			
			String speechAct = innerContent.get(0).stringValue();
			System.out.println("Speech Act: " + speechAct);
			KQMLList responseContent = null;
			
			
			if (speechAct.equalsIgnoreCase("ADOPT"))
			{
				System.out.println("Handling adopt message");
				responseContent = evaluateHandler.handleAdoptMessage(innerContent, context);
			}
			else if(speechAct.equalsIgnoreCase("ASSERTION"))
			{
				responseContent = evaluateHandler.handleAssertionMessage(innerContent, context);
			}
			else if (speechAct.equalsIgnoreCase("ASK-WH"))
			{
				responseContent = evaluateHandler.handleAskWhMessage(innerContent, context);
			}
			else if (speechAct.equalsIgnoreCase("ASK-IF"))
			{
				responseContent = evaluateHandler.handleAskIfMessage(innerContent, context);
			}
			else if (speechAct.equalsIgnoreCase("ANSWER"))
			{
				responseContent = evaluateHandler.handleAnswerMessage(innerContent, context);
			}
			
			if (responseContent == null)
			{
				System.out.println("Null response content for evaluate");
				return;
			}
			
			KQMLPerformative performativeToSend = new KQMLPerformative("REPLY");
			performativeToSend.setParameter(":RECEIVER", "DAGENT");
			System.out.println("Response content: " + responseContent);
			performativeToSend.setParameter(":CONTENT",responseContent);
			performativeToSend.setParameter(":IN-REPLY-TO", replyWith);
			System.out.println("Reply-with: " + replyWith);
			
			sendKQMLPerformative(performativeToSend);
			
		}
		else if (content0.equalsIgnoreCase("set-parameters"))
		{

			// Hostname of processing host
			KQMLObject hostName = content.getKeywordArg(":hostname");
        
			
			if (hostName != null)
			{
				this.hostName = hostName.stringValue();

			}
		}
		else if (content0.equalsIgnoreCase("load-json-files"))
		{
			blockMessagePuller.pause = true;
			if (blockMessageReader != null)
				blockMessageReader.stop = true;
			String metaFilename = content.getKeywordArg(":metafile").stringValue();
			String blockDataFilename = content.getKeywordArg(":blockdatafile").stringValue();
			double speedMultiplier = 1;
			if (content.getKeywordArg(":speedmultiplier") != null)
				speedMultiplier = Double.parseDouble(content.getKeywordArg(":speedmultiplier").stringValue());
			blockMessageReader = new BlockMessageReader(this, goalStateHandler, plan, metaFilename, blockDataFilename);
			blockMessageReader.setSpeedMultiplier(speedMultiplier);
			blockMessageReaderThread = new Thread(blockMessageReader);
			blockMessageReaderThread.start();
		}
		else if (content0.equalsIgnoreCase("load-properties-file"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
			readPropertiesFile(filename);
		}
		else if (content0.equalsIgnoreCase("init-speech"))
		{
			System.out.println("Initializing speech");
			// Hostname of processing host
			KQMLObject hostName = content.getKeywordArg(":hostname");
			KQMLObject messagePort = content.getKeywordArg(":message-port");
			KQMLObject dataPort = content.getKeywordArg(":message-port");

			
		}
		else {
		    errorReply(msg, "bad request: " + content0);
		}
    }
    
	/**
	 * Read the properties file from the given filename, and send results to the corresponding
	 * classes
	 * @param filename
	 */
	public void readPropertiesFile(String filename)
	{
		InputStream in = null;
		HashMap<String,String> properties = new HashMap<String,String>();
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
			    	
			    	properties.put(propertyName.toLowerCase(), propertyValue.toLowerCase());
			    	System.out.println(propertyName.toLowerCase() + ":" + propertyValue.toLowerCase());
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open properties file " + filename.toString() + " for reading.");
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

	}
	
	public void readProperties(HashMap<String, String> properties)
	{
		
	}
	
	public void sendKQMLPerformative(KQMLPerformative performative)
	{
		//System.out.println("Sending performative: " + performative);
		send(performative);
	}
	
	public void sendText(String text)
	{
		try {
		KQMLPerformative startedSpeaking = 
				KQMLPerformative.fromString(
						String.format("(tell :content (started-speaking :channel Desktop"
								+ " :direction input :mode text :uttnum %d))",uttnum));
		
		sendKQMLPerformative(startedSpeaking);
		
		KQMLPerformative stoppedSpeaking = 
				KQMLPerformative.fromString(
						String.format("(tell :content (stopped-speaking :channel Desktop"
								+ " :direction input :mode text :uttnum %d))",uttnum));
		sendKQMLPerformative(stoppedSpeaking);
		
		KQMLPerformative word = 
				KQMLPerformative.fromString(
						String.format("(tell :content (word \"%s\" :index 1 :uttnum %d))",text, uttnum));
		
		sendKQMLPerformative(word);
		
		KQMLPerformative utterance = 
				KQMLPerformative.fromString(
						String.format("(tell :content (utterance :channel Desktop "
								+ ":direction input :mode text :uttnum %d :text \"%s\"))",uttnum, text));
		
		sendKQMLPerformative(utterance);
		}
		catch (IOException ioe)
		{
			ioe.printStackTrace();
		}
		uttnum++;
		
	}
    
	
	@Override
	protected void exit(int n)
	{
		shutdown();
		if (isApplication) {
		    System.exit(n);
		} else {
		    // Try to at least bring down the KQMLDispatcher
		    // Should we maybe just close our socket?
		    if (dispatcher != null)
			dispatcher.shutdown();
		}
	}
	
	private void shutdown()
	{
		System.out.println("Shutting down SRIWrapper");
		if (blockMessagePuller != null)
			blockMessagePuller.stop = true;
		if (commDataReader != null)
			commDataReader.stop = true;
		if (blockMessageReader != null)
			blockMessageReader.stop = true;
		
	
		
		if (blockMessagePullerThread != null)
			blockMessagePullerThread.interrupt();
		if (commDataReaderThread != null)
			commDataReaderThread.interrupt();
		if (blockMessageReaderThread != null)
			blockMessageReaderThread.interrupt();
		

		//System.exit(0);
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
	new SRIWrapper(argv, true).run();
    }
}
