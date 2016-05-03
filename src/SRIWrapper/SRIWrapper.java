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

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;
import java.math.*;

import spatialreasoning.Plan;
import spatialreasoning.Step;
import utilities.JsonReader;
import utilities.TextToSpeech;
import messages.BlockMessagePuller;
import messages.BlockMessageReader;
import messages.CommDataReader;

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
    private int callingProcess;
    private BlockMessagePuller blockMessagePuller;
    private CommDataReader commDataReader;
    private BlockMessageReader blockMessageReader;
    private Thread blockMessagePullerThread;
    private Thread commDataReaderThread;
    private Thread blockMessageReaderThread;
    private Plan plan;
	
	
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

	//TextToSpeech tts = new TextToSpeech("");
	
	//LatticeDemo.test(new String[0]);
	
	plan = new Plan();
//	plan.steps.add(new Step("say", "Hello, today we're going to make a staircase."));
//	plan.steps.add(new Step("say", "First, we'll need two blocks."));
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
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (mentioned . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (demonstrated . *)))");
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
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (word . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (utterance . *)))");
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
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (STARTED-SPEAKING . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (STOPPED-SPEAKING . *)))");
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
		KQMLPerformative.fromString("(subscribe :content (request &key :content (write-assoc-files . *)))");
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
	
	blockMessagePuller = new BlockMessagePuller(this, plan);
	blockMessagePullerThread = new Thread(blockMessagePuller);
	blockMessagePullerThread.start();
    
    commDataReader = new CommDataReader();
    commDataReaderThread = new Thread(commDataReader);
    commDataReaderThread.start();
    
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
		    KQMLObject startTime = content.getKeywordArg(":start-time");
		    KQMLObject endTime = content.getKeywordArg(":end-time");
		    KQMLObject uttNum = content.getKeywordArg(":uttnum");
		    
		    if (text == null || uttNum == null) {
		    	errorReply(msg, "malformed UTTERANCE message");
		    } 
		    else if (startTime == null || endTime == null)
		    {
		    	// Speech utterance
		    	int uttNumValue = Integer.parseInt(uttNum.stringValue());
		    }
		    else {
		    	// Store the utterance to be referenced later and matched with the demonstration
		    	String [] words = text.stringValue().split(" ");
		    	
		    	long startTimeValue = 
		    			(long)(Double.parseDouble(startTime.stringValue()) * 100);
		    	long endTimeValue = 
		    			(long)(Double.parseDouble(endTime.stringValue()) * 100);
		    	int uttNumValue = Integer.parseInt(uttNum.stringValue());
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
		
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();
		
		if (content0.equalsIgnoreCase("quit"))
		{
			System.out.println("Shutting down");
			shutdown();
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
			blockMessageReader = new BlockMessageReader(this, plan, metaFilename, blockDataFilename);
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
		send(performative);
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
