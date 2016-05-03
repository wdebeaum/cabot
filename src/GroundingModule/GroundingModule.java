/*
 * File: GroundingModule.java
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
package TRIPS.GroundingModule;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import javax.sound.sampled.UnsupportedAudioFileException;
import javax.xml.stream.XMLStreamException;

import owlground.reference.ReferenceTester;
import owlground.reference.TUNATester;
import owlground.reference.LatticeLearner;

import java.util.*;
import java.math.*;

import owlground.utilities.AssociationFileCreator;
import owlground.language.Demonstration;
import owlground.FeatureManager;
import owlground.objects.ObjectModel;
import owlground.SocketFeatureManager;;
import owlground.language.Query;
import owlground.geometry.VidaExpertFileCreator;
import owlground.WorldManager;
import owlground.spaces.SpaceManager;
import owlground.objects.WorldObject;
import owlground.language.Utterance;
import owlground.language.Demonstration.DemonstrationType;
import owlground.Manifold;
import owlground.reference.*;
import owlground.objects.WorldObjectGroup;
import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;


/**
 * Grounding Module for learning properties
 */
public class GroundingModule extends StandardTripsModule  {

	private WorldManager worldManager;
	/** The FeatureManager for the current training or testing session. */
	private FeatureManager currentFeatureManager;
	private SocketFeatureManager speechFeatureManager;
	/** The FeatureManager for each session */
	private SpaceManager spaceManager;
	private ReferenceTester referenceTester;
	private ArrayList<String> fileList;
	private int currentFile = -1;
	private int firstUttNumOfFile = 0;
	private int callingProcess = -1;
	private boolean readingFileList = false;
	
	private boolean speechEnabled = false;
	private String hostName = "localhost";
	private boolean localFeatureExtraction = true;
	
	private final String TRANSCRIPT_SUFFIX = "_transcript";
	private final String FEATURE_SUFFIX = "_features.xml";
	private final String REFERENCE_SUFFIX = "_references";
	private final String BASE_DIRECTORY = "../../src/Systems/bolt/";
	private final String FEATURE_DIRECTORY = "features/";
	private final String TRANSCRIPT_DIRECTORY = "transcripts/";
	private final String REFERENCE_DIRECTORY = "references/";
	private final String GROUNDTRUTH_DIRECTORY = "groundtruth/";
	private final String GROUNDTRUTH_SUFFIX = "_gt";
	private final boolean READ_ONLY_FOCUS = false;
	private final boolean FOCUS_ON_DESCRIPTIVE_OBJECTS = true;
	// Port for sending time intervals to frame_sender.py
	private final int DEFAULT_MESSAGE_PORT = 22345;
	// Port for receiving frame data
	private final int DEFAULT_DATA_PORT = 22347;
	
	
	private String[] nonDescriptiveClasses = new String[]{"(:* ONT::REFERENTIAL-SEM W::ONE)",
											"(:* ONT::PHYS-OBJECT W::OBJECT)",
											"ONT::ANY-SEM",
											"(:* ONT::REFERENTIAL-SEM W::MIDDLE)"};
	
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
    public GroundingModule(String argv[], boolean isApplication) {
    	super(argv, isApplication);
    }

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters (default is not be a standalone
     * application).
     */
    public GroundingModule(String argv[]) {
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
	name = "Grounding";
	// Perform standard initializations
	super.init();
	
	// Sneak in a speech test here to check the libraries are working

	//LatticeDemo.test(new String[0]);
	
	// Test TUNA corpus overspecification detection
//	LatticeLearner latticeLearner = new LatticeLearner();
//	TUNATester tunaTester = new TUNATester(latticeLearner);
//	tunaTester.train();
//	tunaTester.test();

	
	// Initialize grounding objects
	
	spaceManager = new SpaceManager();
	currentFeatureManager = new FeatureManager(spaceManager);
	speechFeatureManager = new SocketFeatureManager(spaceManager, hostName, DEFAULT_MESSAGE_PORT, DEFAULT_DATA_PORT);
	worldManager = new WorldManager(currentFeatureManager);
	referenceTester = new ReferenceTester();
	fileList = new ArrayList<String>();

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
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-start-time . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (run-geometry-tests . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-features . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-test-features . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-file-list . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (read-object-models . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (identify-object . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (identify-all-objects . *)))");
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
		else if (content0.equalsIgnoreCase("mentioned") || 
				content0.equalsIgnoreCase("demonstrated") ||
				content0.equalsIgnoreCase("description") ||
				content0.equalsIgnoreCase("relate")) {
			KQMLObject termsObject = content.getKeywordArg(":terms");
			if (termsObject == null || !(termsObject instanceof KQMLList))
			{
				errorReply(msg, "no terms list");
				return;
			}
			KQMLList terms = (KQMLList)(termsObject);
			
			KQMLObject uttNum = content.getKeywordArg(":uttnum");
		    KQMLObject id = content.getKeywordArg(":id");
		    
	    	if (currentFeatureManager.getUtterance(Integer.parseInt(uttNum.stringValue())) == null)
	    	{
	    		System.out.println("No utterance with that number found.");
	    		return;
	    	}
	    	
	    	Demonstration currentDemonstration = new Demonstration(currentFeatureManager.getUtterance(
					Integer.parseInt(uttNum.stringValue())),
				DemonstrationType.valueOf(content0.toUpperCase(Locale.ENGLISH)));
	    	
		    for (KQMLObject termObject : terms)
		    {
		    	KQMLList term = (KQMLList)termObject;
		    	String termId = term.get(1).stringValue();
		    	//Issue with coreference and this method
		    	if (READ_ONLY_FOCUS && !termId.equals(id.stringValue()))
		    		continue;
			    KQMLObject className = term.getKeywordArg(":class");
			    KQMLObject quan = term.getKeywordArg(":quan");
			    KQMLObject proform = term.getKeywordArg(":proform");
			    KQMLObject size = term.getKeywordArg(":size");
			    String quantifier = "";
			    String proformString = "";
			    String sizeString = "";
			    KQMLList properties = (KQMLList)term.getKeywordArg(":properties");
			    
			    ArrayList<String> propertyList = new ArrayList();
			    
			    if (properties != null)
			    {
			    	for (KQMLObject ko : properties)
			    	{
			    		propertyList.add(ko.stringValue());
			    	}
			    }
			    
			    if (quan != null)
			    	quantifier = quan.stringValue();
			    
			    if (proform != null)
			    	proformString = proform.stringValue();
			    
			    if (size != null)
			    	sizeString = size.stringValue();
			    	
			    if (id == null) {
			    	errorReply(msg, "malformed MENTIONED/DEMONSTRATED/DESCRIBED");
			    	continue;
			    } 
			    else {

			    	WorldObject wo = new WorldObject(className.stringValue(),id.stringValue(), propertyList);
			    	WorldObjectGroup wog = new WorldObjectGroup(wo, currentDemonstration,className.stringValue(), quantifier);
			    	wog.setNumber(sizeString);
			    	
			    	wo.setDemonstration(currentDemonstration);
			    	
			    	// If there are multiple objects, pick the one with the descriptive class name
			    	if (FOCUS_ON_DESCRIPTIVE_OBJECTS && terms.size() > 1)
			    	{
			    		boolean nonDescriptive = false;
			    		for (String nonDescriptiveClass : nonDescriptiveClasses)
			    		{
			    			if (className.stringValue().equals(nonDescriptiveClass))
			    			{
			    				nonDescriptive = true;
			    				break;
			    			}
			    		}
			    		if (!nonDescriptive)
			    		{
			    			currentDemonstration.setFocusWorldObjectGroup(wog);
			    			wog.setFocus(true);
			    		}
			    		
			    	}
			    	else
			    	{
			    		currentDemonstration.setFocusWorldObjectGroup(wog);
			    		wog.setFocus(true);
			    	}

			    	wo.addUtterance(currentFeatureManager.getUtterance(Integer.parseInt(uttNum.stringValue())));
			    	
			    	currentDemonstration.addWorldObject(wo);
			    	worldManager.addDemonstration(currentDemonstration);
			    	
			    }
		    }
		}
		else if (content0.equalsIgnoreCase("started-speaking") && speechEnabled)
		{
			long currentTime = System.currentTimeMillis() / 10L;
			KQMLObject uttNum = content.getKeywordArg(":uttnum");
			if (uttNum == null)
			{
				errorReply(msg, "no uttnum in started-speaking message");
				return;
			}
			int uttNumValue = Integer.parseInt(uttNum.stringValue());
			Utterance u = new Utterance(currentFeatureManager, uttNumValue);
			
			u.setStartTime(currentTime);
			currentFeatureManager.storeUtterance(u);
			
			
		}
		else if (content0.equalsIgnoreCase("stopped-speaking") && speechEnabled)
		{
			long currentTime = System.currentTimeMillis() / 10L;
			KQMLObject uttNum = content.getKeywordArg(":uttnum");
			if (uttNum == null)
			{
				errorReply(msg, "no uttnum in started-speaking message");
				return;
			}
			int uttNumValue = Integer.parseInt(uttNum.stringValue());
			Utterance u = currentFeatureManager.getUtterance(uttNumValue);
			u.setEndTime(currentTime);
			
			
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
		    	Utterance u = currentFeatureManager.getUtterance(uttNumValue);
		    	u.setWords(text.stringValue().split(" "));
		    }
		    else {
		    	// Store the utterance to be referenced later and matched with the demonstration
		    	String [] words = text.stringValue().split(" ");
		    	
		    	long startTimeValue = 
		    			(long)(Double.parseDouble(startTime.stringValue()) * 100);
		    	long endTimeValue = 
		    			(long)(Double.parseDouble(endTime.stringValue()) * 100);
		    	int uttNumValue = Integer.parseInt(uttNum.stringValue());
		    	Utterance u = new Utterance(currentFeatureManager,words,startTimeValue,
		    								endTimeValue,currentFeatureManager.getGlobalStartTime(),uttNumValue);
		    	currentFeatureManager.storeUtterance(u);
		    }
		} 
		else if (content0.equalsIgnoreCase("word"))
		{
			String word = content.get(1).toString();
			word = word.replace("\"", "");
			KQMLList senseInfo = (KQMLList)(content.getKeywordArg(":sense-info"));
			
			// We only want single words
			if (word == null || word.split("\\s+").length > 1 || senseInfo == null)
			{
				//errorReply(msg, "malformed word message or multiple words");
			}
			else
			{
				senseInfo = (KQMLList)(senseInfo.nth(0));
				KQMLList posTags = (KQMLList)(senseInfo.getKeywordArg(":trips-parts-of-speech"));
				ArrayList<String> posStrings = new ArrayList<String>();
			    if (posTags != null)
			    {
			    	for (KQMLObject ko : posTags)
			    	{
			    		posStrings.add(ko.stringValue());
			    	}
			    }
				worldManager.addWordPosStrings(word, posStrings);
			}
		}
		else if (content0.equalsIgnoreCase("start-conversation")) {
	    	speechEnabled = true;
			currentFeatureManager = speechFeatureManager;
			worldManager.addSession(currentFeatureManager);

		
		}
		else if (content0.equalsIgnoreCase("stop-conversation")) {
	    	speechEnabled = false;
			currentFeatureManager = new FeatureManager(spaceManager);
			worldManager.addSession(currentFeatureManager);

		
		}
		else if (content0.equalsIgnoreCase("TURN-FINISHED") && readingFileList == true)
		{
			System.out.println("Received Turn finished message reading file list");
			KQMLObject sender = msg.getParameter(":sender");
			KQMLObject uttnum = content.getKeywordArg(":uttnum");
			
			if (sender != null && 
					sender.stringValue().equalsIgnoreCase("DAGENT") &&
					uttnum != null)
			{
				System.out.println("Uttnum: " + uttnum.stringValue());
				System.out.println("Utterances: " + currentFeatureManager.getNumberOfUtterances());
				if (Integer.parseInt(uttnum.stringValue()) == 
						currentFeatureManager.getNumberOfUtterances())
				{
					String nextFilename =
							System.getProperty("user.dir") + "/" +
							BASE_DIRECTORY + FEATURE_DIRECTORY +
							fileList.get(currentFile) + 
							FEATURE_SUFFIX;
					
					// Read the reference file
					String nextReferenceFilename =
							System.getProperty("user.dir") + "/" +
							BASE_DIRECTORY + REFERENCE_DIRECTORY +
							fileList.get(currentFile) + 
							REFERENCE_SUFFIX;

					boolean referenceFileWasRead = 
							referenceTester.readGroundTruthFile(nextReferenceFilename, 
																currentFeatureManager);
					
					String nextGroundTruthFilename =
							System.getProperty("user.dir") + "/" +
							BASE_DIRECTORY + GROUNDTRUTH_DIRECTORY +
							fileList.get(currentFile) + 
							GROUNDTRUTH_SUFFIX;
					
					worldManager.getEvaluation().readGroundTruthFile(nextGroundTruthFilename);
					
					if (!referenceFileWasRead)
						System.out.println("No Ground Truth file, skipping evaluation");
					
					if (!readFeatures(msg,nextFilename))
						currentFile = -1;
					else // Feature reading was successful
					{

							
						currentFile++;
						sendFileListRequest();
					}
				}
			}
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
		
		//
		// This is a request from DAGENT when it sees something like "What is this?" in the dialogue
		// Pragmatic inferences messages are also passed along with the query
		//
		if (content0.equalsIgnoreCase("identify-object")) {
			KQMLObject replyWith = msg.getParameter(":REPLY-WITH");
		    KQMLObject id = content.getKeywordArg(":id");
		    
		    KQMLObject termsObject = content.getKeywordArg(":terms");
			if (termsObject == null || !(termsObject instanceof KQMLList))
			{
				errorReply(msg, "no terms list");
				return;
			}
		    
		    KQMLObject query = content.getKeywordArg(":query-type");
		    Query.QueryType queryType = Query.QueryType.IDENTIFY;
		    
		    KQMLObject className = content.getKeywordArg(":class");
		    String classNameString = "(:* null null)";
		    
		    if (className != null)
		    	classNameString = className.stringValue();
		    
		    KQMLObject quan = content.getKeywordArg(":quan");
		    String quantifier = "";
		    if (quan != null)
		    	quantifier = quan.stringValue();
		    
		    KQMLObject aspect = content.getKeywordArg(":aspect");
		    String aspectString = "";
/*		    if (aspect != null)
		    {
		    	aspectString = aspect.stringValue();
		    	if (aspectString.equalsIgnoreCase("(:* ONT::COLOR W::COLOR)") || 
		    			aspectString.equalsIgnoreCase("(:* ONT::SHAPE W::SHAPE)"))
		    	{
		    		queryType = Query.QueryType.PROPERTY;
		    	}
		    		
		    }*/
		    
		    
		    
		    KQMLList properties = (KQMLList)content.getKeywordArg(":properties");
		    ArrayList<String> propertyList = new ArrayList();
		    
		    KQMLObject uttNum = content.getKeywordArg(":uttnum");
		    KQMLObject proform = content.getKeywordArg(":proform");
		    
		    if (id == null || replyWith == null || uttNum == null) {
		    	errorReply(msg, "missing :id, :replyWith, or :uttNum in MENTIONED");
		    } else {

			    if (properties != null)
			    {
			    	for (KQMLObject ko : properties)
			    	{
			    		propertyList.add(ko.stringValue());
			    	}
			    }
			    
			    int uttNumValue = Integer.parseInt(uttNum.stringValue());
			    
			    if (speechEnabled && currentFeatureManager instanceof SocketFeatureManager)
			    {
			    	int speechChoice = (int)(Math.random() * 5);
		    		KQMLPerformative response = new KQMLPerformative("request");
		    		response.setParameter(":receiver", "SPEECH-OUT");
		    		switch (speechChoice)
		    		{
		    		case 0:
		    			response.setParameter(":content", "(say \"Um, let me see\")");
		    			
		    		case 1:
		    			response.setParameter(":content", "(say \"Give me a second\")");
		    			break;
		    		case 2:
		    			response.setParameter(":content", "(say \"Let me see\")");
		    			break;
		    		case 3:
		    			response.setParameter(":content", "(say \"Let me get a closer look\")");
		    			break;
		    		default:
		    			response.setParameter(":content", "(say \"Hmm\")");
		    			break;
		    		}
		    		
		    		send(response);
			    	System.out.println("Sending XML Request");
			    	try{
			    		((SocketFeatureManager)currentFeatureManager).loadXmlFileFromSocket(uttNumValue);
			    	}
			    	catch (XMLStreamException xe)
			    	{
			    		System.err.println("Couldn't read XML file");
			    		return;
			    	}
			    	catch (IOException ie)
			    	{
			    		System.err.println("Communication error with socket");
			    		return;
			    	}
			    }
			    
		    	WorldObject wo = new WorldObject(classNameString, id.stringValue(), propertyList);
		    	wo.addUtterance(currentFeatureManager.getUtterance(uttNumValue));
		    	
		    	Query q = new Query(currentFeatureManager.getUtterance(uttNumValue),
		    			queryType, replyWith.stringValue(), "");
		    	
		    	wo.setDemonstration(q);
		    	WorldObjectGroup wog = new WorldObjectGroup(wo,q,classNameString,quantifier);
		    	q.addWorldObjectGroup(wog);
		    	wog.setFocus(true);
		    	q.setFocusWorldObjectGroup(wog);
		    	
		    	//worldManager.addIdentificationRequest(wo);
		    	worldManager.addQuery(q);
		    	
		    	if (speechEnabled)
		    	{
		    		identifyAllObjects(true);
		    	}
		    }
		}
		else if (content0.equalsIgnoreCase("set-parameters"))
		{
			// What method to determine properties
			KQMLObject propertyMethod = content.getKeywordArg(":property-method");
			// What method to determine object names
			KQMLObject objectMethod = content.getKeywordArg(":object-method");
			// K value for property nearest neighbor
			KQMLObject propertyKValue = content.getKeywordArg(":property-k");
			// K value for object nearest neighbor
			KQMLObject objectKValue = content.getKeywordArg(":object-k");
			// Is the k-nearest neighbor distance weighted?
			KQMLObject kDistanceWeighted = content.getKeywordArg(":distance-weighted");
			// Minimum number of utterances to consider a property
			KQMLObject minUtterances = content.getKeywordArg(":min-utterances");
			// Hostname of processing host
			KQMLObject hostName = content.getKeywordArg(":hostname");
			
			if (propertyMethod != null)
				worldManager.setPropertyMethod(propertyMethod.stringValue());
			
			if (objectMethod != null)
				worldManager.setObjectMethod(objectMethod.stringValue());
			
			if (propertyKValue != null)
				worldManager.setPropertyK(Integer.parseInt(propertyKValue.stringValue()));
			
			if (objectKValue != null)
				worldManager.setObjectK(Integer.parseInt(objectKValue.stringValue()));
			
			if (kDistanceWeighted != null)
				worldManager.setDistanceWeighted(Boolean.parseBoolean(kDistanceWeighted.stringValue()));
			
			if (minUtterances != null)
				worldManager.setMinUtterances(Integer.parseInt(minUtterances.stringValue()));
			
			if (hostName != null)
			{
				this.hostName = hostName.stringValue();
				if (currentFeatureManager instanceof SocketFeatureManager)
					((SocketFeatureManager)currentFeatureManager).setHostName(hostName.stringValue());
			}
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
			
			int messagePortValue = DEFAULT_MESSAGE_PORT;
			int dataPortValue = DEFAULT_DATA_PORT;
			
			if (messagePort != null)
				messagePortValue = Integer.parseInt(messagePort.stringValue());
			
			if (dataPort != null)
				dataPortValue = Integer.parseInt(dataPort.stringValue());
			
			if (hostName != null && !this.hostName.equals("localhost"))
			{
				this.hostName = hostName.stringValue();
				
				speechFeatureManager = new SocketFeatureManager(spaceManager, hostName.stringValue(), 
																messagePortValue, dataPortValue);
				localFeatureExtraction = false;
				System.out.println("Speech Control Initialized with Host: " + this.hostName + 
						" Msg Port: " + messagePortValue + " Data Port: " + dataPortValue);
			}
			else
			{
				speechFeatureManager = new SocketFeatureManager(spaceManager, messagePortValue);
				this.hostName = "localhost";
				localFeatureExtraction = false;
				System.out.println("Speech Control Initialized with localhost at port: " + messagePortValue);
			}
		}
		else if (content0.equalsIgnoreCase("shutdown-speech"))
		{
			speechFeatureManager.closeSockets();
		}
		//
		// This message causes the system to answer all queries at once, according to the given parameters
		// It can be called multiple times with different parameters, although the messages won't be 
		// accepted by DAGENT after the first
		//
		else if (content0.equalsIgnoreCase("identify-all-objects"))
		{
			identifyAllObjects(false);
		}
		//
		// This message should be sent after the corresponding transcript has been loaded.
		//
		else if (content0.equalsIgnoreCase("read-features")) {
		    String filename = content.getKeywordArg(":file").stringValue();

		    if (filename == null) {
		    	errorReply(msg, "missing :file in READ-FEATURES");
		    } else {
		    	readFeatures(msg, filename);
		    }
		} 
		else if (content0.equalsIgnoreCase("read-file-list"))
		{		    
			String filename = content.getKeywordArg(":file").stringValue();
		    readingFileList = true;
		    if (filename == null) {
		    	errorReply(msg, "missing :file in READ-FILE-LIST");
		    } else {
		    	readFileList(filename);
		    }
		}
		else if (content0.equalsIgnoreCase("read-object-models"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
		    if (filename == null) {
		    	errorReply(msg, "missing :file in READ-OBJECT-MODELS");
		    } else {
		    	ObjectModel.loadObjectModels(worldManager, filename);
		    }
		}
		else if (content0.equalsIgnoreCase("write-assoc-files"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
		    readingFileList = true;
		    if (filename == null) {
		    	errorReply(msg, "missing :file in WRITE-ASSOC-FILES");
		    } else {
		    	AssociationFileCreator afc = new AssociationFileCreator(worldManager);
		    	afc.createFeatureFile(filename + ".ftr");
		    	afc.createTranscriptFile(filename + ".trp");
		    }
		}
		//
		// This is used if a video's timestamps are not aligned (e.g. Ted's video). It specifies the
		// start time in Unix time of the transcript, when the transcript timestamps start at 0.
		//
		else if (content0.equalsIgnoreCase("set-start-time")) {
		    KQMLObject startTime = content.getKeywordArg(":time");
		    
		    if (startTime == null) {
		    	errorReply(msg, "missing :time in SET-START-TIME");
		    } else {
		    	
		    		currentFeatureManager.setGlobalStartTime((long)(Double.parseDouble(startTime.stringValue()) * 100));
		    		send(new KQMLPerformative("GLOBAL-TIME-SET"));

		    }
		}
		else if (content0.equalsIgnoreCase("run-geometry-tests"))
		{
			//Manifold test = new Manifold();
			//TestGeometry.testConvexHull();
		}
		else {
		    errorReply(msg, "bad request: " + content0);
		}

		
    }
    
    
    private boolean readFeatures(KQMLPerformative msg, String filename)
    {
	    if (currentFeatureManager.getUtterance(1) == null)
	    {
	    	errorReply(msg, "No utterances have been loaded.");
	    	return false;
	    }
	    
    	try {
	    	currentFeatureManager.loadInputXmlFile(filename);
	    	worldManager.resolveDemonstrations();
	    	worldManager.resolveRemainingDemonstrations(false);
			currentFeatureManager = new FeatureManager(spaceManager);
			worldManager.addSession(currentFeatureManager);
			send(new KQMLPerformative("FEATURES-LOADED"));
			return true;
	    } catch (Exception e) {
			// TODO Auto-generated catch block
	    	e.printStackTrace();
			errorReply(msg, "XML file load failed");
			return false;
		}    	
    }
    
    private void sendFileListRequest()
    {
    	if (currentFile < 0)
    		return;
    	if (currentFile < fileList.size())
    	{
	    	KQMLList responseContent = new KQMLList();
	    	responseContent.add("read-transcript");
	    	responseContent.add(":file");
	    	responseContent.add("\"" +
	    						BASE_DIRECTORY + TRANSCRIPT_DIRECTORY +
	    						fileList.get(currentFile) +
	    						TRANSCRIPT_SUFFIX +
	    						"\"");
	
	    	KQMLPerformative response = new KQMLPerformative("REQUEST");
			response.setParameter(":RECEIVER", "TRANSCRIPTREADER");
			response.setParameter(":CONTENT", responseContent);
			send(response);
    	}
    	else
    	{
    		identifyAllObjects(false);
    		readingFileList = false;
    		referenceTester.runTest();
    		//identifyAllObjects(false);
    		//referenceTester.runTest();
    	}
    }
    
    private void identifyAllObjects(boolean clearQueries)
    {
		// Identify the objects, returning a list of world objects
		List<Query> identifiedObjects = 
				worldManager.identifyObjects(clearQueries);
		
		//System.out.println("Sending identify responses");
		//System.out.println(identifiedObjects.size() + " responses");
		for (Query q : identifiedObjects)
		{
			// Create a response for each object
	    	for (WorldObject wo : q.getNonNullWorldObjects())
	    	{
	    		//System.out.println("Sending identify response");
		    	KQMLList responseContent = new KQMLList();
		    	responseContent.add("description");
		    	responseContent.add(":ID");
		    	responseContent.add(wo.getObjectId());
		    	responseContent.add(":CLASS");
		    	responseContent.add(wo.getClassName());
		    	responseContent.add(":PROPERTIES");
		    	KQMLList properties = new KQMLList();
		    	for (String propertyString : wo.getPerceivedProperties())
		    		properties.add(propertyString);
		    	
		    	responseContent.add(properties);
		    	if (wo.getClassName().equalsIgnoreCase("(:* null null)"))
		    	{
		    		KQMLPerformative response = new KQMLPerformative("request");
		    		response.setParameter(":receiver", "SPEECH-OUT");
		    		response.setParameter(":content", "(say \"I don't know\")");
		    		send(response);
		    	}
	/*	    	else if (wo.getResponseString() != null)
		    	{
		    		System.out.println("Response string: " + wo.getResponseString());
		    		if (!wo.getResponseString().equalsIgnoreCase("none"))
		    		{
			    		KQMLPerformative response = new KQMLPerformative("request");
			    		response.setParameter(":receiver", "SPEECH-OUT");
			    		response.setParameter(":content", "(say \"" + wo.getResponseString() + "\")");
			    		send(response);
		    		}
		    		else
		    		{
			    		KQMLPerformative response = new KQMLPerformative("request");
			    		response.setParameter(":receiver", "SPEECH-OUT");
			    		response.setParameter(":content", "(say \"I don't know\")");
			    		send(response);
		    		}
		    	}*/
		    	else
		    	{
					KQMLPerformative response = new KQMLPerformative("TELL");
					response.setParameter(":RECEIVER", "DAGENT");
					response.setParameter(":IN-REPLY-TO", q.getReplyWith());
					response.setParameter(":CONTENT", responseContent);
					//System.out.println("Response: " + response.stringValue());
					send(response);
		    	}
				wo.getPerceivedProperties().clear();
	    	}
		}
    	VidaExpertFileCreator.createDatFiles(worldManager);
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
		worldManager.readProperties(properties);
		ReferenceReasoner.readProperties(properties);
		ReferenceLattice.readProperties(properties);
		FeatureManager.readProperties(properties);
		Demonstration.readProperties(properties);
	}
	
	public void readProperties(HashMap<String, String> properties)
	{

	}

    
	public void readFileList(String file)
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
		    	fileList.add(line.trim());
		    }
		    in.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.err.println("Could not open " + file.toString() + " for reading.");
			return;
		} finally {
			try {
				if (in != null)
					in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				return;
			}
		}
		
		currentFile = 0;
		sendFileListRequest();
	}
	
	public void shutdown()
	{
		
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
	new GroundingModule(argv, true).run();
    }
}
