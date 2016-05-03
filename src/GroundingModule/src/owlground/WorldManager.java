package owlground;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.language.Demonstration;
import owlground.language.Query;
import owlground.language.Utterance;
import owlground.language.Word;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.objects.WorldObject;
import owlground.perception.PerceptCluster;
import owlground.reference.LatticeLearner;
import owlground.reference.ReferenceReasoner;
import owlground.spaces.FeatureSpace;
import owlground.spaces.SpaceManager;


/*
 * Keeps track of all objects received from the Dialogue Agent. 
 * 
 */


public class WorldManager {
	

	// Each FeatureManager corresponds to a session. This is added to when a 
	// new features file is loaded
	private ArrayList<FeatureManager> sessions;
	// A mapping from the names of properties to Property objects
	private HashMap<String, Property> properties; 
	// A mapping from names of object classes to ObjectClass objects
	private HashMap<String, ObjectClass> objectClasses;
	// A mapping from word strings to Word objects
	private HashMap<String, Word> words;
	// Objects mentioned, but not explicitly demonstrated
	private HashMap<String, WorldObject> mentionedObjects;
	// Objects demonstrated
	private HashMap<String, WorldObject> demonstratedObjects;
	// Demonstrations
	private List<Demonstration> demonstrations;
	// Demonstrations without enough information to resolve
	private Set<Demonstration> unresolvedDemonstrations;
	// List of queries to respond to
	private List<Query> queries;
	// List of identification requests to respond to
	private List<WorldObject> identificationRequests;

	// Equivalent descriptors
	private Map<String, String> equivalentDescriptors;
	// Maximum variance ratio for accepting a percept
	public final double FEATURE_THRESHOLD = .25f;
	// Maximum variance ratio for accepting a property percept
	public final double PROPERTY_FEATURE_THRESHOLD = .3f;
	// The length of the description to return
	public final int DESCRIPTION_LENGTH = 2;
	// Maximum distance for feature to consider in mean distance method
	public final double FEATURE_DISTANCE_THRESHOLD = 1f;
	
	private SpaceManager spaceManager = null;
	
	private Map<String,Integer> wordCountMap;
	
	private static WorldManager instance;
	
	private LatticeLearner latticeLearner;
	
	private Evaluation evaluation;
	
	String propertyMethod;
	String objectMethod;
	int propertyK;
	int objectK;
	boolean distanceWeighted;
	int minUtterances;
	boolean useObjectModels;
	boolean learnRepresentativeFeatures = false;
	
	private static boolean LAST_GUESS = false;
	private static boolean USE_HAND_DISTANCE = false;
	private static int DEMONSTRATION_MEMORY = 10;
	
	
	public void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("classifiers.property.method"))
			propertyMethod = properties.get("classifiers.property.method");
		if (properties.containsKey("classifiers.object.method"))
			objectMethod = properties.get("classifiers.object.method"); 
		if (properties.containsKey("classifiers.property.k"))
			propertyK = Integer.parseInt(properties.get("classifiers.property.k")); 
		if (properties.containsKey("classifiers.object.k"))
			objectK = Integer.parseInt(properties.get("classifiers.object.k")); 
		if (properties.containsKey("classifiers.distanceWeighted"))
			distanceWeighted = Boolean.parseBoolean(properties.get("classifiers.distanceWeighted").toLowerCase()); 
		if (properties.containsKey("classifiers.minutterances"))
			minUtterances = Integer.parseInt(properties.get("classifiers.minutterances"));
		if (properties.containsKey("reference.reasoner.lastguess"))
			LAST_GUESS = Boolean.parseBoolean(properties.get("reference.reasoner.lastguess"));
		if (properties.containsKey("classifiers.objectmodels.enabled"))
			useObjectModels = Boolean.parseBoolean(properties.get("classifiers.objectmodels.enabled").toLowerCase()); 
		if (properties.containsKey("classifiers.learnrepresentativefeatures"))
			learnRepresentativeFeatures = Boolean.parseBoolean(properties.get("classifiers.learnrepresentativefeatures").toLowerCase()); 
		if (properties.containsKey("features.usehanddistance"))
			USE_HAND_DISTANCE = Boolean.parseBoolean("features.usehanddistance");
		
	}

	
	public WorldManager(FeatureManager featureManager)
	{
		this.sessions = new ArrayList<FeatureManager>();
		sessions.add(featureManager);
		featureManager.setWorldManager(this);
		spaceManager = featureManager.getSpaceManager();
		this.objectClasses = new HashMap<String, ObjectClass>();
		this.mentionedObjects = new HashMap<String, WorldObject>();
		this.demonstratedObjects = new HashMap<String, WorldObject>();
		this.properties = new HashMap<String, Property>();
		this.words = new HashMap<String, Word>();
		this.identificationRequests = new LinkedList<WorldObject>();
		this.queries = new LinkedList<Query>();

		this.equivalentDescriptors = new HashMap<String,String>();
		this.wordCountMap = new HashMap<String,Integer>();
		this.demonstrations = new ArrayList<Demonstration>();
		this.unresolvedDemonstrations = new HashSet<Demonstration>();
		WorldManager.instance = this;
		latticeLearner = new LatticeLearner();
		this.evaluation = new Evaluation();
		
		
		// Default values
		propertyMethod = "knearest";
		objectMethod = "mahalanobis";
		propertyK = 35; // was 35
		objectK = 20; // was 13
		distanceWeighted = true;
		minUtterances = 2;
		useObjectModels = false;
	}
	
	/*
	 * Return String containing precision and recall for the tests run so far. Requires 
	 * a ground truth file to be loaded first.
	 */

	
	public void incrementWordCount(String word)
	{
		String lowercased = word.toLowerCase();
		if (!wordCountMap.containsKey(lowercased))
			wordCountMap.put(lowercased, 0);
		
		wordCountMap.put(lowercased,wordCountMap.get(lowercased)+1);
	}
	
	public int getWordCount(String word)
	{
		String lowercased = word.toLowerCase();
		if (wordCountMap.containsKey(lowercased))
			return wordCountMap.get(lowercased);
		
		return 0;
	}
	

	
	/*
	 * Takes in a psuedo WorldObject containing new properties from a DESCRIBED
	 * message. Merges the WorldObject into an existing object.
	 */
	public void addDescription(WorldObject wo)
	{
		if (demonstratedObjects.containsKey(wo.getClassName()))
		{
			demonstratedObjects.get(wo.getClassName()).mergeWorldObject(wo);
			//addPerceptsToProperties(demonstratedObjects.get(wo.getClassName()));
			
		}
		else if (mentionedObjects.containsKey(wo.getClassName()))
		{
			mentionedObjects.get(wo.getClassName()).mergeWorldObject(wo);
			//addPerceptsToProperties(mentionedObjects.get(wo.getClassName()));
		}
		else
		{
			addDemonstratedObject(wo);
		}

	}
	
	public void addQuery(Query q)
	{
		queries.add(q);
	}
	
	/*
	 * Add a Demonstration to be resolved
	 */
	public void addDemonstration(Demonstration d)
	{
		demonstrations.add(d);
		for (WorldObject wo : d.getNonNullWorldObjects())
		{
			addProperties(wo);
			addObjectClass(wo);
		}
	}

	/*
	 * Add a Demonstration that failed resolution to be resolved later
	 */
	public void addUnresolvedDemonstration(Demonstration d)
	{
		unresolvedDemonstrations.add(d);
	}
	
	/*
	 * Add this WorldObject to the list of those to be identified
	 */
	public void addIdentificationRequest(WorldObject wo)
	{
		identificationRequests.add(wo);
	}

	
	/*
	 * Returns a WorldObject with the properties guessed from perceived
	 * feature data of the object corresponding to objectId (they 
	 * will have been stored in perceivedProperties)
	 */
	public List<Query> identifyObjects(boolean clearQueries)
	{
		if (LAST_GUESS)
			resolveRemainingDemonstrations(true);
		
		for (ObjectClass oc: objectClasses.values())
			oc.updatePerceptClusters();
		
		for (Property p: properties.values())
			p.updatePerceptClusters();
		
		//Disable convex generation for now
		//PolytopePerceptualDataRegion.convexHullGenerationEnabled = false;
		
		
		spaceManager.updateFeatureSpaces();
		
		
		
		//chooseRepresentativeFeaturesWithVectorVariance(minUtterances);
		Map<FeatureSpace,Double> averageScaledSumOfSquares = getScaledSumOfSquares();
		if (learnRepresentativeFeatures)
		{
			RepresentativeFeatureSelection.chooseRepresentativeFeatures(averageScaledSumOfSquares, properties.values(), PROPERTY_FEATURE_THRESHOLD, minUtterances);
			RepresentativeFeatureSelection.chooseRepresentativeClassNameFeatures(averageScaledSumOfSquares, objectClasses.values(), FEATURE_THRESHOLD, minUtterances);
			RepresentativeFeatureSelection.chooseRepresentativeWordFeatures(averageScaledSumOfSquares, words.values(), FEATURE_THRESHOLD, minUtterances);
		}
		
		for (Query query : queries)
		{
			//System.out.println("Non null world objects: " + query.getNonNullWorldObjects());
			for (WorldObject wo: query.getNonNullWorldObjects())
			{
				//System.out.println("Classifying WorldObject " + wo);
				// Classify the unknown object using data closest to hand
				wo.getPerceivedProperties().clear();
				wo.updatePerceptClustersToAttended();
				
				if (propertyMethod == null)
					propertyMethod = "knearest";
				if (objectMethod == null)
					objectMethod = "mahalanobis";
					
				if (useObjectModels)
					Classifiers.generatePerceivedObjectClassWithObjectModels(this, wo);
				else
				{
					if (propertyMethod.toLowerCase().equals("meandistance"))
						Classifiers.generatePerceivedPropertiesWithMeanDistanceAndPrechosenFeatures(this,wo, minUtterances, DESCRIPTION_LENGTH, FEATURE_THRESHOLD);
					else if (propertyMethod.toLowerCase().equals("multiknearest"))
						Classifiers.generatePerceivedPropertiesWithMultiKNearestDistance(this, wo, minUtterances, propertyK, FEATURE_THRESHOLD);
					else if (propertyMethod.toLowerCase().equals("knearestunchosen"))
						Classifiers.generatePerceivedPropertiesWithKNearestDistance(this, wo, minUtterances, propertyK);
					else
						//Classifiers.generatePerceivedPropertiesWithKNearestDistanceAndPrechosenFeatures(this, wo, minUtterances, propertyK);
						Classifiers.generatePerceivedPropertiesWithKNearestPrechosen(this, wo, minUtterances, propertyK);
					//generatePerceivedPropertiesWithMeanDistanceAndPrechosenFeatures(wo);
					
					if (objectMethod.toLowerCase().equals("meandistance"))
						Classifiers.generatePerceivedObjectClassWithMeanDistance(this, wo, minUtterances);
					else if (objectMethod.toLowerCase().equals("mahalanobis"))
						Classifiers.generatePerceivedObjectClassWithKNearestMahalanobisDistance(this, wo, minUtterances, objectK);
					else if (objectMethod.toLowerCase().equals("mahalanobiscombined"))
						Classifiers.generatePerceivedObjectClassWithKNearestMahalanobisDistanceCombined(this, wo, minUtterances, objectK);
					else
						Classifiers.generatePerceivedObjectClassWithKNearestDistance(this, wo, minUtterances, objectK, FEATURE_THRESHOLD);
				}
				
				
				
				updateWordPerceptClusters();
				
				//Classifiers.generateWordsWithKNearestDistance(this, wo, 1, objectK, DESCRIPTION_LENGTH);
				/*
				String wordDescription = "";
				for (String word : wo.getPerceivedWords())
				{
					wordDescription += word + " ";
				}
				System.out.println(wordDescription);
				*/
			}
		}
			
		for (Query q : queries)
		{
			System.out.println("Query: " + q.getUtterance().getUttNum());
			for (WorldObject wo : q.getNonNullWorldObjects())
				System.out.println("Class: " + wo.getClassNameWord());
			q.answer();
		}
		
		for (FeatureManager session : sessions)
			System.out.println("Session " + session.getSessionId() + 
					" has " + session.getAttendedPerceptClusters().size() + " percept Clusters");

		//Debug.debugObjectClasses("objectclasses_info", this);
		//Debug.debugProperties("properties_info", this);
		//Debug.debugWords("words_info", this);
		
		
		System.out.println("Number of property PCs: " + getNumberOfPropertyPerceptClusters());
		System.out.println("Number of object PCs: " + getNumberOfObjectClassPerceptClusters());
		
		List<Query> result = new ArrayList<Query>(queries);
		
		System.out.println(evaluation.getStatistics(queries));
		
		if (clearQueries)
		{
			identificationRequests.clear();
			queries.clear();
		}
		//latticeLearner.printPaths();
		//System.out.println("Total demonstrations: " + latticeLearner.totalDemonstrations);
		//System.out.println("Overspecified demonstrations: " + latticeLearner.overspecifiedDemonstrations);
		//System.out.println("Prior probability: " + latticeLearner.getPriorOverspecificationProbability());
		//System.out.println("Object mean: " + latticeLearner.numberOfObjectsGivenOverspecified.getMean());
		//System.out.println("Object variance: " + latticeLearner.numberOfObjectsGivenOverspecified.getNumericalVariance());
		//System.out.println(getStatistics());
		
		return result;
	}
	
	/*
	 * Takes in a demonstrated WorldObject
	 * TODO: Update properties
	 */
	public void addDemonstratedObject(WorldObject wo)
	{
		demonstratedObjects.put(wo.getObjectId(), wo);
		addPerceptsToProperties(wo);
		addPerceptsToObjectClasses(wo);
		addPerceptsToWords(wo);
		wo.updateUtterances();
	}

	public void resolveDemonstrations()
	{
		int missingFeatureDataCount = 0;
		for (Demonstration d: demonstrations)
		{
			
			if (!USE_HAND_DISTANCE && !d.resolveWorldObjectsWithLattice(latticeLearner) && !d.isMissingFeatureData())
			{
				System.out.println("Adding unresolved demonstration");
				addUnresolvedDemonstration(d);
			}
			else if (d.isMissingFeatureData())
			{
				missingFeatureDataCount++;
				System.out.println("Demonstration was missing feature data");
			}
			else
			{
				d.resolveWithHandDistance();
			}

			resolveRemainingDemonstrations(false);
			//d.resolveWorldObjects();
			//d.resolveWithHandDistance();
		}
		
		System.out.println(missingFeatureDataCount + " demonstrations were missing data");
	}
	
	public void resolveRemainingDemonstrations(boolean lastGuess)
	{

		if (ReferenceReasoner.isEnabled() == false)
			return;
		ReferenceReasoner rr = new ReferenceReasoner(unresolvedDemonstrations);
		rr.updateMetaProbabilities();
		int unresolvedDemonstrationsBefore = unresolvedDemonstrations.size();
		
		Iterator<Demonstration> it = unresolvedDemonstrations.iterator();
		while (it.hasNext())
		{
			//System.out.println("Resolving remaining demonstration");
			Demonstration d = it.next();
			if (d.resolveLattice(true,lastGuess))
				it.remove();
		}
		//System.out.println(unresolvedDemonstrationsBefore + " unresolved demonstrations before");
		//System.out.println(unresolvedDemonstrations.size() + " unresolved demonstrations after");
	}
	
	/*
	 * Takes in a mentioned WorldObject
	 * TODO: Update properties IF we decide to use Mentioned Objects
	 */
	public void addMentionedObject(WorldObject wo)
	{
		// Uncomment if we decide to use mentioned objects
		mentionedObjects.put(wo.getObjectId(), wo);
		addPerceptsToProperties(wo);
		addPerceptsToObjectClasses(wo);
		addPerceptsToWords(wo);
		wo.updateUtterances();
	}
	
	int getNumberOfPropertyPerceptClusters()
	{
		int numPerceptClusters = 0;
		
		for (Property p : properties.values())
		{
			numPerceptClusters += p.getPerceptClusters().size();
		}
		
		return numPerceptClusters;
	}
	
	int getNumberOfObjectClassPerceptClusters()
	{
		int numPerceptClusters = 0;
		
		for (ObjectClass oc : objectClasses.values())
		{
			numPerceptClusters += oc.getPerceptClusters().size();
		}
		
		return numPerceptClusters;
	}
	
	int getNumberOfWordPerceptClusters()
	{
		int numPerceptClusters = 0;
		
		for (Word w : words.values())
		{
			numPerceptClusters += w.getPerceptClusters().size();
		}
		
		return numPerceptClusters;
	}
	
	void updateWordPerceptClusters()
	{
		for (FeatureManager session : sessions)
		{
			for (Utterance u : session.getNumberedUtterances().values())
			{
				for (String wordString : u.getWords())
				{
					wordString = wordString.toLowerCase();
					wordString = wordString.trim();
					wordString = wordString.replaceAll("\\.", "");
					if (!words.containsKey(wordString))
						words.put(wordString, new Word(wordString));
					
					Word w = words.get(wordString);		
					w.addUtterance(u);
					w.addPerceptClusters(u.getAttendedPerceptClusters().values());
				}
			}
		}
	}
	private void addProperties(WorldObject wo)
	{
		for (String propertyString : wo.getProperties())
		{
			if (Property.isFalseWord(propertyString))
				continue;
			if (!properties.containsKey(propertyString))
				properties.put(propertyString, new Property(propertyString));
			
			Property p = properties.get(propertyString);
			//p.addComentions(wo.getProperties());
			
			// TEMP Uncomment			
			for (Utterance u : wo.getUtterances())
				p.addUtterance(u);
		}
	}
	
	/*
	 * Updates all properties found in the WorldObject to add Percepts according
	 * to the utterance during which the WorldObject was demonstrated/mentioned.
	 * 
	 * Functionality overlaps with Property's updatePerceptClusters
	 */
	private void addPerceptsToProperties(WorldObject wo)
	{
		addProperties(wo);
		
		// TEMP Testing without adding these perceptclusters
//		for (String propertyString : wo.getProperties())
//		{
//			Property p = properties.get(propertyString);
//			p.addPerceptClusters(wo.getPerceptClusters().values());
////			for (Utterance u : p.getUtterances())
////			{
////				p.addPerceptClusters(u.getPerceptClusters().values());
////			}
//			
//		}

		for (String negatedPropertyString : wo.getNegatedProperties())
		{
			Property p = properties.get(negatedPropertyString);
			p.addNegativePerceptClusters(wo.getPerceptClusters().values());
		}
	}
	
	private void addObjectClass(WorldObject wo)
	{
		String objectClassString = wo.getClassName();
		if (!objectClasses.containsKey(objectClassString))
			objectClasses.put(objectClassString, new ObjectClass(objectClassString));
		
		ObjectClass oc = objectClasses.get(objectClassString);
		

		// TEMP Uncomment
		for (Utterance u : wo.getUtterances())
			oc.addUtterance(u);
	}
	
	/*
	 * Updates all properties found in the WorldObject to add Percepts according
	 * to the utterance during which the WorldObject was demonstrated/mentioned.
	 * 
	 * Functionality overlaps with ObjectClass updatePerceptClusters
	 */
	private void addPerceptsToObjectClasses(WorldObject wo)
	{
		addObjectClass(wo);
		
		ObjectClass oc = objectClasses.get(wo.getClassName());
		oc.addPerceptClusters(wo.getPerceptClusters().values());
			
		for (ObjectClass negatedClass : wo.getNegatedClasses())
		{
			negatedClass.addNegativePerceptClusters(wo.getPerceptClusters().values());
		}
		
		// This will update according to the utterance perceptclusters, not the blobs
		//oc.updatePerceptClusters();
		
/*		if (pcs == 0)
		{
			System.out.println("No perceptClusters for times: ");
			for (Utterance u : oc.getUtterances())
			{
				System.out.println(u);
			}
		}*/
	}
	
	/*
	 * Updates all properties found in the WorldObject to add Percepts according
	 * to the utterance during which the WorldObject was demonstrated/mentioned.
	 * 
	 * Functionality overlaps with ObjectClass updatePerceptClusters
	 */
	private void addPerceptsToWords(WorldObject wo)
	{
		for (Utterance u : wo.getUtterances())
		{
			for (String wordString : u.getWords())
			{
				wordString = wordString.toLowerCase();
				if (!words.containsKey(wordString))
					words.put(wordString, new Word(wordString));
				
				Word w = words.get(wordString);				
				w.addUtterance(u);
				w.addPerceptClusters(u.getAttendedPerceptClusters().values());
			}
		}
	}
	
	/*
	 * Calculates scaled Sum of Squares for the data, a method for measuring 
	 * variance scaled according to the number of data points.
	 * 
	 * @return Scaled Sum of Square data for each property
	 */
	public Map<FeatureSpace,Double> getScaledSumOfSquares()
	{
		Map<FeatureSpace, Double> result = new HashMap<FeatureSpace, Double>();
		Map<FeatureSpace, DoubleMatrix> mean = new HashMap<FeatureSpace, DoubleMatrix>();
		int numPerceptClusters = 0;
		for (FeatureManager session : sessions)
		{
			for (Utterance u : session.getNumberedUtterances().values())
			{
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					numPerceptClusters++;
					for (FeatureSpace featureSpace : pc.getPercepts().keySet())
					{
						if (!mean.containsKey(featureSpace))
							mean.put(featureSpace, 
									DoubleMatrix.zeros(pc.
											getPercepts().get(featureSpace).
												getValue().length));
						mean.get(featureSpace).addi(pc.getPercepts().get(featureSpace).getValue());
					}
				}
			}
		}
		
		for (FeatureSpace featureSpace : mean.keySet())
		{
			mean.get(featureSpace).divi(numPerceptClusters);
		}
		
		for (FeatureManager session : sessions)
		{
			for (Utterance u : session.getNumberedUtterances().values())
			{
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					
					for (FeatureSpace featureSpace : pc.getPercepts().keySet())
					{
						if (!result.containsKey(featureSpace))
							result.put(featureSpace,0.);
						result.put(featureSpace,result.get(featureSpace) +
												pc.getPercepts().get(featureSpace).
												getValue().distance2(mean.get(featureSpace)));
					}
				}
			}
		}
		
		for (FeatureSpace featureSpace : result.keySet())
		{
			result.put(featureSpace, result.get(featureSpace) / numPerceptClusters);
		}
		
		return result;
		
	}
	
	private Map<FeatureSpace,DoubleMatrix> getVectorVariance()
	{
		Map<FeatureSpace, DoubleMatrix> result = new HashMap<FeatureSpace, DoubleMatrix>();
		Map<FeatureSpace, DoubleMatrix> mean = new HashMap<FeatureSpace, DoubleMatrix>();
		int numPerceptClusters = 0;
		// Initialize map and calculate mean
		for (FeatureManager session : sessions)
		{
			for (Utterance u : session.getNumberedUtterances().values())
			{
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					numPerceptClusters++;
					for (FeatureSpace featureSpace : pc.getPercepts().keySet())
					{
						if (!mean.containsKey(featureSpace))
							mean.put(featureSpace, 
									DoubleMatrix.zeros(pc.
											getPercepts().get(featureSpace).
												getValue().length));
						mean.get(featureSpace).addi(pc.getPercepts().get(featureSpace).getValue());
					}
				}
			}
		}
		
		for (FeatureSpace featureSpace : mean.keySet())
		{
			mean.get(featureSpace).divi(numPerceptClusters);
		}
		
		// Calculate variance
		for (FeatureManager session : sessions)
		{
			for (Utterance u : session.getNumberedUtterances().values())
			{
				for (PerceptCluster pc : u.getAttendedPerceptClusters().values())
				{
					
					for (FeatureSpace featureSpace : pc.getPercepts().keySet())
					{
						if (!result.containsKey(featureSpace))
							result.put(featureSpace,DoubleMatrix.zeros(mean.get(featureSpace).length));
						result.get(featureSpace).addi(
												(pc.getPercepts().get(featureSpace).getValue().sub(mean.get(featureSpace)))
												.mul(
												pc.getPercepts().get(featureSpace).getValue().sub(mean.get(featureSpace))));
					}
				}
			}
		}
		
		for (FeatureSpace featureSpace : result.keySet())
		{
			result.get(featureSpace).divi(numPerceptClusters);
		}
		
		return result;
		
		
	}
	
	public FeatureManager getLastSession()
	{
		return sessions.get(sessions.size() - 1);
	}
	
	public void addSession(FeatureManager fm)
	{
		sessions.add(fm);
		fm.setWorldManager(this);
	}
	
	public void addObjectClass(ObjectClass oc)
	{
		objectClasses.put(oc.getName(), oc);
	}

	public String getPropertyMethod() {
		return propertyMethod;
	}

	public void setPropertyMethod(String propertyMethod) {
		this.propertyMethod = propertyMethod;
	}

	public String getObjectMethod() {
		return objectMethod;
	}

	public void setObjectMethod(String objectMethod) {
		this.objectMethod = objectMethod;
	}

	public int getPropertyK() {
		return propertyK;
	}

	public void setPropertyK(int propertyK) {
		this.propertyK = propertyK;
	}

	public int getObjectK() {
		return objectK;
	}

	public void setObjectK(int objectK) {
		this.objectK = objectK;
	}

	public boolean isDistanceWeighted() {
		return distanceWeighted;
	}

	public void setDistanceWeighted(boolean distanceWeighted) {
		this.distanceWeighted = distanceWeighted;
	}

	public int getMinUtterances() {
		return minUtterances;
	}

	public void setMinUtterances(int minUtterances) {
		this.minUtterances = minUtterances;
	}

	public ArrayList<FeatureManager> getSessions() {
		return sessions;
	}

	public HashMap<String, Property> getProperties() {
		return new HashMap<String, Property>(properties);
	}

	public HashMap<String, ObjectClass> getObjectClasses() {
		return new HashMap<String, ObjectClass>(objectClasses);
	}

	public HashMap<String, WorldObject> getMentionedObjects() {
		return new HashMap<String, WorldObject>(mentionedObjects);
	}

	public HashMap<String, WorldObject> getDemonstratedObjects() {
		return new HashMap<String, WorldObject>(demonstratedObjects);
	}
	
	public List<Demonstration> getDemonstrations()
	{
		return demonstrations;
	}
	
	public List<PerceptCluster> getPerceptClusters() {
		List<PerceptCluster> result = new ArrayList<PerceptCluster>();
		for (Demonstration demonstration : demonstrations)
		{
			for (WorldObject wo : demonstration.getNonNullWorldObjects())
			{
				result.addAll(wo.getPerceptClusters().values());
			}
		}
//		for (FeatureManager session : sessions)
//		{
//			result.addAll(session.getAttendedPerceptClusters().values());
//		}
		
		return result;
	}

	public HashMap<String, Word> getWords() {
		return words;
	}
	
	public void addWordPosStrings(String wordString, ArrayList<String> posStrings)
	{
		wordString = wordString.toLowerCase();
		if (!words.containsKey(wordString))
			words.put(wordString, new Word(wordString));
		
		Word w = words.get(wordString);	
		w.addPosStrings(posStrings);
	}
	
	public static WorldManager getInstance()
	{
		return instance;
	}
	
	public SpaceManager getSpaceManager()
	{
		return spaceManager;
	}
	
	public void setSpaceManager(SpaceManager sm)
	{
		this.spaceManager = sm;
	}

	public Evaluation getEvaluation() {
		return evaluation;
	}
}
