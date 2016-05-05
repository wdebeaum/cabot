package owlground.objects;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.WorldManager;
import owlground.language.Demonstration;
import owlground.language.Utterance;
import owlground.perception.Blob;
import owlground.perception.BlobChain;
import owlground.perception.BlobGroup;
import owlground.perception.BlobGroupChain;
import owlground.perception.Percept;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;

/*
 * Stores an object given to use from a DEMONSTRATED or MENTIONED message.
 * 
 */
public class WorldObject {
	
	HashSet<String> properties; 	//Properties given in the message
	HashSet<String> perceivedProperties;	// Our guesses for what properties it has might go here
	HashSet<String> negatedProperties;
	HashSet<String> perceivedWords;
	HashSet<ObjectClass> negatedObjectClasses;
	ObjectClass objectClass;
	
	Demonstration demonstration; // The demonstration that evoked this property
	
	// The timestamps its utterances encompass with the associated PerceptClusters
	HashMap<Long, PerceptCluster> perceptClusters;
	List<BlobChain> resolvedBlobChains;
	String className;
	String objectId;
	boolean blobsResolved;
	BlobChain currentBlobChain;
	private boolean isFocus;
	private int positionIndex;
	
	// The utterances where it has been mentioned
	// Note that this only covers the introduction of the object into discourse and any
	// co-reference from the parser
	private ArrayList<Utterance> utterances;
	
	public WorldObject(String className, String objectId, List<String> properties)
	{
		this.properties = new HashSet<String>(properties);
		this.negatedProperties = new HashSet<String>();
		this.perceivedProperties = new HashSet<String>();
		this.perceivedWords = new HashSet<String>();
		this.negatedObjectClasses = new HashSet<ObjectClass>();
		this.perceptClusters = new HashMap<Long, PerceptCluster>();
		this.utterances = new ArrayList<Utterance>();
		this.resolvedBlobChains = new ArrayList<BlobChain>();
		this.className = className;
		this.objectId = objectId;
		blobsResolved = false;
		setFocus(false);
		positionIndex = -1;
		objectClass = null;
	}
	
	public WorldObject(String objectId, String replyWith)
	{
		this(ObjectClass.nullClass,objectId, new ArrayList<String>());
	}
	
	public WorldObject()
	{
		this(ObjectClass.defaultClass, "null", new ArrayList<String>());
	}
	
	public WorldObject(WorldObject toCopy, int copyNumber)
	{
		this(toCopy.getClassName(), toCopy.getObjectId() + "_" + copyNumber, 
				new ArrayList<String>(toCopy.getProperties()));
	}
	
	/*
	 * Take in the WorldObject generated by a DESCRIBED message or a repeat DEMONSTRATED
	 * message to add information about the object
	 */
	public void mergeWorldObject(WorldObject wo)
	{
		properties.addAll(wo.getProperties());
		perceivedProperties.addAll(wo.getPerceivedProperties());
		for (long timestamp : wo.getPerceptClusters().keySet())
		{
			perceptClusters.put(timestamp, wo.getPerceptClusters().get(timestamp));
		}
		
		utterances.addAll(wo.getUtterances());
		updateUtterances();
	}
	
	public void addFeatureDataToWorldManager()
	{
		WorldManager wm = demonstration.getUtterance().getSession().getWorldManager();
		switch (demonstration.getDemonstrationType())
		{
		case DEMONSTRATED:
			wm.addDemonstratedObject(this);
			break;
		case RELATE:
			wm.addDemonstratedObject(this);
			break;
		case DESCRIBED:
			wm.addDescription(this);
			break;
		case MENTIONED:
			wm.addMentionedObject(this);
			break;
		case QUERY:
			break;
		}
	}
	
	public void addUtterance(Utterance u)
	{
		utterances.add(u);
		u.addWorldObject(this);
	}
	
	public void addProperty(Property p)
	{
		properties.add(p.getName());
		p.addExample(this);
	}
	
	public void addNegatedProperty(Property p)
	{
		negatedProperties.add(p.getName());
	}
	
	public void setObjectClass(ObjectClass oc)
	{
		if (objectClass != null)
			objectClass.removeExample(this);
		
		className = oc.getName();
		
	
		oc.addExample(this);
	}
	
//	public void setClassName(ObjectClass oc)
//	{	
//		className = oc.getName();
//	
//		oc.addExample(this);
//	}
	
	/*
	 * Make sure each Utterance contains this WorldObject
	 */
	public void updateUtterances()
	{
		for (Utterance u : utterances)
		{
			u.addWorldObject(this);
		}
	}

	public HashSet<String> getProperties() {
		return properties;
	}

	public HashMap<Long, PerceptCluster> getPerceptClusters() {
		
		return perceptClusters;
	}

	public String getClassName() {
		return className;
	}
	
	public String getClassNameWord() 
	{
		System.out.println(className);
		if (className.equals(ObjectClass.nullClass) || className.length() == 0 || 
				className.split("W::").length < 2)
			return "object";
		return className.split("W::")[1].replace(")", "");
	}
	
	public void setClassName(String className)
	{
		this.className = className;
		// We only have text now, so set objectClass to null
		if (objectClass != null && 
				objectClass.getName().equals(className))
			objectClass = null;
			
	}

	public List<String> getPlainTextDescription()
	{
		List<String> description = new ArrayList<String>();
		for (String propertyString : getPerceivedProperties())
		{

			String propertyWord = propertyString.split(" ")[2].split("::")[1].split("\\)")[0].trim().toLowerCase();
			if (propertyWord.endsWith("s"))
				propertyWord = propertyWord.substring(0, propertyWord.length()-1);
			description.add(propertyWord);
		}
		description.add(getClassNameWord().toLowerCase());
		
		return description;
	}
	
	public String getObjectId() {
		return objectId;
	}

	public HashSet<String> getPerceivedProperties() {
		return perceivedProperties;
	}
	
	public String toString()
	{
		String result = "Class name: " + className + "\n" + 
				"ID: " + objectId + "\n";
		result += "Properties: \n";
		for (String propertyString: properties)
			result += "  " + propertyString + "\n";
		result += "Perceived Properties: \n";
		for (String perceivedPropertyString: perceivedProperties)
			result += "  " + perceivedPropertyString + "\n";
		
		return result;
	}

	public ArrayList<Utterance> getUtterances() {
		return utterances;
	}
	
	/**
	 * Updates percept clusters to attended (whatever the person's hand is closest to.
	 * These percept clusters are determined during feature extraction. Also adds 
	 * the WorldObject's properties and class name to the WorldManager.
	 */
	public void updatePerceptClustersToAttended()
	{
		for (Utterance u: utterances)
		{
			perceptClusters.putAll(u.getAttendedPerceptClusters());
		}
		blobsResolved = true;
		addFeatureDataToWorldManager();
	}
	
	public Map<FeatureSpace,DoubleMatrix> getAveragePerceptValues()
	{
		//updatePerceptClusters();
		return PerceptCluster.getAveragePerceptCluster(perceptClusters.values());
	}
	
	/*
	 * Set this WorldObject's PerceptClusters to the those in the given BlobChain
	 */
	public void setPerceptClustersToBlobChain(BlobChain bc)
	{
		tempSetPerceptClustersToBlobChain(bc);
		addFeatureDataToWorldManager();
	}
	
	public void setPerceptClustersToCurrentBlobChain()
	{
		perceptClusters = currentBlobChain.getTimestampedPerceptClusters();
		blobsResolved = true;
		for (PerceptCluster pc : perceptClusters.values())
		{
			for (Entry<FeatureSpace, Percept> entry : pc.getPercepts().entrySet())
			{
				FeatureSpace fs = entry.getKey();
				Percept p = entry.getValue();
				
				fs.addUnlabeledData(p.getValue());
			}
		}
		blobsResolved = true;
		addFeatureDataToWorldManager();
	}
	
	/**
	 * Before the correct labels have been identified for this WorldObject, set
	 * temporary percept clusters so that we can assign labels later, while making 
	 * sure that we don't create objects that have the same feature data.
	 * @param bc The BlobChain that we want to temporarily assign.
	 */
	public void tempSetPerceptClustersToBlobChain(BlobChain bc)
	{
		perceptClusters = bc.getTimestampedPerceptClusters();
		blobsResolved = true;
		currentBlobChain = bc;
		for (PerceptCluster pc : perceptClusters.values())
		{
			for (Entry<FeatureSpace, Percept> entry : pc.getPercepts().entrySet())
			{
				FeatureSpace fs = entry.getKey();
				Percept p = entry.getValue();
				
				fs.addUnlabeledData(p.getValue());
			}
		}
	}
	
	/*
	 * Set this WorldObject's PerceptClusters to the those in the BlobChains
	 */
	public void setPerceptClustersToBlobChains(List<BlobChain> bcList)
	{
		
		perceptClusters = new HashMap<Long,PerceptCluster>();
		for (BlobChain bc : bcList)
		{
			perceptClusters.putAll(bc.getTimestampedPerceptClusters());
		}
		blobsResolved = true;
		addFeatureDataToWorldManager();
	}
	
	public double getMinimumPerceptualDistance(WorldObject other)
	{
		Map<FeatureSpace,DoubleMatrix> woPerceptValues = getAveragePerceptValues();
		Map<FeatureSpace,DoubleMatrix> otherPerceptValues = other.getAveragePerceptValues();
		double minDistance = Double.MAX_VALUE;
		for (FeatureSpace fs : woPerceptValues.keySet())
		{
			double distance = fs.getSquaredMahalanobisDistance(woPerceptValues.get(fs), 
												otherPerceptValues.get(fs));
			
			if (distance < minDistance)
				minDistance = distance;
		}
		
		return minDistance;
	}
	
	public void addResolvedBlobChain(BlobChain bc)
	{
		resolvedBlobChains.add(bc);
	}

	public List<BlobChain> getResolvedBlobChains() {

		return resolvedBlobChains;
	}
	
	/**
	 * Returns the closest blobs to the user's hand in each utterance. This does 
	 * not do any processing of spatial continuity - so blobs might not belong
	 * to the same blob chain.
	 * @return
	 */
	public List<Blob> getClosestBlobToHandListInAllUtterances()
	{
		List<Blob> blobs = new ArrayList<Blob>();
		for (Utterance u : utterances)
		{
			TreeMap<Long,BlobGroup> blobGroups = 
					u.getSession().getBlobGroupsForUtterance(u);
			for (BlobGroup bg : blobGroups.values())
			{
				blobs.add(bg.getClosestBlobToHand());
			}
			
		}
		return blobs;
	}

	public HashSet<String> getNegatedProperties() {
		return negatedProperties;
	}

	public HashSet<String> getPerceivedWords() {
		return perceivedWords;
	}
	
	public void addNegatedClass(ObjectClass oc)
	{
		negatedObjectClasses.add(oc);
	}
	
	public HashSet<ObjectClass> getNegatedClasses()
	{
		return negatedObjectClasses;
	}

	public FeatureManager getSession()
	{
		return demonstration.getUtterance().getSession();
	}
	
	public void setDemonstration(Demonstration d)
	{
		this.demonstration = d;
	}
	
	public Demonstration getDemonstration()
	{
		return demonstration;
	}

	public BlobChain getCurrentBlobChain() {
		return currentBlobChain;
	}

	public int getPositionIndex() {
		return positionIndex;
	}

	public void setPositionIndex(int positionIndex) {
		this.positionIndex = positionIndex;
	}

	public boolean isFocus() {
		return isFocus;
	}

	public void setFocus(boolean isFocus) {
		if (utterances.size() > 0)
			System.out.println("WorldObject in utterance " + utterances.get(0) + " at " + currentBlobChain.getAverageX() + " set to " + isFocus);
		else if (currentBlobChain != null)
			System.out.println("WorldObject " + className + " at " + currentBlobChain.getAverageX() + " set to " + isFocus);
		
		this.isFocus = isFocus;
	}
	
	// TODO: Clear this object from examples in properties too
	public void clearProperties()
	{

	}
	
	public int getMedianSubblobs()
	{
		List<Integer> subblobNumbers = new ArrayList<Integer>();
		for (Blob b: getClosestBlobToHandListInAllUtterances())
		{
			if (b != null)
				subblobNumbers.add(b.getSubBlobs().size());
		}
		Collections.sort(subblobNumbers);
	
	
	return subblobNumbers.get(subblobNumbers.size() / 2);
	}
	
}