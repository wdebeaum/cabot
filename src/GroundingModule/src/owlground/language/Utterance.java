package owlground.language;


import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.jblas.DoubleMatrix;

import owlground.FeatureManager;
import owlground.objects.WorldObject;
import owlground.perception.BlobGroup;
import owlground.perception.BlobGroupChain;
import owlground.perception.PerceptCluster;
import owlground.reference.ReferenceTester;
import owlground.spaces.FeatureSpace;

/*
 * Stores the words of an utterance and the interval over which it is said.
 * Words won't be used anymore.
 */
public class Utterance {
	
	private String[] words;
	private long startTime = -1;
	private long endTime = -1;
	private int uttNum = -1;
	private FeatureManager session;
	private Set<WorldObject> worldObjects;
	private BlobGroupChain blobGroupChain;
	private List<Integer> groundTruthReferences;
	
	public int getUttNum() {
		return uttNum;
	}

	public Utterance(FeatureManager session, String[] words, long startTime, long endTime, long globalStartTime)
	{
		this.words = words;
		this.startTime = startTime + globalStartTime;
		this.endTime = endTime + globalStartTime;
		this.session = session;
		this.blobGroupChain = null;
		worldObjects = new HashSet<WorldObject>();
	}
	
	public Utterance(FeatureManager session, String[] words, long startTime, long endTime, 
			long globalStartTime, int uttNum)
	{
		this(session,words,startTime,endTime,globalStartTime);
		this.uttNum = uttNum;
		
	}
	
	public Utterance(FeatureManager session, int uttNum)
	{
		this(session,new String[0],-1,-1,0);
		this.uttNum = uttNum;
	}
	
	public Set<WorldObject> getWorldObjects() {
		return worldObjects;
	}

	public void addWorldObject(WorldObject worldObject) {
		worldObjects.add(worldObject);
	}

	public String[] getWords() {
		return words;
	}
	
	public void setStartTime(long startTime)
	{
		this.startTime = startTime;
	}
	
	public void setEndTime(long endTime)
	{
		this.endTime = endTime;
	}
	
	public void setWords(String[] words) {
		this.words = words;
	}

	public long getStartTime() {
		return startTime;
	}

	public long getEndTime() {
		return endTime;
	}
	
	public String toString()
	{
		String toReturn = "Session: " + session.getSessionId() + "\n"; 
		toReturn += "uttnum: " + uttNum + " " + startTime + " " + endTime + "\n";
		for (String word : words)
			toReturn += word + " ";
		toReturn += "\n";
		return toReturn;
	}
	
	public DoubleMatrix getAverageValueForFeature(String featureName)
	{
		DoubleMatrix average = null;
		int numPerceptClusters = 0;
		for (PerceptCluster pc : getAttendedPerceptClusters().values())
		{
			if (!pc.getPercepts().containsKey(featureName))
				continue;
			
			if (average == null)
				average = DoubleMatrix.zeros(pc.getPercepts().get(featureName).getValue().length);
			
			average.addi(pc.getPercepts().get(featureName).getValue());
			numPerceptClusters++;
		}
		if (average == null)
			return null;
		
		return average.div(numPerceptClusters);
	}
	
	public TreeMap<FeatureSpace, DoubleMatrix> getAverageFeatureValues()
	{
		// Use treemap to sort by feature name
		TreeMap<FeatureSpace, DoubleMatrix> averages = 
				new TreeMap<FeatureSpace, DoubleMatrix>(PerceptCluster.getAveragePerceptCluster(getAttendedPerceptClusters().values()));
		
		return averages;
	}
	
	public TreeMap<Long,BlobGroup> getBlobGroups()
	{
		return session.getBlobGroupsForUtterance(uttNum);
	}
	
	public Map<Long,PerceptCluster> getAttendedPerceptClusters()
	{
		return session.getAttendedPerceptClustersForUtterance(uttNum);
	}
	
	public Map<WorldObject,Map<Long,PerceptCluster>> getAllPerceptClusters()
	{
		HashMap<WorldObject, Map<Long,PerceptCluster>> result = 
					new HashMap<WorldObject, Map<Long,PerceptCluster>>();
		
		for (WorldObject wo : getWorldObjects())
		{
			result.put(wo, wo.getPerceptClusters());
		}
		return result;
	}
	
	public BlobGroupChain getBlobGroupChain()
	{
		if (blobGroupChain == null)
			blobGroupChain = new BlobGroupChain(this, null);
		
		return blobGroupChain;
		
	}
	
	public boolean equals(Utterance u)
	{
		return (this.startTime == u.startTime && this.endTime == u.endTime);
	}

	public FeatureManager getSession() {
		return session;
	}
	
	// Labels the word list for the parse data (objects and properties)
	public String[] getLabeledWords()
	{
		String[] labeledWords = new String[words.length];
		for (int i = 0; i < words.length; i++)
		{
			labeledWords[i] = words[i];
			for (WorldObject wo : worldObjects)
			{
				if (words[i].equalsIgnoreCase(wo.getClassNameWord()))
					labeledWords[i] = words[i] + ":o";
				
				for (String property : wo.getProperties())
				{
					if (words[i].equalsIgnoreCase(property.split("W::")[1].replace(")","")))
					{
						labeledWords[i] = words[i] + ":p";
						break;
					}
				}
				
			}
		}
		
		return labeledWords;
	}

	public List<Integer> getGroundTruthReferences() {
		return groundTruthReferences;
	}

	public void setGroundTruthReferences(List<Integer> groundTruthReferences) {
		this.groundTruthReferences = groundTruthReferences;
	}
	
	public boolean isGroundTruth()
	{
		return ReferenceTester.matchesGroundTruth(this);
	}
	
	public Set<WorldObject> groundTruthResult()
	{
		return ReferenceTester.groundTruthReferenceObjects(this);
	}
	
}
