package owlground;

import java.io.*;
import java.util.*;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.StartElement;

import org.jblas.DoubleMatrix;

import owlground.language.Utterance;
import owlground.perception.Blob;
import owlground.perception.BlobGroup;
import owlground.perception.Percept;
import owlground.perception.PerceptCluster;
import owlground.spaces.FeatureSpace;
import owlground.spaces.SpaceManager;
import owlground.utilities.Conversions;
/*
 * Loads feature data and transcript files. Generates PerceptClusters.
 * Currently it's assumed that we will load the Utterances first,
 * then bring in the feature data and map them. 
 */
public class FeatureManager {
	
	WorldManager worldManager;
	SpaceManager spaceManager;
	// A mapping from Unix timestamps in long form to the percept cluster at that time
	HashMap<Long,PerceptCluster> attendedPerceptClusters;
	
	// A mapping from Unix timestamps in long form to the blob group at that time
	HashMap<Long,BlobGroup> blobGroups;
	
	// Blobgroup used when reading in data
	BlobGroup currentBlobGroup;
	
	// Used to keep track of multiple sessions - one FeatureManager per session
	private int sessionId;
	private static int nextSessionId = 0;
	private static boolean USE_HAND_DISTANCE = false;
	
	// Used for storing the utterances that have an uttnum from the Utterance message
	HashMap<Integer,Utterance> numberedUtterances;
	
	// Mapping from words to the set of co-occurring percept clusters
	HashMap<String, HashSet<PerceptCluster>> wordPerceptClusters;
	
	// The start time of the transcript if timestamps are starting at 0
	private long globalStartTime = 0;
	
	private List<String> perceptNames;
	
	private String[] featuresToIgnore = {};
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("features.usehanddistance"))
			USE_HAND_DISTANCE = Boolean.parseBoolean(properties.get("features.usehanddistance"));
	}
	
	
	public WorldManager getWorldManager() {
		return worldManager;
	}

	public void setWorldManager(WorldManager worldManager) {
		this.worldManager = worldManager;
	}

	public int getSessionId() {
		return sessionId;
	}

	@Deprecated
	public HashMap<Long, PerceptCluster> getAttendedPerceptClusters() {
		return attendedPerceptClusters;
	}

	public long getGlobalStartTime() {
		return globalStartTime;
	}

	public void setGlobalStartTime(long startTime) {
		this.globalStartTime = startTime;
	}
	
	public List<String> getPerceptNames()
	{
		if (perceptNames != null)
			return Collections.unmodifiableList(perceptNames);
		for (PerceptCluster pc : attendedPerceptClusters.values())
		{
			if (pc.getPercepts().keySet().size() > 0)
			{
				perceptNames = new ArrayList<String>(pc.getPerceptsByName().keySet());
				return Collections.unmodifiableList(perceptNames);
			}
		}
		
		return null;
	}
	
	public HashMap<Long,PerceptCluster> getAttendedPerceptClustersForUtterance(int uttNum)
	{
		HashMap<Long,PerceptCluster> result = new HashMap<Long,PerceptCluster>();

		Utterance u = numberedUtterances.get(uttNum);
		System.out.println(blobGroups.size() + " blobGroups");
		for (long timestamp : blobGroups.keySet())
		{
			if (timestamp >= u.getStartTime() && timestamp <= u.getEndTime())
			{
				if (blobGroups.get(timestamp).getClosestBlobToHand() != null)
					result.put(timestamp, blobGroups.get(timestamp).getClosestBlobToHand().getPerceptCluster());
		
			}
		}
		return result;
	}
	
	public HashMap<Long, PerceptCluster> getPerceptClustersForUtterance(Utterance u)
	{
		HashMap<Long,PerceptCluster> result = new HashMap<Long,PerceptCluster>();
		
		if (!USE_HAND_DISTANCE)
			return result;
		
		for (long timestamp : blobGroups.keySet())
		{
			if (timestamp >= u.getStartTime() && timestamp <= u.getEndTime())
				result.put(timestamp, blobGroups.get(timestamp).getClosestBlobToHand().getPerceptCluster());			
		}
		
		return result;
	}
	
	public TreeMap<Long,BlobGroup> getBlobGroupsForUtterance(int uttNum)
	{
		TreeMap<Long,BlobGroup> result = new TreeMap<Long,BlobGroup>();
		Utterance u = numberedUtterances.get(uttNum);
		for (long timestamp : blobGroups.keySet())
		{
			if (timestamp >= u.getStartTime() && timestamp <= u.getEndTime())
				result.put(timestamp, blobGroups.get(timestamp));
		}
		
		return result;
	}
	
	
	
	public TreeMap<Long, BlobGroup> getBlobGroupsForUtterance(Utterance u)
	{
		TreeMap<Long,BlobGroup> result = new TreeMap<Long,BlobGroup>();
		for (long timestamp : blobGroups.keySet())
		{
			if (timestamp >= u.getStartTime() && timestamp <= u.getEndTime())
				result.put(timestamp, blobGroups.get(timestamp));			
		}
		
		return result;
	}

	public FeatureManager(SpaceManager spaceManager)
	{
		this.spaceManager = spaceManager;
		attendedPerceptClusters = new HashMap<Long,PerceptCluster>();
		currentBlobGroup = new BlobGroup();
		blobGroups = new HashMap<Long, BlobGroup>();
		wordPerceptClusters = new HashMap<String, HashSet<PerceptCluster>>();
		numberedUtterances = new HashMap<Integer, Utterance>();
		sessionId = nextSessionId;
		nextSessionId++;
	}
	
	/*
	 * Currently this maps words to PerceptClusters. We won't use this with
	 * the properties and objects we get from messages. But the general 
	 * idea is the same.
	 */
	public void generateWordPerceptClusters()
	{
		Long [] timestamps = attendedPerceptClusters.keySet().toArray(new Long[0]);
		Arrays.sort(timestamps);
		int utteranceIndex = 0;
		for (long timestamp : timestamps)
		{
			Utterance u = numberedUtterances.get(utteranceIndex);
			
			if (timestamp < u.getStartTime())
				continue;
			
			while (timestamp >= u.getEndTime())
			{
				utteranceIndex++;
			
				if (utteranceIndex >= numberedUtterances.size())
					break;
				
				u = numberedUtterances.get(utteranceIndex);
			}
			
			String [] words = u.getWords();
			for (String word: words)
			{
				String strippedWord = word.replaceAll("[^A-Za-z]","");
				if (!wordPerceptClusters.containsKey(strippedWord.toLowerCase()))
					wordPerceptClusters.put(strippedWord.toLowerCase(), new HashSet<PerceptCluster>());
				wordPerceptClusters.get(strippedWord.toLowerCase()).add(attendedPerceptClusters.get(timestamp));
			}
			
		}
		
	}
	
	public Utterance getUtteranceAtTimestamp(long timestamp)
	{
		for (Utterance u : numberedUtterances.values())
		{
			if (timestamp >= u.getStartTime() && timestamp <= u.getEndTime())
				return u;
		}
		return null;
	}
	
	public Utterance getUtterance(int uttNum)
	{
		if (!numberedUtterances.containsKey(uttNum))
		{
			System.out.println("No utterance number " + uttNum);
			System.out.println("Number of utterances: " + numberedUtterances.size());
		}
		return numberedUtterances.get(uttNum);
	}
	

	
	public DoubleMatrix getColorForWord(String word)
	{
		int instances = 0;
		DoubleMatrix totalColor = DoubleMatrix.zeros(3);
		
		for (PerceptCluster pc : wordPerceptClusters.get(word.toLowerCase()))
		{
			
			HashMap<FeatureSpace, Percept> percepts = pc.getPercepts();

			DoubleMatrix color = percepts.get("rgb").getValue();
			if (color == null || color.isNaN().sum() > 0.0f || color.sum() < 0.1f)
				continue;

			instances++;
			totalColor.addi(color);
		}
		totalColor.divi(instances);

		return totalColor;
	}
	
	public void loadTranscriptFile(String file) throws IOException
	{
		InputStream in = new FileInputStream(file);
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				
			    while ((line = reader.readLine()) != null) {
			    	String[] lineSplit = line.split("\\s+");
			    	
			    	long startTime = (long)(Double.parseDouble(lineSplit[0]) * 100);
			    	
			    	long endTime = (long)(Double.parseDouble(lineSplit[1]) * 100);
			    	numberedUtterances.put(numberedUtterances.size(),new Utterance(this, Arrays.copyOfRange(lineSplit, 2, lineSplit.length),
			    									startTime,endTime, globalStartTime));
			    	
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open " + file.toString() + " for reading.");
			} finally {
				if (in != null)
					in.close();
			}
	}

	/**
	 * Given an XMLStreamReader pointing at a start tag, advance it until it
	 * points to the corresponding end tag. Assumes there are no nested
	 * elements of the same name.
	 */
	protected void skipElement(XMLStreamReader r) throws XMLStreamException {
		String name = r.getLocalName();
		while (r.hasNext()) {
			if (r.next() == XMLStreamConstants.END_ELEMENT &&
				r.getLocalName().equals(name))
				break;
		}
	}

	/**
	 * Given an XMLStreamReader pointing at an end tag, throw a
	 * RuntimeException if its name doesn't match.
	 */
	public static void assertEndTag(XMLStreamReader r, String name)
	{
		if (!r.getLocalName().equals(name))
			throw new RuntimeException("unexpected end tag </" + r.getLocalName() + ">");
	}

	/**
	 * Read a &lt;spaces&gt; element, making Space objects and adding them to
	 * spaceManager.
	 */
	protected void readSpaces(XMLStreamReader r) throws XMLStreamException
	{
		while (r.hasNext()) {
			switch (r.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					try {
						spaceManager.addFeatureSpace(FeatureSpace.fromXML(r));
					} catch (RuntimeException e) {
						System.err.println("error reading space");
						e.printStackTrace();
					}
					break;
				case XMLStreamConstants.END_ELEMENT:
					assertEndTag(r, "spaces");
					return;
			}
		}
	}

    /**
	 * Read a &lt;frame&gt; element, making Blobs, PerceptClusters, and
	 * Percepts corresponding to &lt;blob&gt;s, &lt;features&gt;', and their
	 * children. Frames seen outside an utterance are ignored. Otherwise,
	 * returns true if any blobs are read.
	 */
	protected boolean readFrame(XMLStreamReader r) throws XMLStreamException
	{
		// Convert the doubles to longs, because I'm anxious about hashing with doubles
		long timestamp = (long)(Double.parseDouble(r.getAttributeValue(null,"timestamp")) * 100.0);
		Utterance u = getUtteranceAtTimestamp(timestamp);
		if (u == null)
		{
			skipElement(r);
			return false;
		} else {
			boolean blobsRead = false;
			while (r.hasNext()) {
				switch (r.nextTag()) {
					case XMLStreamConstants.START_ELEMENT:
						if (r.getLocalName().equals("blobs"))
							blobsRead = readBlobs(r, u, timestamp);
						else // ignore other children: skeleton, features
							skipElement(r);
						break;
					case XMLStreamConstants.END_ELEMENT:
						assertEndTag(r, "frame");
						return blobsRead;
				}
			}
		}
		throw new RuntimeException("unterminated <frame> element");
	}

	/**
	 * Read a &lt;blobs&gt; element, adding its &lt;blob&gt; children to blobs.
	 * Returns true if there were any blobs read.
	 */
	protected boolean readBlobs(XMLStreamReader r, Utterance u, long timestamp) throws XMLStreamException
	{
		boolean blobsRead = false;
		PerceptCluster pc = null;
		Blob blob = null;
		Blob subBlob = null;
		while (r.hasNext()) {
			switch (r.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					if (r.getLocalName().equals("blob"))
					{
						pc = new PerceptCluster(u, timestamp);
						blob = Blob.fromXML(r, pc);
						currentBlobGroup.addBlob(blob);
						blobsRead = true;
					}
					if (r.getLocalName().equals("subblob"))
					{
						pc = new PerceptCluster(u, timestamp);
						subBlob = Blob.fromXML(r, pc);
						blob.addSubBlob(subBlob);
					}
					else if (r.getLocalName().equals("features"))
					{
						if (pc == null)
							throw new RuntimeException("unexpected <features> element in <blobs>");
						readFeatures(r, pc);
					}
					/* ignore other children */
					break;
				case XMLStreamConstants.END_ELEMENT:
					if (r.getLocalName().equals("blobs"))
					{
						return blobsRead;
					}
					else if (r.getLocalName().equals("blob") || 
							r.getLocalName().equals("subblob"))
					{
						//pc = null;
					}
					else if (r.getLocalName().equals("subblobs"))
					{
						
					}
					else
					{
						throw new RuntimeException("unexpected end tag </" + r.getLocalName() + ">");
					}
					break;
			}
		}
		throw new RuntimeException("unterminated <blobs> element");
	}

	/**
	 * Read a &lt;features&gt; element, adding its children as Percepts to the
	 * given PerceptCluster.
	 */
	protected void readFeatures(XMLStreamReader r, PerceptCluster pc) throws XMLStreamException
	{
		while (r.hasNext()) {
			begincase:
			switch (r.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					for (String featureToIgnore : featuresToIgnore)
					{
						if (r.getLocalName().toLowerCase().equals(featureToIgnore))
						{
							readFeature(r,pc);
							break begincase;
						}
					}
							
					pc.addPercept(readFeature(r, pc));
					break;
				case XMLStreamConstants.END_ELEMENT:
					assertEndTag(r, "features");
					return;
			}
		}
		throw new RuntimeException("unterminated <features> element");
	}

	/**
	 * Read a feature element (a child of &lt;features&gt;), returning a Percept.
	 */
	protected Percept readFeature(XMLStreamReader r, PerceptCluster pc) throws XMLStreamException
	{
		String spaceName = r.getLocalName().toLowerCase();
		//System.out.println("Looking for space " + spaceName);
		DoubleMatrix featureData = new DoubleMatrix(spaceManager.getFeatureSpaceByName(spaceName).getDimension());
		int featureIndex = 0;
		while (r.hasNext()) {
			switch (r.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
				{
					// Hack to replace rgb_prob with rgb if no value
					String elementText = r.getElementText();
					double featureValue = 0f;
					if (elementText.equals("nan") || elementText.equals("inf"))
						//featureValue = pc.getPercepts().get("rgb").getValue().get(featureIndex);
						featureValue = 0.0d;
					else
						featureValue = Double.parseDouble(elementText);
					
					featureData.put(featureIndex, featureValue);
					featureIndex++;
					break;
				}
				case XMLStreamConstants.END_ELEMENT:
					if (r.getLocalName().equalsIgnoreCase(spaceName))
					{
						if (spaceName.equals("rgb"))
							return new Percept(spaceName,Conversions.convertRgbToLab2(featureData),pc);
						return new Percept(spaceName, featureData, pc);
					}
					/* ignore other end elements */
			}
		}
		throw new RuntimeException("unterminated <" + spaceName + "> element");
	}
	
	public void readXmlStream(InputStream in) throws XMLStreamException
	{
		XMLInputFactory factory = XMLInputFactory.newInstance();
		XMLStreamReader r = null;

		r = factory.createXMLStreamReader(in);
	
		while (r.hasNext())
		{
			if (r.nextTag() == XMLStreamConstants.START_ELEMENT)
			{
				if (r.getLocalName().equals("spaces"))
					readSpaces(r);
				else if (r.getLocalName().equals("frame"))
				{
					if (readFrame(r))
					{
						//perceptClusters.put(timestamp, pc);
						// Choose the best blob from those seen so far
						Blob closestBlob = currentBlobGroup.getClosestBlobToHand();
						blobGroups.put(currentBlobGroup.getTimestamp(), currentBlobGroup);
						if (closestBlob != null)
						{
							PerceptCluster pc = closestBlob.getPerceptCluster();
							attendedPerceptClusters.put(pc.getTimestamp(), pc);
						}
					}
					/*
					 * Reset blobs, as we're in a different frame now
					 */
					currentBlobGroup = new BlobGroup();
					
				}
			}
			else if (r.getLocalName().equals("session"))
			{
				break;
			}
		}
		r.close();
		//in.close();
		//System.out.println(perceptClusters);		
	}
	
	public void loadInputXmlFile(String file) throws XMLStreamException, IOException
	{
		InputStream in = null;
		in = new FileInputStream(file);
		
		readXmlStream(in);
		in.close();
	}


	public void storeUtterance(Utterance u)
	{
		numberedUtterances.put(u.getUttNum(), u);
	}

	public TreeMap<Integer, Utterance> getNumberedUtterances() {
		return new TreeMap<Integer, Utterance>(numberedUtterances);
	}

	public int getNumberOfUtterances()
	{
		return numberedUtterances.size();
	}

	public SpaceManager getSpaceManager() {
		return spaceManager;
	}
}
