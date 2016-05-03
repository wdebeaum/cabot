package owlground.language;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import owlground.WorldManager;
import owlground.objects.WorldObject;
import owlground.objects.WorldObjectGroup;
import owlground.perception.BlobChain;
import owlground.perception.BlobGroupChain;
import owlground.reference.LatticeLearner;
import owlground.reference.PartitionGenerator;
import owlground.reference.ReferenceLattice;



public class Demonstration {
	public enum ImplicationType { OBJECT_UNIQUENESS, DOBJECT_UNIQUENESS, OBJECT_EXISTENTIAL, DOBJECT_EXISTENTIAL, NONE }
	public enum DemonstrationType { DEMONSTRATED, MENTIONED, DESCRIBED, QUERY, RELATE}
	
	protected Utterance utterance;
	protected List<WorldObjectGroup> worldObjectGroups;
	protected BlobGroupChain blobGroupChain;
	protected boolean worldObjectsResolved;
	protected WorldObjectGroup focusWorldObjectGroup;
	protected boolean useLatticeResolution;
	protected boolean blobsAssigned;
	protected boolean missingFeatureData;
	protected DemonstrationType demonstrationType;
	private ReferenceLattice lattice;
	
	private final int MIN_FRAMES = 4;
	
	private static boolean USE_HAND_DISTANCE = false;
	private static boolean LAX_HAND_DISTANCE = false;
	private static boolean USE_GROUND_TRUTH = false;
	private static boolean PRECLASSIFY_BLOB_CHAINS = false;
	
	public Demonstration(Utterance u, DemonstrationType demonstrationType)
	{
		this.utterance = u;
		worldObjectGroups = new ArrayList<WorldObjectGroup>();
		this.worldObjectsResolved = false;
		this.useLatticeResolution = true;
		blobsAssigned = false;
		this.demonstrationType = demonstrationType;
		missingFeatureData = false;
	}
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("features.usehanddistance"))
			USE_HAND_DISTANCE = Boolean.parseBoolean(properties.get("features.usehanddistance"));
		if (properties.containsKey("reference.usegroundtruth"))
			USE_GROUND_TRUTH = Boolean.parseBoolean(properties.get("reference.usegroundtruth"));
		if (properties.containsKey("reference.preclassifyblobchains"))
			PRECLASSIFY_BLOB_CHAINS = Boolean.parseBoolean(properties.get("reference.preclassifyblobchains"));
		if (properties.containsKey("reference.laxhanddistance"))
			LAX_HAND_DISTANCE = Boolean.parseBoolean(properties.get("reference.laxhanddistance"));
	}
	
	public void addWorldObject(WorldObject wo)
	{
		addWorldObjectGroup(new WorldObjectGroup(wo));
		utterance.addWorldObject(wo);
		wo.setDemonstration(this);
		this.worldObjectsResolved = false;
		blobsAssigned = false;
	}
	
	public void addWorldObjectGroup(WorldObjectGroup wog)
	{
		worldObjectGroups.add(wog);
		for (WorldObject wo : wog.getWorldObjects())
			utterance.addWorldObject(wo);
		if (wog.isFocus())
			focusWorldObjectGroup = wog;
		this.worldObjectsResolved = false;
		blobsAssigned = false;
	}
	
	/*
	 * Assigns all of the WorldObjects to BlobIDs by resolving the BlobGroupChain and finding
	 * the appropriate BlobChain for each object
	 */
	public boolean assignWorldObjectsToBlobIds()
	{
		if (blobsAssigned)
			return true;
		
		WorldManager worldManager = utterance.getSession().getWorldManager();
		
		blobGroupChain = new BlobGroupChain(utterance,this);
		System.out.println("Utterance: " + utterance.hashCode());
		System.out.println("Blob group chain size before pruning: " + blobGroupChain.size());
		// Create consistent blob id's
		blobGroupChain.resolveBlobIds();
		System.out.println("Blob group chain size after resolve: " + blobGroupChain.size());
		
		blobGroupChain.pruneShortBlobChains(MIN_FRAMES);
		
		System.out.println("Blob group chain size after pruning: " + blobGroupChain.size());
		
		if (blobGroupChain.size() == 0)
		{
			missingFeatureData = true;
			return false;
		}
		
		// If there are more chains than WorldObjects mentioned, we create implicit WOs
		generateBlankWorldObjects();
		
		List<WorldObject> worldObjectList = new ArrayList<WorldObject>(getNonNullWorldObjects());

		System.out.println("Number of objects: " + worldObjectList.size());
		System.out.println("Number of blobs: " + blobGroupChain.size());
		int objectIndex = 0;
		// There seems to be a bug in Java util where the blobChains weren't removed correctly
		for (Entry<Integer,BlobChain> entry : blobGroupChain.getBlobChains().entrySet())
		{
			System.out.println("BlobGroupChain: " + entry.getKey() + " " + entry.getValue());
			if (entry.getValue().getAveragePerceptValues().keySet().size() == 0)
				throw new RuntimeException();
			worldObjectList.get(objectIndex).tempSetPerceptClustersToBlobChain(entry.getValue());
			objectIndex++;
		}
		
//		if (objectIndex < worldObjectList.size())
//		{
//			missingFeatureData = true;
//			return false;
//		}
		
		blobsAssigned = true;
		
		return true;
	}
	
	/**
	 * Generate WorldObjects representing objects seen but not mentioned.
	 */
	private void generateBlankWorldObjects()
	{
		int numberOfOtherObjects = blobGroupChain.size() - getNonNullWorldObjects().size();
		WorldObjectGroup others = new WorldObjectGroup();
		for (int i = 0; i < numberOfOtherObjects; i++)
		{
			WorldObject temp = new WorldObject();
			others.addWorldObject(temp);
			temp.setDemonstration(this);
			temp.addUtterance(utterance);
		}
		
		worldObjectGroups.add(others);
	}

	/**
	 * If LAX_HAND_DISTANCE is set, sets all of the focus WorldObjects in the scene to the blob 
	 * closest to the person's hand. Otherwise, it sets the focus world objects to distinct
	 * blob chains that are closest to the hand.
	 */
	public void resolveWithHandDistance()
	{
		if (LAX_HAND_DISTANCE)
		{
			for (WorldObject wo: focusWorldObjectGroup.getWorldObjects())
			{
				wo.updatePerceptClustersToAttended();
			}
		}
		else
		{
			BlobGroupChain bgc = utterance.getBlobGroupChain();
			TreeMap<Double,BlobChain> blobChainHandDistances = 
					new TreeMap<Double, BlobChain>(bgc.getBlobChainHandDistances(MIN_FRAMES));
			
			Set<Entry<Double,BlobChain>> entrySet = blobChainHandDistances.entrySet();
			Iterator<Entry<Double, BlobChain>> it = entrySet.iterator();
			
			for (WorldObject wo : focusWorldObjectGroup.getWorldObjects())
			{
				if (!it.hasNext())
					break;
				wo.setPerceptClustersToBlobChain(it.next().getValue());
			}
			
		}
		
		worldObjectsResolved = true;
	}
	
	public boolean resolveWorldObjectsWithLattice(LatticeLearner ll)
	{
		if (!assignWorldObjectsToBlobIds())
			return false;
		
		System.out.println("Utterance: " + utterance.hashCode());
		System.out.println(utterance.toString());
		System.out.println("Focus WOG: " + focusWorldObjectGroup);
		System.out.println("# of WOGs: " + worldObjectGroups.size());
			
		
		if (blobGroupChain.size() == 1)
		{
			for (WorldObject wo: focusWorldObjectGroup.getWorldObjects())
			{
				System.out.println("Setting perceptclusters for "
						+ "single blob chain in utterance: " + utterance.hashCode());
				for (BlobChain bc : blobGroupChain.getBlobChains().values())
				{
					wo.setPerceptClustersToBlobChain(bc);
					System.out.println("Worldobject " + wo + " now has " + wo.getPerceptClusters().size() + " PC's");
					break;
				}
				wo.setFocus(true);
			}
			return true;
		}
		
		// If there are too many objects, return false
		if (blobGroupChain.size() > 7)
			return false;
			
		lattice = new ReferenceLattice(this);
		if (USE_GROUND_TRUTH && utterance.getGroundTruthReferences() != null)
			lattice.setTargetSet(utterance.groundTruthResult());
		lattice.setLatticeLearner(ll);
		lattice.construct();
		

		return resolveLattice(false, false);
		
	}
	
	/**
	 * Resolves the reference lattice and updates data accordingly.
	 * @param reResolution Whether this is a resolution of a lattice that was already attempted
	 * @param lastGuess Whether an unresolved lattice should be guessed
	 * @return
	 */
	public boolean resolveLattice(boolean reResolution, boolean lastGuess)
	{
		
		boolean resolved = lattice.resolve(reResolution, lastGuess);
		//System.out.println();
		if (resolved)
		{
			worldObjectsResolved = true;
			printObjectLocations();
			updateUtteranceObjects();
			lattice.dumpPaths();
			return true;
		}
		
		worldObjectsResolved = false;
		
		return false;
	}
	
	/**
	 * Prints the X coordinates of objects for debugging reference resolution.
	 */
	public void printObjectLocations()
	{
		System.out.println("Demonstration: " + utterance.toString());
		System.out.println("Utterance: " + utterance.hashCode());
		System.out.println("Referred objects");
		for (WorldObject wo : focusWorldObjectGroup.getWorldObjects())
		{
			System.out.println(wo.getClassName() + " X position: " + wo.getCurrentBlobChain().getAverageX());
		}
		System.out.println("Other objects");
		for (WorldObjectGroup wog : worldObjectGroups)
		{
			if (wog != focusWorldObjectGroup)
				for (WorldObject wo : wog.getWorldObjects())
				{
					if (!focusWorldObjectGroup.getWorldObjects().contains(wo))
					{
						System.out.println("Other object X position: " + wo.getCurrentBlobChain().getAverageX());
						
					}
				}
		}
	}
	
	/**
	 * This sets the focus of all of the FocusWorldObjects to true, and non-focus WorldObjects
	 * to false. Mainly used for reference resolution evaluation.
	 */
	public void updateUtteranceObjects()
	{
		System.out.println("Other objects");
		utterance.getWorldObjects().clear();
		for (WorldObjectGroup wog : worldObjectGroups)
		{
			if (wog != focusWorldObjectGroup)
				for (WorldObject wo : wog.getWorldObjects())
				{
					if (!focusWorldObjectGroup.getWorldObjects().contains(wo))
					{
						wo.setFocus(false);
						utterance.addWorldObject(wo);
					}
				}
		}
		for (WorldObject wo : focusWorldObjectGroup.getWorldObjects())
		{
			System.out.println("Focus world object: " + wo);
			wo.setFocus(true);
			utterance.addWorldObject(wo);
		}

	}

	
	public void resolveWorldObjectsOld()
	{
		//Removed for now - don't need to auto duplicate right now
/*		if (worldObjectGroups.size() < blobGroupChain.size())
		{
			
			// Duplicate plural world objects
			for (WorldObjectGroup wog : worldObjectGroups)
			{
				if (wog.getQuantifier() == "plural")
				{
					for (int i = 0; i < blobGroupChain.size() - worldObjectGroups.size(); i++)
						wog.add(new WorldObject(wo,i+1));
				}
			}
		}*/
		blobGroupChain = new BlobGroupChain(utterance,this);
		
		/*
		 * Don't assume this is working for multiple objects - hand distance is the only thing
		 * kind of guaranteed to work.
		 */
		HashSet<BlobChain> assignedBlobChains = new HashSet<BlobChain>();
		WorldManager worldManager = utterance.getSession().getWorldManager();
		// Greedy allocation of blob chains
		for (WorldObjectGroup wog : worldObjectGroups)
		{
			List<BlobChain> blobChains = new ArrayList<BlobChain>();
			BlobGroupChain bgc = utterance.getBlobGroupChain();
			for (WorldObject wo : wog.getWorldObjects())
			{
				
				// Find which blob chain matches the description or hand location the best
				TreeMap<Double, BlobChain> blobChainDistances = null;
				BlobChain bestBlobChain = null;
				
				if (PRECLASSIFY_BLOB_CHAINS)
					blobChainDistances = bgc.getBlobChainDistances(wo,worldManager.getDemonstratedObjects().values());
				
				// If no description, bestBlobChain is that which is closest to the hand
				if (blobChainDistances == null && USE_HAND_DISTANCE)
				{
					TreeMap<Double, BlobChain> handDistances = bgc.getBlobChainHandDistances(MIN_FRAMES);
					
					if (handDistances == null || handDistances.size() == 0)
						continue;
					
					bestBlobChain = handDistances.firstEntry().getValue();
				}
				else if (blobChainDistances != null)
				{
					for (BlobChain bc: blobChainDistances.values())
					{
						// Assign the object to the first unassigned blob chain 
						if (bc.getAssignedWorldObject() == null)
						{
							bestBlobChain = bc;
							
							break;
						}
							
					}
				}
				if (bestBlobChain != null)
				{
					bestBlobChain.setAssignedWorldObject(wo);
					blobChains.add(bestBlobChain);
					assignedBlobChains.add(bestBlobChain);
				}
				
				wo.setPerceptClustersToBlobChain(bestBlobChain);
			}
		}
		
		worldObjectsResolved = true;
	}
	

	
	public List<WorldObject> getResolvedWorldObjects(LatticeLearner ll)
	{
		if (!worldObjectsResolved)
		{
			if (useLatticeResolution)
				resolveWorldObjectsWithLattice(ll);
			else
				resolveWithHandDistance();

		}
		
		List<WorldObject> worldObjects = new ArrayList<WorldObject>();
		
		for (WorldObjectGroup wog : worldObjectGroups)
		{
			worldObjects.addAll(wog.getWorldObjects());
		}
		
		return worldObjects;
	}
	
	protected WorldManager getWorldManager()
	{
		return utterance.getSession().getWorldManager();
	}

	public Utterance getUtterance() {
		return utterance;
	}

	public BlobGroupChain getBlobGroupChain() {
		return blobGroupChain;
	}
	
	public List<WorldObject> getNonNullWorldObjects()
	{	
//		if (!worldObjectsResolved)
//		{
//			if (useLatticeResolution)
//				resolveWorldObjectsWithLattice();
//			else
//				resolveWorldObjects();
//		}
			
		
		List<WorldObject> worldObjects = new ArrayList<WorldObject>();
		
		for (WorldObjectGroup wog : worldObjectGroups)
		{
			if (!wog.isNull())
				worldObjects.addAll(wog.getWorldObjects());
		}
		
		return worldObjects;
	}
	
	public WorldObjectGroup getFocusWorldObjectGroup() {
		return focusWorldObjectGroup;
	}

	public void setFocusWorldObjectGroup(WorldObjectGroup focusWorldObjectGroup) {
		this.focusWorldObjectGroup = focusWorldObjectGroup;
		System.out.println("Focus object " + focusWorldObjectGroup.getClassName() + " has properties ");
		for (String property : focusWorldObjectGroup.getUniversalProperties())
		{
			System.out.println(property);
		}
	}

	public DemonstrationType getDemonstrationType() {
		return demonstrationType;
	}

	public ReferenceLattice getLattice() {
		return lattice;
	}

	/**
	 * Returns whether there is insufficient feature data or it is too noisy to get
	 * consistent blob IDs.
	 * @return
	 */
	public boolean isMissingFeatureData() {
		return missingFeatureData;
	}
	
	
}
