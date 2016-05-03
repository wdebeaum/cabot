package owlground.reference;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class TUNAReader {
	
	NodeList attributes;
	NodeList entities;
	//List<String> overspecifiedDescriptorTypes;
	//List<String> nonOverspecifiedDescriptorTypes;
	HashMap<String, String> overspecifiedDescriptors; // Map from descriptor to descriptor type
	HashMap<String, String> descriptors; // Map from descriptor to descriptor type
	int numberOfObjects;
	int numberOfReferredObjects;
	int visualDiversity;
	Set<String> properties;

	public TUNAReader() {
		//overspecifiedDescriptorTypes = new ArrayList<String>();
		//nonOverspecifiedDescriptorTypes = new ArrayList<String>();
		overspecifiedDescriptors = new HashMap<String,String>();
		descriptors = new HashMap<String, String>();
		properties = new HashSet<String>();
		numberOfReferredObjects = 1; // Only considering single mentions for TUNA
	}

	// From http://www.java-tips.org/java-se-tips/javax.xml.parsers/how-to-read-xml-file-in-java.html
	public boolean loadTUNAFile(File fileToRead)
	{
		DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
		DocumentBuilder db = null;
		try {
			db = dbf.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		}
		Document doc = null;
		try {
			doc = db.parse(fileToRead);
		} catch (SAXException se)
		{
			se.printStackTrace();
			return false;
			
		}
		catch (IOException ie) {
			// TODO Auto-generated catch block
			ie.printStackTrace();
			return false;
		}
		doc.getDocumentElement().normalize();
		entities = doc.getElementsByTagName("ENTITY");
		numberOfObjects = entities.getLength();
		NodeList attributeSetList = doc.getElementsByTagName("ATTRIBUTE-SET");
		Node attributeSet = null;
		for (int i = 0; i < attributeSetList.getLength(); i++)
		{
			Node subNode = attributeSetList.item(i);
			if (subNode.getNodeType() == Node.ELEMENT_NODE)
			{
				attributeSet = attributeSetList.item(0);
				break;
			}
		}
		
		if (attributeSet == null)
			return false;
		
		attributes = attributeSet.getChildNodes();
		
		processTUNADemonstration();
		
		return true;
	}
	
	private void processTUNADemonstration()
	{
		HashSet<Node> entitiesMatchedSoFar = new HashSet<Node>();
		//String lastDescriptorName = null;
		//System.out.println("NEW DEMONSTRATION");
		// Attributes are listed in backwards order, for some reason
		for (int i = attributes.getLength()-1; i >= 0; i--)
		{
			Node attribute = attributes.item(i);
			if (attribute.getNodeType() != Node.ELEMENT_NODE)
				continue;
			HashSet<Node> matchingEntities = getMatchingEntities(attribute);
			if (entitiesMatchedSoFar.isEmpty())
			{
				entitiesMatchedSoFar.addAll(matchingEntities);
				//lastDescriptorName = attribute.getAttributes().getNamedItem("VALUE").getNodeValue();
				descriptors.put(attribute.getAttributes().getNamedItem("VALUE").getNodeValue(), 
						attribute.getAttributes().getNamedItem("NAME").getNodeValue());
				continue;
			}
			else
			{

				//HashSet<Node> previousMatching = new HashSet<Node>(entitiesMatchedSoFar);
				entitiesMatchedSoFar.retainAll(matchingEntities);
				
				descriptors.put(attribute.getAttributes().getNamedItem("VALUE").getNodeValue(),
						attribute.getAttributes().getNamedItem("NAME").getNodeValue());
				
				// This is the old definition of overspecified attributes
//				if (previousMatching.equals(entitiesMatchedSoFar)) // Overspecified
//				{
//
//					{
//						overspecifiedDescriptorTypes.add(
//								attribute.getAttributes().getNamedItem("NAME").getNodeValue());
//					}
//				}
//				else
//				{
//					System.out.println("NOT OVERSPECIFIED");
//					nonOverspecifiedDescriptorTypes.add(
//							attribute.getAttributes().getNamedItem("NAME").getNodeValue());
//				}
				//lastDescriptorName = attribute.getAttributes().getNamedItem("NAME").getNodeValue();
			}
		}
		
		processOverspecification();
	}
	
	/**
	 * Search through the objects listed in the scene to find those matching the given
	 * attribute.
	 * @param attribute
	 * @return HashSet of the matching objects as Nodes
	 */
	private HashSet<Node> getMatchingEntities(Node attribute)
	{
		String attributeName = attribute.getAttributes().getNamedItem("NAME").getNodeValue();
		String attributeValue = attribute.getAttributes().getNamedItem("VALUE").getNodeValue();
		return getMatchingEntities(attributeName, attributeValue);
	}
	
	/**
	 * Search through the objects listed in the scene to find those matching the given
	 * attribute.
	 * @param attribute
	 * @return HashSet of the matching objects as Nodes
	 */
	private HashSet<Node> getMatchingEntities(String attributeValue, String attributeName)
	{
		HashSet<Node> matchingEntities = new HashSet<Node>();

		for (int i = 0; i < entities.getLength(); i++)
		{
			Node entity = entities.item(i);
			if (entity.getNodeType() != Node.ELEMENT_NODE)
				continue;
			
			NodeList entityAttributes = entity.getChildNodes();
			for (int j = 0; j < entityAttributes.getLength(); j++)
			{
				Node entityAttribute = entityAttributes.item(j);
				if (entityAttribute.getNodeType() != Node.ELEMENT_NODE)
					continue;
				String entityAttributeName = entityAttribute.getAttributes().
												getNamedItem("NAME").getNodeValue();
				//System.out.println("Entity attribute name: " + entityAttributeName);
				String entityAttributeValue = entityAttribute.getAttributes().
						getNamedItem("VALUE").getNodeValue();
				//System.out.println("Entity attribute value: " + entityAttributeValue);
				String entityAttributeType = entityAttribute.getAttributes().
						getNamedItem("TYPE").getNodeValue();
				//System.out.println("Entity attribute type: " + entityAttributeType);
				if (entityAttributeType.equals("literal"))
					properties.add(entityAttributeValue);
				
				if (entityAttributeName.equals(attributeName) && 
						entityAttributeValue.equals(attributeValue))
				{
					matchingEntities.add(entity);
					//System.out.println("Added matching entity");
					break;
				}
			}
		}
		//System.out.println(matchingEntities.size() + " matching entities");
		return matchingEntities;
	}
	
	/**
	 * Adds this demonstration to the given LatticeLearner
	 * @param ll
	 */
	public void addToLatticeLearner(LatticeLearner ll)
	{
		System.out.println("Adding to lattice learner:");
		System.out.println("Overspecified: " + isOverspecified());
		System.out.println("Number of objects: " + numberOfObjects);
		System.out.println("Number of descriptors: " + descriptors.size());
		//System.out.println("Number of overspecd types: " + overspecifiedDescriptorTypes.size());
		//System.out.println("Number of nonoverspecd types: " + nonOverspecifiedDescriptorTypes.size());
		HashSet<String> overspec = new HashSet<String>(overspecifiedDescriptors.values());
		HashSet<String> allDescriptors = new HashSet<String>(descriptors.values());
		ll.addTunaDemonstration(isOverspecified(), numberOfObjects, 
				numberOfReferredObjects, allDescriptors,
				overspec, properties.size());
	}
	
	/**
	 * Test the matching entities generated by leaving one property out. If
	 * a property can be left out and resolve to one object, it is overspecified.
	 * Currently only works assuming a single referent in TUNA.
	 */
	private void processOverspecification()
	{
		System.out.println("Processing overspecification...");
		System.out.println(descriptors.size() + " descriptors");
		for (String descriptorName : descriptors.keySet())
		{
			HashSet<Node> matchingEntities = null;
			
			for (String otherDescriptorName : descriptors.keySet())
			{
				if (descriptorName.equals(otherDescriptorName))
					continue;
				if (matchingEntities == null)
				{
					matchingEntities = new HashSet<Node>(getMatchingEntities(otherDescriptorName,
																descriptors.get(otherDescriptorName)));
				}
				else
				{
					matchingEntities.retainAll(getMatchingEntities(otherDescriptorName,
																descriptors.get(otherDescriptorName)));
				}
				System.out.println("Matching entities: " + matchingEntities.size());
			}
			// Only one descriptor, can't be overspecified
			if (matchingEntities == null)
				return;
			System.out.println("Descriptor excluded: " + descriptorName);
			//System.out.println("Matching entities: " + matchingEntities.size());
			if (matchingEntities.size() == 1)
			{
				overspecifiedDescriptors.put(descriptorName, descriptors.get(descriptorName));
				break;
			}
			
		}
	}
	
	
	public int test(LatticeLearner ll)
	{
		List<String> totalDescriptors = new ArrayList<String>();
		totalDescriptors.addAll(descriptors.values());
		//totalDescriptors.addAll(nonOverspecifiedDescriptorTypes);
		//System.out.println("Properties: ");
		//for (String property : properties)
		//	System.out.print(property + " ");
		int correctAnswers = 0;
		for (String descriptorType : descriptors.values())
		{
			boolean overspecified = overspecifiedDescriptors.containsValue(descriptorType);
			if (ll.testTunaEdge(overspecified, numberOfObjects, descriptorType, properties.size()))
				correctAnswers++;
		}
		return correctAnswers;
	}
	
	public boolean isOverspecifiedDescriptor(String descriptor)
	{
		return overspecifiedDescriptors.containsKey(descriptor);
			
	}
	
	public Set<String> overspecifiedDescriptorTypes()
	{
		return new HashSet<String>(overspecifiedDescriptors.values());
	}
	
	public boolean isOverspecified()
	{
		return (overspecifiedDescriptors.size() > 0);
	}
}
