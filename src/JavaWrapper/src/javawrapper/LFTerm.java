package javawrapper;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;
import java.util.*;

import TRIPS.KQML.*;

public class LFTerm implements Serializable{


	private static String[] subjectRoles = {":AGENT",":CAUSE",":COGNIZER",":INSTRUMENT",":EXPERIENCER",":AFFECTED",":THEME"};
	private static String[] objectRoles = {":PROPERTY",":THEME",":RECIPIENT",":GOAL",":BENEFICIARY",
											":AFFECTED",":EFFECT",":ADDRESSEE",":RESULT", ":CO-THEME", ":STIMULUS", ":VIA"};
	public static Map<String,String> referenceMapping = new HashMap<String,String>() {{
		put("PERSON", "person");
}};

	private static final long serialVersionUID = 2L;
	protected static final int MAX_DEPTH = 3;
	
	// KQML format of LFTerm is <ontological Class> <variable name> (<concept>)
	// KQML format of concept is (:* <concept parent> <word>)
	protected String variableName;
	protected String ontologicalClass;
	protected String concept;
	
	protected HashMap<String,KQMLObject> properties; // Note that properties includes roles
	protected Turn turn; // The turn this LFTerm belongs to
	protected KQMLList logicalForm; // The original logical form in KQML format
	protected int coreferenceDepth = 0;

	public static HashSet<String> ontologicalClasses = new HashSet<String>();
	public static HashSet<String> roles = new HashSet<String>();
	public static HashSet<String> speechActs = new HashSet<String>();
	private int termId = 0;

	public LFTerm(String ontologicalClass, String variableName, String concept, Turn turn)
	{
		this.ontologicalClass = ontologicalClass;
		this.variableName = variableName;
		this.concept = concept;
		properties = new HashMap<String, KQMLObject>();
		if (turn == null)
			throw new NullPointerException("Null turn");
		this.turn = turn;
	}
	
	public void addProperty(String parameterName, KQMLObject value)
	{
		properties.put(parameterName, value);
	}

	public String getVariableName() {
		return variableName;
	}

	public String getOntologicalClass() {
		return ontologicalClass;
	}

	public String getConcept() {
		return concept;
	}
	
	public String getConceptParent() {
		if (concept.contains("ONT::"))
		{
			String[] ontSplit = concept.split("ONT::");
			
			return ontSplit[ontSplit.length - 1].split(" ")[0].split("\\)")[0];
			
		}
		else if (concept.contains("W::"))
			return getWord();
		else
			return concept;
	}

	public Map<String, KQMLObject> getProperties() {
		return Collections.unmodifiableMap(properties);
	}
	
	/**
	 * Checks if there is an argument, and returns true (this term represents a verb)
	 * @return true if this LFTerm is a verb, false otherwise
	 */
	public boolean isVerb()
	{
		if (concept.equalsIgnoreCase("(:* ONT::ELLIPSIS)"))
			return false;
		
		for (String argument : properties.keySet())
		{
			for (int i = 0; i < subjectRoles.length; i++)
			{
				if (argument.equalsIgnoreCase(subjectRoles[i]))
					return true;
			}
			
			for (int i = 0; i < objectRoles.length; i++)
			{
				if (argument.equalsIgnoreCase(objectRoles[i]))
					return true;
			}
		}
		return false;
	}
	
	
	/**
	 * Finds the first matching subject role, or null if there is none
	 * @param excluded - a role to exclude (already used for object)
	 * @return
	 */
	public KQMLObject getSubject(String excluded)
	{
		for (String s : subjectRoles)
		{
			if (!s.equalsIgnoreCase(excluded) && properties.containsKey(s))
			{
				return properties.get(s);
			}
		}
		
		return null;
	}
	
	/**
	 * Finds the first matching object role, or null if there is none
	 * @param excluded - a role to exclude (already used for subject)
	 * @return
	 */
	public KQMLObject getObject(String excluded)
	{
		for (String s : objectRoles)
		{
			if (!s.equalsIgnoreCase(excluded) && properties.containsKey(s))
			{
				return properties.get(s);
			}
		}
		
		return null;
	}
	

	/*
	 * Check if the given string is a role
	 */
	public static boolean isRole(String str)
	{
		return (str.charAt(0) == ':');
	}

	/*
	 * Get the object role that isn't the excluded role
	 */
	public String getObjectRole(String excluded)
	{
		for (String s : objectRoles)
		{
			if (!s.equalsIgnoreCase(excluded) && properties.containsKey(s))
			{
				return s;
			}
		}
		
		return null;
	}
	
	/*
	 * Get the subject role that isn't the excluded role
	 */
	public String getSubjectRole(String excluded)
	{
		for (String s : subjectRoles)
		{
			if (!s.equalsIgnoreCase(excluded) && properties.containsKey(s))
			{
				return s;
			}
		}
		
		return null;
	}
	
	/**
	 * Get the number of (object or subject) roles in this LFTerm, assuming that
	 * there is at most one subject.
	 * @return
	 */
	public int getNumberOfRoles()
	{
		int numRoles = 0;
		
		for (String argument : properties.keySet())
		{
			boolean foundSubject = false;
			for (int i = 0; i < subjectRoles.length; i++)
			{
				if (argument.equalsIgnoreCase(subjectRoles[i]))
				{
					numRoles++;
					foundSubject = true;
					break;
				}
			}
			
			if (foundSubject)
				continue;
			
			for (int i = 0; i < objectRoles.length; i++)
			{
				if (argument.equalsIgnoreCase(objectRoles[i]))
					numRoles++;
			}
		}
		return numRoles;
	}
	
	public String getSubjectRole()
	{
		return getSubjectRole("");
	}
	
	public String getObjectRole()
	{
		return getObjectRole("");
	}
	
	public KQMLObject getObject()
	{
		return getObject("");
	}
	
	public KQMLObject getSubject()
	{
		return getSubject("");
	}
	
	public boolean isPassive()
	{
		return (properties.containsKey(":PASSIVE") && 
				properties.get(":PASSIVE").stringValue().equals("+"));
	}
	

	public String getVariableName(KQMLObject ko)
	{
		if (!(ko instanceof KQMLList))
			return null;
		
		return ((KQMLList)ko).get(1).stringValue();
	}
	
	public static String getConcept(KQMLObject ko)
	{
		if (!(ko instanceof KQMLList))
			return null;
		
		return ((KQMLList)ko).get(2).stringValue();
	}
	
	protected String getVariableConcept(String variableName)
	{
		if (turn == null)
			System.out.println("turn is null");
		
		if (turn.getDialogue() == null)
			System.out.println("dialogue is null");
		
		if (turn.getDialogue().getLFTerm(variableName) == null)
			System.out.println("Lfterm is null");
			
		if (variableName != null && turn.getDialogue().getLFTerm(variableName) != null)
			return turn.getDialogue().getLFTerm(variableName).getConcept();
		else
			return "";
	}
	
	private String replaceNullValues(String value)
	{
		if (isNullValue(value))
			return "empty";
		return value;
	}
	
	protected boolean isNullValue(String value)
	{
		return (value.equalsIgnoreCase("ont::referential-sem") || value.equals("") || 
				value.equalsIgnoreCase("nil") || value.equals("(:* "));
	}
	

	
	protected static boolean isVariable(String objectStringValue)
	{
		if (objectStringValue.startsWith("ONT::V"))
		{
			String number = objectStringValue.split("ONT::V")[1];
			if (Character.isDigit(number.charAt(0)))
				return true;
		}
		return false;
	}
	
	/**
	 * Resolves the coreference for a particular variable name
	 * @see resolveCoreference()
	 * @param variableName
	 * @return
	 */
	protected String resolveCoreference(String variableName)
	{
		if (getLFTermById(variableName) == null)
			return "empty";
		return getLFTermById(variableName).resolveCoreference();
	}
	
	public int getVariableNumber(String variableName)
	{
		String number = variableName.split("::V")[1].split(" ")[0];
		if (Character.isDigit(number.charAt(0)))
			return Integer.parseInt(number);
		return -1;
	}
	
	/**
	 * Determines if there is a coreference link in this LFTerm. If so, it returns the result 
	 * as a coref string. Otherwise, it returns the String representation in the triple format.
	 * @return
	 */
	private String resolveCoreference()
	{
		coreferenceDepth++;
		// Prevents infinite loops - not really needed anymore, I don't think
		if (coreferenceDepth < MAX_DEPTH)
		{
			String refType = "ref";
			if (referenceMapping.containsKey(getConceptParent()))
				refType = referenceMapping.get(getConceptParent());
			
			// Ordering is significant - They are ordered so that the first properties 
			// (":COREF", ":VAL") are more likely to be corefs than the later ones
			if (properties.containsKey(":COREF"))
			{
				if (isVariable(properties.get(":COREF").stringValue()))
					return "coref(\"" + getWord() + "\",[" + refType + "-" +
						getVariableNumber(properties.get(":COREF").stringValue()) + "])";
			}
			
			if (properties.containsKey(":VAL"))
			{
				if (isVariable(properties.get(":VAL").stringValue()))
					return "coref(\"" + getWord() + "\",[" + refType + "-" +
						getVariableNumber(properties.get(":VAL").stringValue()) + "])";
			}
			
			if (properties.containsKey(":OF"))
			{
				if (isVariable(properties.get(":OF").stringValue()))
					return "coref(\"" + getWord() + "\",[" + refType + "-" +
						getVariableNumber(properties.get(":OF").stringValue()) + "])";
			}
			
			if (properties.containsKey(":CONTENT"))
			{
				LFTerm term = getLFTermById(properties.get(":CONTENT").stringValue());
				if (term != null && term.isVerb())
					return "chunk-" + term.getChunkId();
				
				if (isVariable(properties.get(":CONTENT").stringValue()))
					return "coref(\"" + getWord() + "\",[" + refType + "-" +
						getVariableNumber(properties.get(":CONTENT").stringValue()) + "])";
			}
			
			// This is often a list, so it needs some special treatment
			if (properties.containsKey(":MODS"))
			{
				KQMLObject modObject = properties.get(":MODS");
				LFTerm term = null;
				String modVariableId = null;
				if (modObject instanceof KQMLList)
				{
					modVariableId = ((KQMLList)modObject).get(0).stringValue();
					term = getLFTermById(modVariableId);
				}
				else
				{
					modVariableId = modObject.stringValue();
					term = getLFTermById(modObject.stringValue());
				}
				
				if (term != null && term.isVerb())
					return "chunk-" + term.getChunkId();
				
				if (isVariable(properties.get(":MODS").stringValue()))
					return "coref(\"" + getWord() + "\",[" + refType + "-" +
						getVariableNumber(modVariableId) + "])";
			}

			// To get the correct referent name, return the name given or the name of the
			// referent with the given ID
			if (properties.containsKey(":NAME-OF"))
			{
				KQMLObject nameOf = properties.get(":NAME-OF");
				if (nameOf instanceof KQMLList)
				{
					KQMLList nameOfList = (KQMLList)nameOf;
					if (!nameOfList.isEmpty())
						if (isVariable(nameOfList.get(0).stringValue()))
							return "coref(\"" + concept + "\",[ref-" + 
										getVariableNumber(properties.get(":NAME-OF").stringValue()) + "])";
						else
							if (!isNullValue(getWord()))
								return "str(\"" + getWord() + "\")";
				}
				if (isVariable(properties.get(":NAME-OF").stringValue()))
					return "coref(\"" + concept + "\",[ref-" + 
						getVariableNumber(properties.get(":NAME-OF").stringValue()) + "])";
			}
			
			// For sequences, append the sequence elements using the operator
			// For example, a sequence with (ONT::BREAD ONT::MILK) with operator AND
			// will return "bread and milk"
			if (properties.containsKey(":SEQUENCE"))
			{
				String result = "";
				KQMLList sequenceList = (KQMLList)(properties.get(":SEQUENCE"));
				String operator = " ";
				// Get operator and remove ONT::
				if (properties.containsKey(":OPERATOR"))
				{
					operator = " " + properties.get(":OPERATOR").stringValue().split("::")[1] + " "; 
				}
				
				for (int i = 0; i < sequenceList.size(); i++)
				{
					LFTerm sequenceTerm = getLFTermById(sequenceList.get(i).stringValue());
					if (sequenceTerm == null)
						continue;
					result += sequenceTerm.getWord();
					if (i < sequenceList.size() - 1)
						result += operator;
				}
				
				if (!isNullValue(result))
					return "str(\"" + result + "\")";
			}
		}

		// Map personal pronouns to the speaker
		if (properties.containsKey(":REFERS-TO"))
			if (properties.get(":REFERS-TO").equals("ONT::USER"))
				return "str(\"" + getTurn().getSpeaker() + "\")";
		
		if (!isNullValue(getWord()))
			return "str(\"" + getWord() + "\")";
		
		return "empty";
	}
	
	public String getWord()
	{
		if (concept.equalsIgnoreCase("ONT::REFERENTIAL-SEM"))
		{
			if (properties.containsKey(":NAME-OF"))
			{
				String word = ((KQMLList)(properties.get(":NAME-OF"))).get(0).stringValue();
				if (word.contains("ONT::"))
					return word.substring(5, word.length());
				return word;
			}
		}
		if (concept.equalsIgnoreCase("ONT::HAVE-PROPERTY"))
		{
			return "is";
		}

		String[] splitResult = concept.split("W::");
		
		return splitResult[splitResult.length-1].split("\\)")[0].split("ONT::")[0];
	}
	
	public LFTerm getLFTermById(String id)
	{
		return turn.getDialogue().getLFTerm(id);
	}
	
	public String getSpeechAct()
	{
		KQMLList hypsList = (KQMLList)(logicalForm.getKeywordArg(":HYPS"));
		if (hypsList != null && hypsList.get(0) != null && hypsList.get(0) instanceof KQMLList)
		{
			return ((KQMLList)hypsList.get(0)).get(2).stringValue();
		}
		
		return null;
	}

	
	public String getSpeechActId()
	{
		KQMLList hypsList = (KQMLList)(logicalForm.getKeywordArg(":HYPS"));
		if (hypsList != null && hypsList.get(0) != null && hypsList.get(0) instanceof KQMLList)
		{
			String speechActIdString = ((KQMLList)hypsList.get(0)).getKeywordArg("ONT::SPEECHACT").stringValue();
			return speechActIdString.substring(2, speechActIdString.length());
		}
		
		return null;
	}
	
	public String toString()
	{
		StringBuilder result = new StringBuilder();
		
		result.append(ontologicalClass);
		result.append(" ");
		result.append(variableName);
		result.append(" ");
		result.append(concept);
		result.append("\n");
		
		for (Map.Entry<String, KQMLObject> property : properties.entrySet())
		{
			result.append("(");
			result.append(property.getKey());
			result.append(" ");
			result.append(property.getValue().stringValue());
			result.append(") ");
		}
		
		return result.toString();
	}
	
	public boolean isConcept(String str)
	{
		if (!isVariable(str) && str.contains("ONT::") && !Character.isDigit(str.split("ONT::")[1].charAt(0)))
		{
			return true;
		}
		return false;
	}
	
	public boolean isWord(String str)
	{
		return str.contains("W::");
	}
	
	public boolean isNumber(String str)
	{
		if (str.contains("ONT::"))
			return Character.isDigit(str.split("ONT::")[1].charAt(0));
		
		return Character.isDigit(str.charAt(0));
	}

	public Turn getTurn() {
		return turn;
	}

	public KQMLList getLogicalForm() {
		return logicalForm;
	}

	public void setLogicalForm(KQMLList logicalForm) {
		this.logicalForm = logicalForm;
	}


	public int getChunkId() {
		return Integer.parseInt(getSpeechActId() + String.format("%02d",termId));
	}

	public void setTermId(int termId)
	{
		this.termId = termId;
	}
	
}
