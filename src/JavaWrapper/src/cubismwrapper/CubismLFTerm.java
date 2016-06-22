package cubismwrapper;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javawrapper.LFTerm;
import javawrapper.Turn;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class CubismLFTerm extends LFTerm {
	
	private boolean gateFormatting = true;
	
	public static Map<String,String> speechActMapping = new HashMap<String,String>() {{
		put("ONT::TELL", "[sno]");
		put("ONT::APOLOGIZE", "[apl]");
		put("ONT::SA_YN-QUESTION", "[ynq]");
		put("ONT::SA_WH-QUESTION", "[whq]");
		put("ONT::REQUEST", "[adr]");
		put("ONT::ASK-WHAT-IS", "[opq]");
		put("ONT::EVALUATE", "[oan]");
		put("ONT::PROPOSE", "[ooc]");
		put("ONT::ANSWER", "[oan]");
}};

	public CubismLFTerm(String ontologicalClass, String variableName,
			String concept, Turn turn) {
		super(ontologicalClass, variableName, concept, turn);
		// TODO Auto-generated constructor stub
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = -223595889130142608L;


	/**
	 * Converts a KQML value to the corresponding Prolog format - a list of con() (concepts),
	 *  var() (variable names), int() (integers), and float() (floats)
	 * @param kObj
	 * @return a String with the Prolog representation of the value
	 */
	public String kqmlValueToProlog(KQMLObject kObj)
	{
		KQMLList kList;
		ArrayList<String> resultStringList = new ArrayList<String>();
		
		if (kObj instanceof KQMLList)
			kList = (KQMLList)kObj;
		else
		{
			kList = new KQMLList(kObj);
		}

		for (KQMLObject klo : kList)
		{
			String elementText = klo.stringValue();
			
			if (isVariable(klo.stringValue()))
			{
				
				//elementText = klo.stringValue().split("::V")[1];
				resultStringList.add(convertVariableNameToProlog(elementText));
				continue;
				//sb.append("var(" + elementText + ")");
			}
			else if (isConcept(elementText) || isRole(elementText) || isWord(elementText) ||
					elementText.equals("+") || elementText.equals("-"))
			{
				resultStringList.add("con(\"" + elementText + "\")");
				continue;
			}
			else
			{
				if (elementText.contains("ONT::"))
					elementText = elementText.split("ONT::")[1];
				
				if (elementText.contains("/"))
				{
					String[] elements = elementText.split("/");
					resultStringList.add("float(" + (Float.parseFloat(elements[0]) / Float.parseFloat(elements[1])) + ")");
					continue;
				}
				else if (elementText.contains("."))
				{
					resultStringList.add("float(" + elementText + ")");
					continue;
				}
				
				try {
					Integer.parseInt(elementText);
					resultStringList.add("int(" + elementText + ")");
					continue;
				}
				catch (NumberFormatException e)
				{
					try {
						float floatValue = Float.parseFloat(elementText);
						resultStringList.add("float(" + floatValue + ")");
						continue;
					}
					catch (NumberFormatException ef){
						ef.printStackTrace();
						resultStringList.add("con(\"" + elementText + "\")");
						continue;
					}
					
				}
			}
		}
		
		StringBuilder sb = new StringBuilder();
		sb.append("[");
		int counter = 0;
		for (String s : resultStringList)
		{
			sb.append(s);
			counter++;
			if (counter < resultStringList.size())
				sb.append(",");
		}
		sb.append("]");
		return sb.toString();
	}

	
	/**
	 * Output term in this form:
		:- type expr ---> expr(turnno,tconst,varname,exptp,attributes).
		:- type tconst ---> the ; a ; pro ; impro.
		(or :- type tconst ---> 'ONT::THE', 'ONT::A', 'ONT::PRO', 'ONT::IMPRO'.)
		:- type turnno == int 
		:- type varname == string.
		:- type exptp == string.
		(is there a list of the expression types?)
		:- type role ---> content ; mods ; ???.
		: type value ---> var(varname) ; vars(list(varname)) ; attrs(attributes).
		:- type attributes == list(pair(role,value)).
	 * @return
	 */
	public String generateExpressionTriple()
	{
		
		String tconst = ontologicalClass;
		ontologicalClasses.add(ontologicalClass);
		String exptp = getSpeechAct();
		speechActs.add(exptp);
		StringBuilder attributes = new StringBuilder();
		attributes.append("[");
		int count = 0;
		for (Map.Entry<String, KQMLObject> entry : properties.entrySet())
		{
			roles.add(entry.getKey());
			attributes.append("\"" + entry.getKey() + "\"-" +
								kqmlValueToProlog(entry.getValue()));
			count++;
			if (count != properties.size())
				attributes.append(",");
		}
		attributes.append("]");
		return "vardef('" + tconst + "'," + getVariableNumber(variableName) + ",\"" + concept + "\"," + attributes + ")";
		
	}
	

	/**
	 * Generates the triple without the surrounding context - just subject, predicate, object
	 * @return
	 */
	public String generateBareTriple()
	{
		if (!isVerb())
			return null;
		
		String subjectRole = "";
		String subject = "empty";
		String objectRole = "";
		String object = "empty";
		String verb = "empty";
		int reference = -1;
		
		// If passive, try to fulfill the object role first
		if (isPassive())
		{
			objectRole = getObjectRole();
			if (objectRole != null)
			{
				String variableName = properties.get(objectRole).stringValue();
				LFTerm term = getLFTermById(variableName);
				if (term != null)
				{
					if (term.isVerb() && coreferenceDepth < MAX_DEPTH)
					{
						reference = term.getChunkId();
						//coreferenceDepth++;
						//object = term.generateBareTriple();
					}
					else 
						object = resolveCoreference(properties.get(objectRole).stringValue());
				}
			}
				
			
			subjectRole = getSubjectRole(objectRole);
			if (subjectRole != null)
			{
				String variableName = properties.get(subjectRole).stringValue();
				LFTerm term = getLFTermById(variableName);
				if (term != null)
				{
					if (term.isVerb() && coreferenceDepth < MAX_DEPTH)
					{
						//coreferenceDepth++;
						//subject = term.generateBareTriple();
						reference = term.getChunkId();
					}
					else 
						subject = resolveCoreference(properties.get(subjectRole).stringValue());
				}
			}
			
		}
		else
		{
			subjectRole = getSubjectRole();
			if (subjectRole != null && subjectRole.equalsIgnoreCase(":THEME") && getNumberOfRoles() == 1)
				subjectRole = null;
			
			if (subjectRole != null)
			{
				String variableName = properties.get(subjectRole).stringValue();
				LFTerm term = getLFTermById(variableName);
				if (term != null)
				{
					if (term.isVerb() && coreferenceDepth < MAX_DEPTH)
					{
						reference = term.getChunkId();
					}
					else 
						subject = resolveCoreference(properties.get(subjectRole).stringValue());
				}
			}
			
			objectRole = getObjectRole(subjectRole);
			if (objectRole != null)
			{
				String variableName = properties.get(objectRole).stringValue();
				LFTerm term = getLFTermById(variableName);
				if (term != null)
				{
					if (term.isVerb() && coreferenceDepth < MAX_DEPTH)
					{
						reference = term.getChunkId();
					}
					else 
						object = resolveCoreference(properties.get(objectRole).stringValue());
				}
			}
			
		}
		
		// Check for negation
		String negation = "";
		if (properties.containsKey(":FORCE"))
			if (properties.get(":FORCE").stringValue().equalsIgnoreCase("ONT::FALSE"))
				negation = "not ";
		
		if (isNullValue(object))
			object = "empty";
		if (isNullValue(subject))
			subject = "empty";
		
		
		if (subject.contains("chunk-"))
		{
			reference = Integer.parseInt(subject.split("-")[1]);
			subject = "empty";
		}
		
		if (object.contains("chunk-"))
		{
			reference = Integer.parseInt(object.split("-")[1]);
			object = "empty";
		}

		if (reference >= 0 && reference != getChunkId())
			return ("triple(" + subject + ",str(\"" + negation + getWord() + "\")," + object + "," +
						reference + ")").toLowerCase();
		
		return ("triple(" + subject + ",str(\"" + negation + getWord() + "\")," + object + ")").toLowerCase();
	}
	
	public String generateTriple()
	{
		if (!isVerb())
			return null;
		
		coreferenceDepth = 0;
		return ("chunk(" + getChunkId() + ",\"" + turn.getCleanAndEscapedText() + "\",\"" +
		turn.getTurnNumber() + "\",\"" + turn.getSpeaker() + "\"," + getGateSpeechAct() +
		",[" + generateBareTriple()).toLowerCase() + "]).";
	}
	
	public static void writeEnums(String filename)
	{
		BufferedWriter bufOut;
		PrintWriter enumWriter = null;
		try {
			bufOut = new BufferedWriter(new FileWriter(filename));
	        enumWriter = new PrintWriter(bufOut);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			
			return;
		}
		enumWriter.println("Speech Acts");
		enumWriter.println();
		for (String speechAct : speechActs)
		{
			enumWriter.print("\'" + speechAct + "\',");
		}
		enumWriter.println("Ont classes");
		for (String ontClass : ontologicalClasses)
		{
			enumWriter.print("\'" + ontClass + "\',");
		}
		enumWriter.println("Roles");
		for (String role : roles)
		{
			enumWriter.print("\'" + role + "\',");
		}
		enumWriter.close();
	}
	
	public String convertVariableNameToProlog(String variableName)
	{
		return "var(" + variableName.split("::V")[1] + ")";
	}
	
	
	public String getGateSpeechAct()
	{
		String speechAct = getSpeechAct();
		
		if (speechAct.equals("ONT::ANSWER"))
		{
			KQMLList hypsList = (KQMLList)(logicalForm.getKeywordArg(":HYPS"));
			if (hypsList != null && hypsList.get(0) != null && hypsList.get(0) instanceof KQMLList)
			{
				KQMLObject posNegValue = ((KQMLList)hypsList.get(0)).getKeywordArg("ONT::WHAT");
				
				if (posNegValue != null)
				{
					if (posNegValue.equals("ONT::POS"))
						return "[yan]";
					if (posNegValue.equals("ONT::NEG"))
						return "[noa]";
				}
			}			
		}
		
		if (speechActMapping.containsKey(speechAct))
		{
			return speechActMapping.get(speechAct);
		}
		else
			return "[uin]";
	}
}
