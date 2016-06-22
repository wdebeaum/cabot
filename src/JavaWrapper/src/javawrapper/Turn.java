package javawrapper;

import javax.xml.stream.*;

import java.io.Serializable;
import java.util.*;
import java.util.Map.Entry;
import java.text.*;

import TRIPS.KQML.*;

public class Turn implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 3L;

	protected String plainText;
	protected String fullText;
	protected String trimmedText;
	protected int turnNumber;
	protected String startTime;
	protected String speaker;
	protected Dialogue dialogue;
	protected ArrayList<Word> words;
	protected ArrayList<KQMLList> logicalForms;
	protected ArrayList<String> sentences;
	protected ArrayList<KQMLList> speechActLogicalForms;
	protected ArrayList<KQMLList> speechActHypsLogicalForms;
	protected ArrayList<LFTerm> lfTerms;
	
	protected HashMap<String, ArrayList<LFTerm>> sentenceMapping ;

	// A reverse mapping from variable ID to the set of variable IDs that point to it
	protected HashMap<String, HashSet<LFTerm>> reversedEdges;
	protected String syntaxTree;
	protected String expression = null;
	boolean parsed = false;
	protected boolean lfTermsGenerated = false;
	
	public Turn(Dialogue dialogue)
	{
		this(dialogue,-1,null,null);
	}
	
	public Turn(Dialogue dialogue, int turnNumber, String startTime, String speaker)
	{
		words = new ArrayList<Word>();
		this.turnNumber = turnNumber;
		this.startTime = startTime;
		this.speaker = speaker;
		this.dialogue = dialogue;
		this.logicalForms = new ArrayList<KQMLList>();
		this.speechActLogicalForms = new ArrayList<KQMLList>();
		this.sentenceMapping = new HashMap<String, ArrayList<LFTerm>>();
		this.lfTerms = new ArrayList<LFTerm>();
		this.reversedEdges = new HashMap<String,HashSet<LFTerm>>();
		this.syntaxTree = null;
	}
	
	public static Turn fromXML(XMLStreamReader reader, Dialogue dialogue) throws XMLStreamException
	{
		String text = null;
		Turn t = null;
		while (reader.hasNext()) {
			switch (reader.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					if (reader.getLocalName().equals("turn"))
					{
						String startTime = reader.getAttributeValue(null,"start");
						String speaker = reader.getAttributeValue(null,"speaker");
						int turnNumber = Integer.parseInt(reader.getAttributeValue(null,"turn_no"));
						t = new Turn(dialogue, turnNumber, startTime, speaker);
						t.processText(reader);
						break;
					}
				case XMLStreamConstants.END_ELEMENT:
					if (reader.getLocalName().equals("turn"))
					{
						dialogue.addTurn(t);
						return t;
					}
			}
		}		
		return null;
	}
	
	public void processText(XMLStreamReader reader) throws XMLStreamException
	{
		StringBuffer plainText = new StringBuffer();
		while (reader.hasNext()) {
			switch (reader.next()) {
				case XMLStreamConstants.START_ELEMENT:
					if (!reader.getLocalName().equals("turn"))
					{
						String wordClass = reader.getLocalName();
						String type = reader.getAttributeValue(null,"type");
						StringBuffer wordText = new StringBuffer();
						
						while (reader.next() != XMLStreamConstants.END_ELEMENT &&
								reader.hasText())
							wordText.append(reader.getText());
						
						plainText.append(wordText);
						Word w = new Word(wordText.toString(),wordClass,type);
						words.add(w);
					}
					break;
					
				case XMLStreamConstants.END_ELEMENT:
					if (reader.getLocalName().equals("turn"))
					{
						this.plainText = plainText.toString();
						return;
					}
					break;
				
				case XMLStreamConstants.CHARACTERS:
					String text = reader.getText();
					for (String word : text.split("\\s+"))
					{
						words.add(new Word(word,null,null));
						
					}
					plainText.append(reader.getText());
					
					break;

				case XMLStreamConstants.SPACE:
					plainText.append(reader.getText());
					break;
			}
		}
	}
	
	public String toString()
	{
		String result = "";
		result += "Turn no. " + turnNumber;
		result += " Speaker: " + speaker;
		result += " Start time: " + startTime + "\n";
		result += " Plain: " + plainText + "\n";
		result += " Words: ";
		
		for (Word w : words)
		{
			String wordClass = w.wordClass;
			if (wordClass == null)
				wordClass = "";
			result += "(" + w.text + ")/" + wordClass + " ";
		}
		result += "\n";
		return result;
	}
	
	public String getCleanText()
	{
		StringBuilder result = new StringBuilder();
		for (Word w : words)
		{
			if (w.wordClass != null && w.wordClass.equalsIgnoreCase("sic") && 
					w.type != null && w.type.equalsIgnoreCase("fragment"))
				continue;
			
			result.append(w.text);
			result.append(" ");
		}
		
		return result.toString();
	}
	
	public String getCleanAndEscapedText()
	{
		return quoteEscaped(getCleanText());
	}
	
	private static String quoteEscaped(String input)
	{
		return input.replace("\\","\\\\").replace("\"", "\\\"").replaceAll("[\\p{Z}\\s]", " ").trim();
	}
	
	public String getPlainText() {
		return plainText;
	}

	public int getTurnNumber() {
		return turnNumber;
	}

	public String getStartTime() {
		return startTime;
	}

	public String getSpeaker() {
		return speaker;
	}


	public Dialogue getDialogue() {
		return dialogue;
	}

	public List<Word> getWords() {
		return Collections.unmodifiableList(words);
	}

	public List<KQMLList> getLogicalForms() {
		return Collections.unmodifiableList(logicalForms);
	}
	
	public void addLogicalForm(KQMLList logicalForm) {
		this.logicalForms.add(logicalForm);
	}
	
	public List<KQMLList> getSpeechActLogicalForms() {
		return Collections.unmodifiableList(speechActLogicalForms);
	}
	
	public void addSpeechActLogicalForm(KQMLList logicalForm) {
		this.speechActLogicalForms.add(logicalForm);
		
	}
	
	public void addSpeechActHypsLogicalForm(KQMLList logicalForm)
	{
		if (speechActHypsLogicalForms == null)
			speechActHypsLogicalForms = new ArrayList<KQMLList>();
		
		speechActHypsLogicalForms.add(logicalForm);
		generateWordNetMapping();
	}

	
	/**
	 * Parses the logical form from the parser to extract WordNet mappings
	 */
	private void generateWordNetMapping()
	{
		for (KQMLList logicalForm : speechActHypsLogicalForms)
		{
			if (!(logicalForm instanceof KQMLList))
				continue;
			
			ArrayList<KQMLList> uttList = new ArrayList<KQMLList>();
			
			// Utts can either be in a list by themselves, or part of compound-communication-act's
			// This lumps them all together in one list
			for (KQMLObject uttOrCompound : logicalForm)
			{
				if (!(uttOrCompound instanceof KQMLList))
					continue; 
				
				if (((KQMLList)uttOrCompound).get(0).stringValue().equalsIgnoreCase("COMPOUND-COMMUNICATION-ACT"))
				{
					uttList.addAll(Arrays.asList(((KQMLList)(((KQMLList)uttOrCompound).getKeywordArg(":ACTS"))).toArray(new KQMLList[0])));
				}
				else
				{
					uttList.add((KQMLList)uttOrCompound);
				}
			}

			for (KQMLList utt : uttList)
			{
				KQMLObject termsObject = utt.getKeywordArg(":TERMS");
				if (termsObject == null || !(termsObject instanceof KQMLList))
					continue;
				
				KQMLList terms = (KQMLList)termsObject;
				
				for (KQMLObject term : terms)
				{
					String variableName = ((KQMLList)term).getKeywordArg(":VAR").stringValue();
					if (!(((KQMLList)term).getKeywordArg(":SEM") instanceof KQMLList))
						continue;
					KQMLList semList = (KQMLList)((KQMLList)term).getKeywordArg(":SEM");
					String wordNetName = null;
					
					semfeatures:
					for (KQMLObject semFeature : semList)
					{
						if (semFeature instanceof KQMLList)
						{
							KQMLList semFeatureList = (KQMLList)semFeature;
							if (semFeatureList.get(0).stringValue().equalsIgnoreCase("F::KR-TYPE"))
							{
								if (!(semFeatureList.get(1) instanceof KQMLList))
									continue;
								
								for (KQMLObject kr : (KQMLList)(semFeatureList.get(1)))
								{
									if (kr instanceof KQMLList && ((KQMLList)kr).get(0).stringValue().equals("WN"))
									{
										wordNetName = ((KQMLList)((KQMLList)kr).get(1)).get(0).stringValue();
										dialogue.getWordNetMapper().addWordNetSenseMapping(variableName, wordNetName);
										//System.out.println(variableName + " : " + wordNetName);
										break semfeatures;
									}
								}
							}
						}
					}
				}
				
			}
		}
	}
	
	public String getSyntaxTree() {
		return syntaxTree;
	}

	public void setSyntaxTree(String syntaxTree) {
		this.syntaxTree = syntaxTree;
	}
	
	public String getExpression()
	{
		return expression;
	}
	

	
	public List<LFTerm> getLFTerms()
	{
		if (!lfTermsGenerated)
		{
			if (!logicalForms.isEmpty())
				generateLFTerms();
			else
				generateLFTermsFromLogSpeechAct();
		}
			
		return lfTerms;
	}

	public void generateLFTerms()
	{
		List<LFTerm> result = new ArrayList<LFTerm>();
		int termId = 0;

		for (KQMLList logicalForm : logicalForms)
		{
			if (!(logicalForm.getKeywordArg(":CONTEXT") instanceof KQMLList))
				continue;

			for (KQMLObject kObj : (KQMLList)logicalForm.getKeywordArg(":CONTEXT"))
			{
				if (kObj instanceof KQMLList)
				{
					KQMLList oList = (KQMLList)kObj;
					LFTerm term = new LFTerm(oList.get(0).stringValue(),
											oList.get(1).stringValue(),
											oList.get(2).stringValue(),
											this);
					term.setTermId(termId);
					termId++;
					term.setLogicalForm(logicalForm);
					
					// Get the arguments of the LF term
					for (int i = 3; i < oList.size(); i += 2)
					{
						term.addProperty(oList.get(i).stringValue(), oList.get(i+1));
					}
					String sentence = Utilities.getSentence(logicalForm);
					
					if (!sentenceMapping.containsKey(sentence))
						sentenceMapping.put(sentence, new ArrayList<LFTerm>());
					sentenceMapping.get(sentence).add(term);
					//listCount++;
					result.add(term);
					dialogue.addLFTerm(term);	
					
				}
			}	
		}
		lfTerms.addAll(result);
		lfTermsGenerated = true;
		
	}
	
	public void generateLFTermsFromLogSpeechAct()
	{
		List<LFTerm> result = new ArrayList<LFTerm>();
		int termId = 0;

		for (KQMLList logicalForm : speechActLogicalForms)
		{
			if (!(logicalForm.getKeywordArg(":LF") instanceof KQMLList))
				continue;

			for (KQMLObject kObj : (KQMLList)logicalForm.getKeywordArg(":LF"))
			{
				if (kObj instanceof KQMLList)
				{
					KQMLList oList = (KQMLList)kObj;
					LFTerm term = new LFTerm(oList.get(0).stringValue(),
											oList.get(1).stringValue(),
											oList.get(2).stringValue(),
											this);
					term.setTermId(termId);
					termId++;
					term.setLogicalForm(logicalForm);
					
					// Get the arguments of the LF term
					for (int i = 3; i < oList.size(); i += 2)
					{
						term.addProperty(oList.get(i).stringValue(), oList.get(i+1));
					}
					String sentence = Utilities.getSentenceFromList(
							(KQMLList)(logicalForm.getKeywordArg(":INPUT")));
					System.out.println("Processing LF: " + term);
					if (!sentenceMapping.containsKey(sentence))
						sentenceMapping.put(sentence, new ArrayList<LFTerm>());
					sentenceMapping.get(sentence).add(term);
					//listCount++;
					result.add(term);
					dialogue.addLFTerm(term);		
				}
			}	
		}
		lfTerms.addAll(result);
		lfTermsGenerated = true;
		
	}

	public class Word implements Serializable
	{
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;
		String text;
		String type;
		String wordClass;
		
		public Word(String text, String wordClass, String type)
		{
			this.text = text;
			this.wordClass = wordClass;
			this.type = type;
		}
	}

	
	public String getPrettyPrintLogicalForm()
	{	
		StringBuilder result = new StringBuilder();
		if (logicalForms.isEmpty())
			return "";
		
		for (KQMLList logicalForm : logicalForms)
		{
			if (!(logicalForm.getKeywordArg(":CONTEXT") instanceof KQMLList))
				continue;
			for (KQMLObject kObj : (KQMLList)logicalForm.getKeywordArg(":CONTEXT"))
			{
				if (kObj instanceof KQMLList)
				{
					KQMLList oList = (KQMLList)kObj;
					result.append(oList.subList(0, 3).stringValue() + "\n");
					for (int i = 3; i < oList.size(); i += 2)
					{
						result.append("    " + oList.get(i).stringValue() + " " + 
								oList.get(i+1).stringValue() + "\n");
					}
				}
			}
		}
		
		return result.toString();
	}
	
	
	public String getWordSense(String variableName)
	{
		return dialogue.getWordNetMapper().getVariableSense(variableName);
	}

	public boolean isParsed() {
		return parsed;
	}
	


	// Returns true if this was not already parsed, false otherwise
	public boolean setParsed(boolean parsed) {
		if (this.parsed)
			return false;
		
		this.parsed = parsed;
		
		return true;
	}
	
	// http://stackoverflow.com/questions/2687012/split-string-into-sentences-based-on-periods
	public List<String> getCleanAndSplitText()
	{
		String cleanText = getCleanText();
		ArrayList<String> splitSentences = new ArrayList<String>();
		
		BreakIterator bIterator = BreakIterator.getSentenceInstance(Locale.US);
		bIterator.setText(cleanText);
		
		int start = bIterator.first();
		
		for (int end = bIterator.next(); end != BreakIterator.DONE; 
				start = end, end = bIterator.next()) 
		{
			splitSentences.add(cleanText.substring(start,end));
		}
		sentences = splitSentences;
		return splitSentences;
	}
	
	private void generateReversedEdges()
	{
		reversedEdges = new HashMap<String, HashSet<LFTerm>>();
		for (LFTerm lfTerm : lfTerms)
		{
			for (Entry<String, KQMLObject> entry: lfTerm.getProperties().entrySet())
			{
				String variableName = entry.getValue().stringValue();
				if (LFTerm.isRole(entry.getKey()) && 
						LFTerm.isVariable(variableName))
				{
					if (reversedEdges.get(entry) == null)
						reversedEdges.put(variableName, new HashSet<LFTerm>());
					
					reversedEdges.get(variableName).add(lfTerm);
				}
			}
		}
	}
	
	// TODO Check for change?
	public HashMap<String, HashSet<LFTerm>> getReversedEdges()
	{
		if (reversedEdges == null)
			generateReversedEdges();
		return reversedEdges;
	}
	
	public int getNumberOfFragments()
	{		
		return getRootVariables().size();
	}
	
	public List<String> getRootVariables()
	{
		List<String> rootVariables = new ArrayList<String>();
		for (Entry<String, HashSet<LFTerm>> e : getReversedEdges().entrySet())
		{
			if (e.getValue() == null)
				rootVariables.add(e.getKey());
		}
		
		return rootVariables;
	}
	
	public List<LFTerm> getRootLFTerms()
	{
		List<LFTerm> rootLFTerms = new ArrayList<LFTerm>();
		for (String variableName : getRootVariables())
			rootLFTerms.add(dialogue.getLFTerm(variableName));
		
		return rootLFTerms;
	}
	
	public void setPlainText(String plainText)
	{
		this.plainText = plainText;
	}
	
	
}
