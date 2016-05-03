package owlground.language.dialogue;

import javax.xml.stream.*;

import owlground.utilities.Utilities;

import java.io.Serializable;
import java.util.*;
import java.text.*;

import TRIPS.KQML.*;

public class Turn implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 3L;

	String plainText;
	String fullText;
	String trimmedText;
	int turnNumber;
	String startTime;
	String speaker;
	Dialogue dialogue;
	ArrayList<Word> words;
	ArrayList<KQMLList> logicalForms;
	ArrayList<String> sentences;
	ArrayList<KQMLList> speechActLogicalForms;
	ArrayList<KQMLList> speechActHypsLogicalForms;


	String syntaxTree;
	String expression = null;
	boolean parsed = false;
	
	public Turn(Dialogue dialogue, int turnNumber, String startTime, String speaker)
	{
		words = new ArrayList<Word>();
		this.turnNumber = turnNumber;
		this.startTime = startTime;
		this.speaker = speaker;
		this.dialogue = dialogue;
		this.logicalForms = new ArrayList<KQMLList>();
		this.speechActLogicalForms = new ArrayList<KQMLList>();
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
										dialogue.addWordNetSenseMapping(variableName, wordNetName);
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

	public List<LFTerm> generateLFTerms()
	{
		List<LFTerm> result = new ArrayList<LFTerm>();
		int termId = 0;
		StringBuilder expressionBuilder = new StringBuilder();
		expressionBuilder.append("turn(" + turnNumber + ",\"" + speaker + "\",[");
		
		
		HashMap<String, ArrayList<LFTerm>> sentenceMapping = new HashMap<String, ArrayList<LFTerm>>();

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
					for (int i = 3; i < oList.size(); i += 2)
					{
						term.addProperty(oList.get(i).stringValue(), oList.get(i+1));
					}
					String sentence = getSentence(logicalForm);
					
					if (!sentenceMapping.containsKey(sentence))
						sentenceMapping.put(sentence, new ArrayList<LFTerm>());
					sentenceMapping.get(sentence).add(term);
					//listCount++;
					result.add(term);
					dialogue.addLFTerm(term);		
				}
			}	
		}
		
		return result;
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
	
	
	
	private String indentKQML(KQMLObject kObject, int depth)
	{
		
		StringBuilder indentString = new StringBuilder();
		for (int i = 0; i < depth; i++)
			indentString.append("    ");
		if (!(kObject instanceof KQMLList) || ((KQMLList)kObject).size() < 2)
		{
			if (kObject.stringValue().charAt(0) == ':' && !kObject.stringValue().equalsIgnoreCase(":CONTEXT"))
				return indentString.toString() + kObject.stringValue();
			
			return indentString.toString() + kObject.stringValue() + "\n";
		}
			
		
		StringBuilder result = new StringBuilder();
		KQMLList kList = (KQMLList)kObject;
		result.append("(");
		for (KQMLObject ko : kList)
		{
			result.append(indentKQML(ko,depth+1));
		}
		result.append(")");
		
		return result.toString();
		 
	}
	
	public String getWordSense(String variableName)
	{
		return dialogue.getVariableSense(variableName);
	}

	public boolean isParsed() {
		return parsed;
	}
	
	/**
	 * Get the sentence associated with this logical form
	 * @param logicalForm
	 * @return
	 */
	public static String getSentence(KQMLList logicalForm)
	{
		StringBuilder sentenceBuilder = new StringBuilder();
		
		KQMLObject wordsObject = logicalForm.getKeywordArg(":WORDS");
		if (wordsObject == null || !(wordsObject instanceof KQMLList))
			return "";
		
		int count = 0;
		KQMLList wordsList = (KQMLList)wordsObject;
		for (KQMLObject kWord : wordsList)
		{
			String word = kWord.stringValue();
			if (kWord.stringValue().contains("::"))
				word = kWord.stringValue().split("::")[1].toLowerCase();

			if (word.contains("punc-"))
				word = "(" + word + ")";
			
			sentenceBuilder.append(word);
			
			count++;
			
			if (count < wordsList.size())
				sentenceBuilder.append(" ");
		}
		
		return sentenceBuilder.toString();
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
	
	
}
