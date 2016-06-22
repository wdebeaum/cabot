package javawrapper;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class Utilities {
	public static String quoteEscaped(String input)
	{
		return input.replace("\\","\\\\").replace("\"", "\\\"").replaceAll("[\\p{Z}\\s]", " ").trim();
	}
	
	/**
	 * Get the sentence associated with this logical form
	 * @param logicalForm
	 * @return
	 */
	public static String getSentence(KQMLList logicalForm)
	{
		KQMLObject wordsObject = logicalForm.getKeywordArg(":WORDS");
		if (wordsObject == null || !(wordsObject instanceof KQMLList))
			return "";
		
		return getSentenceFromList((KQMLList)wordsObject);
	}
	
	public static String getSentenceFromList(KQMLList wordsList)
	{
		StringBuilder sentenceBuilder = new StringBuilder();
		
		int count = 0;
		
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
	
	
	
	public static String indentKQML(KQMLObject kObject, int depth)
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
}
