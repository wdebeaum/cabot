package corenlpwrapper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import javawrapper.Dialogue;
import javawrapper.DialogueIndexer;
import javawrapper.Turn;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.xml.sax.XMLReader;

import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.trees.TypedDependency;

public class CoreNLPXmlProcessor {

	private String serialDataDirectory;
	private String baseDataDirectory;
	private DialogueIndexer dialogueIndexer;
	
	public CoreNLPXmlProcessor(DialogueIndexer dialogueIndexer, String serialDataDirectory, 
			String baseDataDirectory)
	{
		this.serialDataDirectory = serialDataDirectory;
		this.baseDataDirectory = baseDataDirectory;
		this.dialogueIndexer = dialogueIndexer;
	}
	
    /**
     * Checks to see if the file exists serialized as a .ser file, otherwise
     * reads it from XML
     * @throws FileNoFoundException if the specified file does not exist
     **/
    public List<String> readSentencesFromFile(File file) throws FileNotFoundException
    {
		System.out.println(file.getAbsolutePath());
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
		factory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);
		XMLStreamReader reader = null;
		FileInputStream fis = null;

		try {
			fis = new FileInputStream(file);
			reader = factory.createXMLStreamReader(fis);
		}
		catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
		StringBuilder currentSentence = new StringBuilder();
		int lastOffset = -1;
		String docId;
		int tokenId = 0;
		int sentenceId = 0;
		String currentWord = "";
		List<String> sentenceList = new LinkedList<String>();
		
		// Begin parsing XML
		try {
			int dialogueNumber = 0;
			while (reader.hasNext()) {
				switch (reader.next()) {
					case XMLStreamConstants.START_ELEMENT:
						if (reader.getLocalName().equals("document"))
						{

						}
						else if (reader.getLocalName().equals("docId"))
						{
							docId = readCharacters(reader);
						}
						else if (reader.getLocalName().equals("sentence"))
						{
							sentenceId = Integer.parseInt((reader.getAttributeValue(null, "id")));
						}
						else if (reader.getLocalName().equals("token"))
						{
							tokenId = Integer.parseInt((reader.getAttributeValue(null, "id")));
						}
						else if (reader.getLocalName().equals("word"))
						{
							currentWord = readCharacters(reader);
						}
						else if (reader.getLocalName().equals("CharacterOffsetBegin"))
						{
							int beginOffset = Integer.parseInt(readCharacters(reader));
							if (tokenId > 1 && (beginOffset - lastOffset > 0))
								currentSentence.append(" ");
							
							currentSentence.append(currentWord);
						}
						else if (reader.getLocalName().equals("CharacterOffsetEnd"))
						{
							lastOffset = Integer.parseInt(readCharacters(reader));
						}
						break;
					case XMLStreamConstants.END_ELEMENT:
						if (reader.getLocalName().equals("sentence"))
						{
							sentenceList.add(currentSentence.toString());
							currentSentence = new StringBuilder();
						}						
				}
				dialogueNumber++;
			}
			reader.close();
		} catch (NumberFormatException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			if (fis != null)
				fis.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return sentenceList;
    }
    
    public Dialogue readDialogueFromFile(File file)
    {
		System.out.println(file.getAbsolutePath());
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
		factory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.TRUE);
		XMLStreamReader reader = null;
		FileInputStream fis = null;
		
		Dialogue dialogue = new Dialogue();
		
		try {
			fis = new FileInputStream(file);
			reader = factory.createXMLStreamReader(fis);
		}
		catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		StringBuilder currentSentenceStringBuilder = new StringBuilder();
		int lastOffset = -1;
		String docId;
		int tokenId = 0;
		int sentenceId = 0;
		CoreNLPSentence currentCoreNLPSentence = null;
		List<String> sentenceList = new LinkedList<String>();
		
		// Begin parsing XML
		try {
			int dialogueNumber = 0;
			while (reader.hasNext()) {
				switch (reader.next()) {
					case XMLStreamConstants.START_ELEMENT:
						if (reader.getLocalName().equals("docId"))
						{
							docId = readCharacters(reader);
						}
						else if (reader.getLocalName().equals("sentence"))
						{
							sentenceId = Integer.parseInt((reader.getAttributeValue(null, "id")));
							currentCoreNLPSentence = new CoreNLPSentence(sentenceId);
						}
						else if (reader.getLocalName().equals("token"))
						{
							tokenId = Integer.parseInt((reader.getAttributeValue(null, "id")));
							CoreNLPToken token = readToken(reader, tokenId); 
							currentCoreNLPSentence.addToken(token);
							
							// See if there should be a space when appending the token
							if (tokenId > 1 && (token.getCharacterOffsetBegin() - lastOffset > 0))
								currentSentenceStringBuilder.append(" ");
							
							currentSentenceStringBuilder.append(token.getWord());
							
							lastOffset = token.getCharacterOffsetEnd();
						}
						else if (reader.getLocalName().equals("dependencies"))
						{
							String dependencyProcessingType = reader.getAttributeValue(null,"type");
							readDependenciesAndUpdateTokens(reader, dependencyProcessingType, 
									currentCoreNLPSentence);
						}
						break;
					case XMLStreamConstants.END_ELEMENT:
						if (reader.getLocalName().equals("sentence"))
						{
							sentenceList.add(currentSentenceStringBuilder.toString());
							currentCoreNLPSentence.setText(currentSentenceStringBuilder.toString());
							currentSentenceStringBuilder = new StringBuilder();
						}						
				}
				dialogueNumber++;
			}
			reader.close();
		} catch (NumberFormatException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			if (fis != null)
				fis.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		return dialogue;    	
    }
    
    private Turn parseTurn(XMLStreamReader reader)
    {
    	return null;
    }
  

	public List<String> readSentencesFromFilesInDirectory(String directoryString)
	{
		if (directoryString == null)
			return null;
		
		File directory = new File(directoryString);
		
		if (!directory.exists())
			return null;
		
		List<String> sentenceList = new LinkedList<String>();
		
		for (File file : directory.listFiles())
		{
			try{
				sentenceList.addAll(readSentencesFromFile(file));
			}
			catch (FileNotFoundException e)
			{
				e.printStackTrace();
			}
		}
		System.out.println("XML files loaded");

		return sentenceList;
	}
	
	private CoreNLPToken readToken(XMLStreamReader reader, int id) throws XMLStreamException {
		
		String word = null;
		String lemma = null;
		int characterOffsetBegin = -1;
		int characterOffsetEnd = -1;
		String pos = null;
		String ner = null;
		String speaker = null;
		
		while (reader.hasNext())
		{
			int eventType = reader.next();
            switch (eventType) {
            	case XMLStreamReader.START_ELEMENT:
					if (reader.getLocalName().equals("word"))
						word = readCharacters(reader);
					else if (reader.getLocalName().equals("lemma"))
						lemma = readCharacters(reader);
					else if (reader.getLocalName().equals("CharacterOffsetBegin"))
						characterOffsetBegin = Integer.parseInt(readCharacters(reader));
					else if (reader.getLocalName().equals("CharacterOffsetEnd"))
						characterOffsetEnd = Integer.parseInt(readCharacters(reader));
					else if (reader.getLocalName().equals("pos"))
						pos = readCharacters(reader);
					else if (reader.getLocalName().equals("ner"))
						ner = readCharacters(reader);
					else if (reader.getLocalName().equals("speaker"))
						speaker = readCharacters(reader);
					break;
            	case XMLStreamReader.END_ELEMENT:
            		
            		// Using this in case Stanford changes CoreNLP Token order in XML
            		if (reader.getLocalName().equals("token"))
            		{
            			return new CoreNLPToken(id, word, lemma, characterOffsetBegin,
            					characterOffsetEnd, pos, ner, speaker);
            		}
            		break;
            }
		}
		
		return null;
	}
	
	private IndexedWord readIndexedWord(XMLStreamReader reader, int id) throws XMLStreamException {
		
		String word = null;
		String lemma = null;
		int characterOffsetBegin = -1;
		int characterOffsetEnd = -1;
		String pos = null;
		String ner = null;
		String speaker = null;
		
		while (reader.hasNext())
		{
			int eventType = reader.next();
            switch (eventType) {
            	case XMLStreamReader.START_ELEMENT:
					if (reader.getLocalName().equals("word"))
						word = readCharacters(reader);
					else if (reader.getLocalName().equals("lemma"))
						lemma = readCharacters(reader);
					else if (reader.getLocalName().equals("CharacterOffsetBegin"))
						characterOffsetBegin = Integer.parseInt(readCharacters(reader));
					else if (reader.getLocalName().equals("CharacterOffsetEnd"))
						characterOffsetEnd = Integer.parseInt(readCharacters(reader));
					else if (reader.getLocalName().equals("pos"))
						pos = readCharacters(reader);
					else if (reader.getLocalName().equals("ner"))
						ner = readCharacters(reader);
					else if (reader.getLocalName().equals("speaker"))
						speaker = readCharacters(reader);
					break;
            	case XMLStreamReader.END_ELEMENT:
            		
            		// Using this in case Stanford changes CoreNLP Token order in XML
            		if (reader.getLocalName().equals("token"))
            		{
            			CoreLabel cl = new CoreLabel();
            			cl.setWord(word);
            			cl.setLemma(lemma);
            			cl.setSentIndex(id);
            			cl.setBeginPosition(characterOffsetBegin);
            			cl.setEndPosition(characterOffsetEnd);
            			cl.setNER(ner);
            			cl.setCategory(pos); // Not sure about this, documentation isn't clear
            			IndexedWord toReturn = new IndexedWord(cl);
            			
            			return toReturn;
            		}
            		break;
            }
		}
		
		return null;
	}
	
	private void readDependenciesAndReturnGraph(XMLStreamReader reader, 
			String dependencyProcessingType,
			CoreNLPSentence sentence) throws XMLStreamException
	{
		String currentDependencyType = null;
		int currentGovernor = -1;
		int currentDependent = -1;

		SemanticGraph semanticGraph = new SemanticGraph();
		
		
		while (reader.hasNext())
		{
			int eventType = reader.next();
            switch (eventType) {
            	case XMLStreamReader.START_ELEMENT:
					if (reader.getLocalName().equals("dep"))
						currentDependencyType = reader.getAttributeValue(null, "type");
					else if (reader.getLocalName().equals("governor"))
						currentGovernor = Integer.parseInt(reader.getAttributeValue(null, "idx"));
					else if (reader.getLocalName().equals("dependent"))
						currentDependent = Integer.parseInt(reader.getAttributeValue(null, "idx"));
					break;
            	case XMLStreamReader.END_ELEMENT:
            		if (reader.getLocalName().equals("dep"))
            		{
            			sentence.getToken(currentGovernor).addDependency(
            					dependencyProcessingType,
            					currentDependencyType, 
            					sentence.getToken(currentDependent));
            			
            		}
            		else if (reader.getLocalName().equals("dependencies"))
            			return;
            		break;
            }
		}		
		
	}
	
	private void readDependenciesAndUpdateTokens(XMLStreamReader reader, 
												String dependencyProcessingType,
												CoreNLPSentence sentence) throws XMLStreamException
	{
		String currentDependencyType = null;
		int currentGovernor = -1;
		int currentDependent = -1;
		
		
		while (reader.hasNext())
		{
			int eventType = reader.next();
            switch (eventType) {
            	case XMLStreamReader.START_ELEMENT:
					if (reader.getLocalName().equals("dep"))
						currentDependencyType = reader.getAttributeValue(null, "type");
					else if (reader.getLocalName().equals("governor"))
						currentGovernor = Integer.parseInt(reader.getAttributeValue(null, "idx"));
					else if (reader.getLocalName().equals("dependent"))
						currentDependent = Integer.parseInt(reader.getAttributeValue(null, "idx"));
					break;
            	case XMLStreamReader.END_ELEMENT:
            		if (reader.getLocalName().equals("dep"))
            		{
            			sentence.getToken(currentGovernor).addDependency(
            					dependencyProcessingType,
            					currentDependencyType, 
            					sentence.getToken(currentDependent));
            			
            		}
            		else if (reader.getLocalName().equals("dependencies"))
            			return;
            		break;
            }
		}		
	}
	

	
	
    private String readCharacters(XMLStreamReader reader) throws XMLStreamException {
        StringBuilder result = new StringBuilder();
        while (reader.hasNext()) {
            int eventType = reader.next();
            switch (eventType) {
                case XMLStreamReader.CHARACTERS:
                case XMLStreamReader.CDATA:
                    result.append(reader.getText());
                    break;
                case XMLStreamReader.END_ELEMENT:
                    return result.toString();
            }
        }
        throw new XMLStreamException("Premature end of file");
    }
	
}
