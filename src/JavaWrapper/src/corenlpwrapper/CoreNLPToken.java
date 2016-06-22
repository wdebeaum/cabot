package corenlpwrapper;

import java.util.HashMap;

public class CoreNLPToken {
	private int id;
	private String word;
	private String lemma;
	private int characterOffsetBegin;
	private int characterOffsetEnd;
	private String pos;
	private String ner;
	private String speaker;
	
	// Map is dependency type -> dependent
	private HashMap<String,CoreNLPToken> basicDependencies;
	private HashMap<String,CoreNLPToken> collapsedDependencies;
	private HashMap<String,CoreNLPToken> collapsedCCProcessedDependencies;
	
	private HashMap<String,CoreNLPToken> reversedBasicDependencies;
	private HashMap<String,CoreNLPToken> reversedCollapsedDependencies;
	private HashMap<String,CoreNLPToken> reversedCollapsedCCProcessedDependencies;
	
	public CoreNLPToken(int id, String word, String lemma, int characterOffsetBegin, 
		int characterOffsetEnd,	String pos,	String ner,	String speaker)
	{
		this.id = id;
		this.word = word;
		this.lemma = lemma;
		this.characterOffsetBegin = characterOffsetBegin;
		this.characterOffsetEnd = characterOffsetEnd;
		this.pos = pos;
		this.ner = ner;
		this.speaker = speaker;
	}
	
	public String getWord() {
		return word;
	}
	public String getLemma() {
		return lemma;
	}
	public int getCharacterOffsetBegin() {
		return characterOffsetBegin;
	}
	public int getCharacterOffsetEnd() {
		return characterOffsetEnd;
	}
	public String getPos() {
		return pos;
	}
	public String getNer() {
		return ner;
	}
	public String getSpeaker() {
		return speaker;
	}
	public HashMap<String, CoreNLPToken> getBasicDependencies() {
		return basicDependencies;
	}
	public HashMap<String, CoreNLPToken> getCollapsedDependencies() {
		return collapsedDependencies;
	}
	public HashMap<String, CoreNLPToken> getCollapsedCCProcessedDependencies() {
		return collapsedCCProcessedDependencies;
	}

	public HashMap<String, CoreNLPToken> getReversedBasicDependencies() {
		return reversedBasicDependencies;
	}

	public HashMap<String, CoreNLPToken> getReversedCollapsedDependencies() {
		return reversedCollapsedDependencies;
	}

	public HashMap<String, CoreNLPToken> getReversedCollapsedCCProcessedDependencies() {
		return reversedCollapsedCCProcessedDependencies;
	}
	
	public void addDependency(String dependencyProcessingType, String dependencyType,
								CoreNLPToken dependent)
	{
		if (dependencyProcessingType.equals("basic-dependencies"))
			basicDependencies.put(dependencyType,dependent);
		else if (dependencyProcessingType.equals("collapsed-dependencies"))
			collapsedDependencies.put(dependencyType, dependent);
		else if (dependencyProcessingType.equals("collapsed-ccprocessed-dependencies"))
			collapsedCCProcessedDependencies.put(dependencyType, dependent);
		
		dependent.addReverseDependency(dependencyProcessingType, dependencyType, this);
	}
	
	public void addReverseDependency(String dependencyProcessingType, String dependencyType,
			CoreNLPToken dependent)
{
		if (dependencyProcessingType.equals("basic-dependencies"))
			reversedBasicDependencies.put(dependencyType,dependent);
		else if (dependencyProcessingType.equals("collapsed-dependencies"))
			reversedCollapsedDependencies.put(dependencyType, dependent);
		else if (dependencyProcessingType.equals("collapsed-ccprocessed-dependencies"))
			reversedCollapsedCCProcessedDependencies.put(dependencyType, dependent);

}
	
}
