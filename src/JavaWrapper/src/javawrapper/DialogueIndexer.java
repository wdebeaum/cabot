package javawrapper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DialogueIndexer {
	
	private int dialogueIndex = 0;
	private int turnIndex = 0;
	List<Dialogue> dialogues = new ArrayList<Dialogue>();
	Turn currentTurn = null;
	Dialogue currentDialogue = null;
	// Mapping from variable names to their corresponding LFTerms
	Map<String,LFTerm> termMapping = new HashMap<String,LFTerm>();
	private String serialDirectory;
	Dialogue interactiveDialogue;
	
	public DialogueIndexer(String serialDirectory)
	{
		this.serialDirectory = serialDirectory;
		this.interactiveDialogue = new Dialogue("interactive",-1);
		dialogues.add(interactiveDialogue);
	}
	
	/**
	 * Go through the list of turns to parse and return the next one
	 * @return The next turn to parse
	 */
	public Turn startParsingTurns()
	{
		dialogueIndex = 0;
		turnIndex = 0;
		
		if (dialogues.size() == 0)
		{
			System.out.println("No dialogues loaded");
			return null;
			//shutdown();
		}
		
		if (dialogues.get(dialogueIndex) == null)
			throw new RuntimeException("Corrupt dialogue file loaded");
		
		while (dialogueIndex < dialogues.size() - 1 && 
				(dialogues.get(dialogueIndex).getTurns().size() == 0 ||
				dialogues.get(dialogueIndex).isParsed()))
		{
			dialogueIndex++;
		}
		
		if (dialogues.get(dialogueIndex).getTurns().size() != 0)
		{
			currentDialogue = dialogues.get(dialogueIndex);
			currentTurn = currentDialogue.nextTurn();
			
			//sendTurnToParser(currentTurn);
			return currentTurn;
		}
		
		return null;
	}
	
	/**
	 * Advances the current dialogue to the next one
	 **/
	public boolean setCurrentDialogueToNext()
	{
		
		System.out.println("Moving to next dialogue");

	    currentDialogue.writeToFile(serialDirectory + currentDialogue.getBaseName() + ".ser");
		while (dialogueIndex < dialogues.size() - 1)
		{
			dialogueIndex++;
			currentDialogue = dialogues.get(dialogueIndex);
			if (currentDialogue == null || currentDialogue.getTurns().isEmpty())
				continue;
			System.out.println("New dialogue set to " + currentDialogue.getBaseName() +
					" at index " + dialogueIndex);
			turnIndex = 0;
			
			return true;
		}
		
		return false;
	}
	
	public void addDialogue(Dialogue d)
	{
		dialogues.add(d);
	}
	
	public Dialogue getCurrentDialogue()
	{
		if (currentDialogue == null)
			return interactiveDialogue;
		return currentDialogue;
	}
	
	public Dialogue getInteractiveDialogue()
	{
		return interactiveDialogue;
	}
}
