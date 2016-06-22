package corenlpwrapper;

import java.util.ArrayList;
import java.util.List;

public class CoreNLPSentence {

	private String text;
	private int id;
	private List<CoreNLPToken> tokens;
	
	public CoreNLPSentence(int id)
	{
		this.id = id;
		this.tokens = new ArrayList<CoreNLPToken>();
		this.text = null;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public int getId() {
		return id;
	}
	
	public void addToken(CoreNLPToken token)
	{
		tokens.add(token);
	}
	
	public int getCharacterOffsetBegin()
	{
		if (!tokens.isEmpty())
			return tokens.get(0).getCharacterOffsetBegin();
		
		return -1;
	}
	
	public int getCharacterOffsetEnd()
	{
		if (!tokens.isEmpty())
			return tokens.get(tokens.size()-1).getCharacterOffsetEnd();
		
		return -1;
	}
	
	public CoreNLPToken getToken(int id)
	{
		return tokens.get(id);
	}
	
	
	
	
}
