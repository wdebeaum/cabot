package cubismwrapper;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javawrapper.Dialogue;
import javawrapper.LFTerm;
import javawrapper.Turn;
import javawrapper.Utilities;

public class CubismTurn extends Turn {
	
	
	public CubismTurn(Dialogue dialogue, int turnNumber, String startTime,
			String speaker) {
		super(dialogue, turnNumber, startTime, speaker);
		// TODO Auto-generated constructor stub
	}
	
	public List<LFTerm> getLFTerms()
	{
		if (!lfTermsGenerated)
			generateLFTerms();
		
		List<LFTerm> cubismLfTerms = new ArrayList<LFTerm>();
		
		for (LFTerm lf : lfTerms)
			cubismLfTerms.add((CubismLFTerm)lf);
		
		return cubismLfTerms;
	}

	public void generateExpression()
	{
		if (!lfTermsGenerated)
			generateLFTerms();
		
		StringBuilder expressionBuilder = new StringBuilder();
		expressionBuilder.append("turn(" + turnNumber + ",\"" + speaker + "\",[");
		
		int listCount = 0;
		
		int sentenceCount = 0;
		for (Map.Entry<String, ArrayList<LFTerm>> entry : sentenceMapping.entrySet())
		{
			
			String speechAct = "";
			
			for (LFTerm lft : entry.getValue())
			{
				String tempSpeechAct = lft.getSpeechAct();
				if (tempSpeechAct != null)
					speechAct = tempSpeechAct;
			}
			
			String sentence = Utilities.quoteEscaped(entry.getKey());
			if (sentence.trim().equals(""))
				sentence = getCleanAndEscapedText();
			
			StringBuilder sentenceBuilder = new StringBuilder();
			
			sentenceBuilder.append("sent(");
			sentenceBuilder.append("[\"" + sentence.trim().replace(" ","\",\"").replace("^","'") + "\"]"); //Sentence
			sentenceBuilder.append(",");
			sentenceBuilder.append("'" + speechAct + "'");
			sentenceBuilder.append(",[");
			
			
			int termCount = 0;
			for (LFTerm lft : entry.getValue())
			{
				if (!(lft instanceof CubismLFTerm))
					break;
				listCount++;
				sentenceBuilder.append(((CubismLFTerm)lft).generateExpressionTriple());
				termCount++;
				if (termCount < entry.getValue().size())
					sentenceBuilder.append(",");
			}
			
			sentenceBuilder.append("])");
			
			expressionBuilder.append(sentenceBuilder.toString());
			sentenceCount++;
			
			if (sentenceCount < sentenceMapping.size())
				expressionBuilder.append(",");
		}
		
		expressionBuilder.append("]).");
		if (listCount > 0)
			expression = expressionBuilder.toString();
		else
			expression = "";
	}
}
