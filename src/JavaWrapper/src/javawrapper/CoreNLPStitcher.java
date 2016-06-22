package javawrapper;

import java.util.List;

import corenlpwrapper.CoreNLPSentence;

public class CoreNLPStitcher {

	private Turn tripsTurn;
	private CoreNLPSentence coreNLPSentence;
	private List<LFTerm> stitchedLFGraph;
	
	public CoreNLPStitcher(Turn tripsTurn, CoreNLPSentence coreNLPSentence)
	{
		this.tripsTurn = tripsTurn;
		this.coreNLPSentence = coreNLPSentence;
	}
	
	public int getNumberOfFragments()
	{
		return tripsTurn.getNumberOfFragments();
	}
	
	public List<LFTerm> getStitchedLFGraph()
	{
		if (stitchedLFGraph == null)
			stitchLFGraph();
		
		return stitchedLFGraph;
	}
	
	private void stitchLFGraph()
	{
		
	}
}
