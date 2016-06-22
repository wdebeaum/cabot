package corenlpwrapper;

import java.util.HashMap;

import javawrapper.LFTerm;
import javawrapper.Turn;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.semgrex.SemgrexPattern;

public class SemanticGraphConverter {

	private HashMap<String, IndexedWord> nodeList;
	
	public SemanticGraphConverter()
	{
		nodeList = new HashMap<String, IndexedWord>();
	}
	
	public SemanticGraph getSemanticGraphFromTurn(Turn t)
	{
		SemanticGraph semanticGraph = new SemanticGraph();
		
		for (LFTerm lfTerm : t.getLFTerms())
		{
			System.out.println("LFTerm:");
			System.out.println(lfTerm);
			IndexedWord iw = getIndexedWordFromLFTerm(lfTerm);
			System.out.println("IndexedWord:");
			System.out.println(iw);
			semanticGraph.addVertex(iw);
		}
		
		
		
		for (LFTerm lfTerm : t.getLFTerms())
		{
			IndexedWord iw = getIndexedWordFromLFTerm(lfTerm);
		}
		
		return semanticGraph;
	}
	
	private CoreLabel getCoreLabelFromLFTerm(LFTerm lfTerm)
	{
		CoreLabel coreLabel = new CoreLabel();
		coreLabel.setWord(lfTerm.getConcept());
		coreLabel.setLemma(lfTerm.getWord());
		coreLabel.setBeginPosition(Integer.parseInt(
						lfTerm.getProperties().get(":START").stringValue()));
		coreLabel.setEndPosition(Integer.parseInt(
				lfTerm.getProperties().get(":END").stringValue()));
		coreLabel.setSentIndex(lfTerm.getTurn().getTurnNumber());
		coreLabel.setNER(lfTerm.getConceptParent());
		coreLabel.setCategory(lfTerm.getOntologicalClass());
		
		return coreLabel;
	}
	
	private IndexedWord getIndexedWordFromLFTerm(LFTerm lfTerm)
	{
		IndexedWord indexedWord = new IndexedWord(getCoreLabelFromLFTerm(lfTerm));
		return indexedWord;
	}
}
