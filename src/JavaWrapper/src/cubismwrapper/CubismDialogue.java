package cubismwrapper;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import javawrapper.Dialogue;
import javawrapper.LFTerm;
import javawrapper.Turn;

import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import org.semanticweb.owlapi.model.OWLAxiom;

public class CubismDialogue extends Dialogue {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1901733717030302794L;
	// Default directory to write Gate triples
	private static final String TRIPLE_DIRECTORY = "../../data/tpl/";
	// Default directory to write Prolog-style TRIPS data
	private static final String EXP_DIRECTORY = "../../data/exp/";

	
	private static String prologServerBaseUrl = "";
	private static String prologServerAuthToken= ""; 
	private static boolean sendProlog = true;
	
	private transient PrintWriter tripleWriter;
	private transient PrintWriter expWriter;
	
	public static void readProperties(HashMap<String, String> properties)
	{
		if (properties.containsKey("prolog.server.baseurl"))
			prologServerBaseUrl = properties.get("prolog.server.baseurl"); 
		if (properties.containsKey("prolog.server.auth_token"))
			prologServerAuthToken = properties.get("prolog.server.auth_token"); 
		if (properties.containsKey("prolog.server.sendprolog"))
			sendProlog = Boolean.parseBoolean(properties.get("prolog.server.sendprolog").toLowerCase()); 
	}
	
	public void initTripleFile()
	{
		initTripleFile(TRIPLE_DIRECTORY);
	}
	
	public void initExpFile()
	{
		initExpFile(EXP_DIRECTORY);		
	}
	

	
	public void initExpFile(String directory)
	{
		BufferedWriter bufOut;
		try {
			File expFile = new File(directory + getBaseName() + ".exp");
			expFile.getAbsoluteFile().getParentFile().mkdirs();
			
			bufOut = new BufferedWriter(new FileWriter(expFile));
	        expWriter = new PrintWriter(bufOut);
	        System.out.println("Init Exp file to: " + expFile.getAbsoluteFile().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}				
	}
	
	
	/**
	 * Initializes the triple file for the GATE triples output
	 * @param directory
	 */
	public void initTripleFile(String directory)
	{
		BufferedWriter bufOut;
		try {
			File tripleFile = new File(directory + getBaseName() + ".tpl");
			tripleFile.getAbsoluteFile().getParentFile().mkdirs();
			bufOut = new BufferedWriter(new FileWriter(tripleFile));
	        tripleWriter = new PrintWriter(bufOut);
	        System.out.println("Init triple file to: " + tripleFile.getAbsoluteFile().toString());
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private void initFiles()
	{
		if (lfWriter == null)
			initDebugFile();
		
		if (tripleWriter == null)
			initTripleFile();
		
		if (expWriter == null)
			initExpFile();		
	}
	
	public void printTurnToFile(CubismTurn t)
	{
		initFiles();
		
		lfWriter.println(t.toString());
		lfWriter.println(t.getPrettyPrintLogicalForm());
		lfWriter.println(t.getExpression());
		List<LFTerm> terms = t.getLFTerms();
		lfWriter.println(terms.size() + " terms");
		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		for (LFTerm lft : t.getLFTerms())
		{
			if (lft instanceof CubismLFTerm)
			{
				String triple = ((CubismLFTerm)lft).generateTriple();

				lfWriter.println(lft.toString());
				lfWriter.println(lft.getOWLString());
				
				
				if (triple != null)
				{
					lfWriter.println(triple);
					tripleWriter.println(triple);
				}
				lfWriter.println();
			}
			axioms.addAll(lfRefConverter.getAxiomsForLastTerm());
			
		}
		lfWriter.println("Axioms:");
		for (OWLAxiom oa : axioms)
			lfWriter.println(oa);
		lfWriter.println();
		
		if (t.getExpression() != null && !t.getExpression().equals(""))
			expWriter.println(t.getExpression());
		expWriter.flush();
		lfWriter.flush();
		tripleWriter.flush();
	}
	
	/**
	 * Perform operations after waiting for remaining parses. No further parses will be added
	 */
	protected void cleanUp()
	{
		super.cleanUp();
		
		if (sendProlog)
		{
			sendPrologToServer();
		}

	}
	
	/**
	 * Load the dialogue and Turns from an XML file
	 * @param reader - the XMLStreamReader containing the XML information
	 * @throws NumberFormatException if the integer for turnNumber cannot be parsed
	 * @throws XMLStreamException if the data cannot be read from the stream
	 */
	public void fromDEFTXML(XMLStreamReader reader) throws NumberFormatException, XMLStreamException
	{
		Turn t = null;
		while (reader.hasNext()) {
			switch (reader.nextTag()) {
				case XMLStreamConstants.START_ELEMENT:
					
					if (reader.getLocalName().equals("turn"))
					{
						//System.out.println(reader.getLocalName());
						String startTime = reader.getAttributeValue(null,"start");
						String speaker = reader.getAttributeValue(null,"speaker");
						int turnNumber = Integer.parseInt(reader.getAttributeValue(null,"turn_no"));
						t = new Turn(this, turnNumber, startTime, speaker);
						t.processText(reader);
						this.addTurn(t);
						//System.out.println(t);
					}
					break;
				case XMLStreamConstants.END_ELEMENT:
					if (reader.getLocalName().equals("dialogue"))
					{
						return;
					}
					break;
			}
		}
	}
	
	public void sendPrologToServer()
	{
		String url = "";
		
		try {
			url = prologServerBaseUrl + getBaseName() + "?auth_token=" + 
						URLEncoder.encode(prologServerAuthToken, "UTF-8");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		String prologData = getExpressionsForTurns();
		Messaging.sendDataToServer(prologData, url);
	}
}
