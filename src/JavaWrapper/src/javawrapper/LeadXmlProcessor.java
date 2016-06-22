package javawrapper;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

public class LeadXmlProcessor {

	private String serialDataDirectory;
	private String baseDataDirectory;
	private DialogueIndexer dialogueIndexer;
	
	public LeadXmlProcessor(DialogueIndexer dialogueIndexer, String serialDataDirectory, 
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
    public void readFile(File file) throws FileNotFoundException
    {
    	String filename = file.getName();
    	filename = filename.substring(0,filename.length() - 3) + "ser";
    	File dialogueFile = new File(serialDataDirectory + filename);
    	System.out.println("Checking: " + dialogueFile.getAbsolutePath());
    	if (dialogueFile.exists())
    	{
    		//if (!writeTriples)
    		//	return;
    		
    		Dialogue d = Dialogue.fromFile(dialogueFile);
    		
    		if (d != null)
    		{
    			d.setParsed(true);
	    		//dialogues.add(d);
    			
	    		d.initTripleFile();
	    		d.initExpFile();
	    		d.initDebugFile();
	    		d.printTurnsToFile();
	    		d.sendPrologToServer();
	    		d.sendOntologyToServer();
	    	    return;
    		}
    	}
		
		System.out.println(file.getAbsolutePath());
		
		XMLInputFactory factory = XMLInputFactory.newInstance();
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

		// Begin parsing XML
		try {
			int dialogueNumber = 0;
			while (reader.hasNext()) {
				switch (reader.next()) {
					case XMLStreamConstants.START_ELEMENT:
						if (reader.getLocalName().equals("dialogue"))
						{
							Dialogue d = new Dialogue(file.getName(),dialogueNumber);
							d.fromXML(reader);
							dialogueIndexer.addDialogue(d);
						}
						break;
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
    }

	public void readFilesInDirectory(String directoryString)
	{
		if (directoryString == null)
			return;
		
		File directory = new File(directoryString);
		
		if (!directory.exists())
			return;
		
		for (File file : directory.listFiles())
		{
			try{
				readFile(file);
			}
			catch (FileNotFoundException e)
			{
				e.printStackTrace();
			}
		}
		System.out.println("XML files loaded");

		LFTerm.writeEnums(baseDataDirectory + "enums");
		dialogueIndexer.startParsingTurns();
	}
    
}
