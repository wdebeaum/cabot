package environment;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.jblas.DoubleMatrix;

public class SceneLoader {

	String conceptName;
	boolean readingPositive = true;
	final String BLOCK_CHAR = "x";
	final String SPACE_CHAR = "o";
	List<Scene> positiveExamples;
	List<Scene> negativeExamples;
	
	public SceneLoader() {
		positiveExamples = new ArrayList<Scene>();
		negativeExamples = new ArrayList<Scene>();
	}

	public void readExamplesFromFile(String filename)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		boolean[][] currentBlocks = new boolean[5][5];
		int numberOfBlocksRead = 0;
		int currentRow = 0;
		System.out.println("Reading example...");
		int lineNumber = 0;
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;

				
			    while ((line = reader.readLine()) != null) {
			    	
				    	if (lineNumber == 0)
				    		conceptName = line.trim();
				    	else if (line.trim().toLowerCase().equals("positive"))
				    		readingPositive = true;
				    	else if (line.trim().toLowerCase().equals("negative"))
				    		readingPositive = false;
				    	else if (line.trim().length() == 0)
				    	{
				    		// Processes an example when a blank line is encountered
				    		if (numberOfBlocksRead > 0)
				    			processExample(readingPositive, currentBlocks);
				    		continue;
				    	}
				    	else if (line.contains("x") && line.length() < 6)
				    	{
				    		for (int currentColumn = 0; currentColumn < line.length(); currentColumn++)
				    		{
				    			if (line.charAt(currentColumn) == 'x')
				    				currentBlocks[currentRow][currentColumn] = true;
				    		}
				    	}
				    
				    	lineNumber++;
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open events file " + filename.toString() + " for reading.");
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}		
	}
	
	private void processExample(boolean positive, boolean[][] currentBlocks)
	{
		Scene s = new Scene();
		
		for (int i = currentBlocks.length - 1; i >= 0; i--)
		{
			for (int j = 0; j < currentBlocks[i].length; j++)
			{
				if (currentBlocks[i][j])
				{
					double x = i * Block.BLOCK_WIDTH;
					double y = j * Block.BLOCK_WIDTH;
					Block b = new Block(new DoubleMatrix(new double[]{x,y,0}));
					s.addBlock(b);
				}
			}
		}
		
		if (positive)
			positiveExamples.add(s);
		else
			negativeExamples.add(s);
	}
}
