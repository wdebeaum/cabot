package messages;

import java.io.*;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.SRIWrapper.SRIWrapper;
import environment.Scene;
import goals.GoalStateHandler;
import spatialreasoning.Plan;
import utilities.JsonReader;
import utilities.TextToSpeech;

public class BlockMessagePuller implements Runnable {

	public volatile boolean stop = false;
	public volatile boolean pause = false;
	public boolean recordedSession = false;
	private String apiIp;
	public Plan currentPlan;
	private Scene lastScene;
	private int statesRecorded;
	
	private SRIWrapper wrapper;
	private GoalStateHandler goalStateHandler;
	
	public BufferedWriter outputWriter;
	
	public BlockMessagePuller(SRIWrapper wrapper, GoalStateHandler gsh, Plan p, String apiIp)
	{
		currentPlan = p;
		lastScene = null;
		recordedSession = false;
		statesRecorded = 0;
		lastScene = null;
		this.goalStateHandler = gsh;
		this.wrapper = wrapper;
		this.apiIp = apiIp;
		
		try {
			File file = new File("block-state-log.json");
			FileWriter fw = new FileWriter(file.getAbsoluteFile());
			outputWriter = new BufferedWriter(fw);
		} catch (FileNotFoundException | UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public BlockMessagePuller(SRIWrapper wrapper, GoalStateHandler gsh, Plan p, 
			String recordedSessionFileName, String apiIp)
	{
		this(wrapper, gsh, p, apiIp);
		recordedSession = true;
	}
	
	
	@Override
	public void run() {
		
		// Beginning of JSON array
		try {
			outputWriter.write("[");
		} catch (IOException e2) {
			// TODO Auto-generated catch block
			e2.printStackTrace();
		}
		
		
		
		while(stop == false)
		{
			if (!pause)
			{
				//System.out.println("Reading metadata");
				JSONObject blockMetadataInfoObject = JsonReader.readURL("http://" + apiIp + ":" + NetworkConfiguration.apiPort + "/world-api/block-metadata.json");
				if (blockMetadataInfoObject == null)
					System.out.println("Error reading apiInfo");
		
				//System.out.println("Reading block data");
				JSONObject blockStateInfoObject = JsonReader.readURL("http://" + apiIp + ":" + NetworkConfiguration.apiPort + "/world-api/block-state.json");
				if (blockStateInfoObject == null)
					System.out.println("Error reading apiInfo");
//				else
//					System.out.println(blockStateInfoObject.toJSONString());
				
				try {
					if (statesRecorded > 0)
					{
						outputWriter.write(",");
					}
					blockStateInfoObject.writeJSONString(outputWriter);
					outputWriter.flush();
					
					
				} catch (IOException e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
				statesRecorded++;
				
				Scene s = new Scene(blockStateInfoObject, blockMetadataInfoObject);
				
				if (lastScene == null || !lastScene.isSimilarTo(s))
				{
					//System.out.println("Sending KQML Message");
					sendKQMLMessage(s);
					goalStateHandler.checkNewScene(s);
				}
				
				lastScene = s;
				currentPlan.executeNextStep();
				
			}
			
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				closeWriter();
				//Thread.currentThread().interrupt();
				
			}
		}

	}
	
	private void closeWriter()
	{
		try {
			outputWriter.write("]");
			outputWriter.close();
			
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}		
	}
	
	public void sendKQMLMessage(Scene s)
	{
		KQMLPerformative response = new KQMLPerformative("tell");
		KQMLList responseContent = s.getKQMLRepresentation();
		response.setParameter(":CONTENT", responseContent);
		wrapper.sendKQMLPerformative(response);
	}
	
	

}
