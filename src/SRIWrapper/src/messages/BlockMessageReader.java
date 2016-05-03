package messages;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.time.*;
import java.time.format.*;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import TRIPS.SRIWrapper.SRIWrapper;
import environment.Scene;
import spatialreasoning.Plan;
import utilities.JsonReader;
import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;



public class BlockMessageReader implements Runnable {

	public volatile boolean stop = false;
	public Plan currentPlan;
	private Scene lastScene;
	private JSONArray currentLoadedBlockData;
	private JSONObject currentLoadedMetaData;
	private ZonedDateTime lastTime;
	private long millisecondsToWait;
	private double speedMultiplier;
	private int currentStep = 0;
	private SRIWrapper wrapper;
	
	public BlockMessageReader(SRIWrapper wrapper, Plan p, String metaDataFile, String blockDataFile)
	{
		currentPlan = p;
		lastScene = null;
		lastTime = null;
		millisecondsToWait = 0;
		speedMultiplier = 1;
		this.wrapper = wrapper;

		JSONParser parser = new JSONParser();
		try {
			currentLoadedBlockData = (JSONArray)parser.parse(new FileReader(blockDataFile));
		} catch (FileNotFoundException e) {
			System.err.println("No such file found.");
			e.printStackTrace();
		} catch (IOException e) {
			System.err.println("IO error on block data file.");
			e.printStackTrace();
		} catch (ParseException e) {
			System.err.println("Block data file is not a valid JSON array.");
			e.printStackTrace();
		}
		
		try {
			currentLoadedMetaData = (JSONObject)parser.parse(new FileReader(metaDataFile));
		} catch (FileNotFoundException e) {
			System.err.println("No such file found.");
			e.printStackTrace();
		} catch (IOException e) {
			System.err.println("IO error on meta data file.");
			e.printStackTrace();
		} catch (ParseException e) {
			System.err.println("Meta data file is not a valid JSON array.");
			e.printStackTrace();
		}
		
	}

	
	
	@Override
	public void run() {
		
		
		while(stop == false)
		{
			JSONObject currentBlockState = (JSONObject)currentLoadedBlockData.get(currentStep);
			String timestamp = (String)currentBlockState.get("Timestamp");
			
			
			ZonedDateTime currentTime = ZonedDateTime.parse(timestamp, DateTimeFormatter.ISO_OFFSET_DATE_TIME);
			
			currentStep++;
			Scene s = new Scene(currentBlockState, currentLoadedMetaData);
			
			if (lastScene == null || lastScene.isSimilarTo(s))
				sendKQMLMessage(s);
			
			lastScene = s;
			currentPlan.executeNextStep();
			
			if (currentStep < currentLoadedBlockData.size())
			{
				JSONObject nextBlockState = (JSONObject)currentLoadedBlockData.get(currentStep);
				String nextTimeStamp = (String)nextBlockState.get("Timestamp");
				ZonedDateTime nextTime = ZonedDateTime.parse(nextTimeStamp, DateTimeFormatter.ISO_OFFSET_DATE_TIME);
				
				long currentMilliseconds = currentTime.toInstant().toEpochMilli();
				long nextMilliseconds = nextTime.toInstant().toEpochMilli();
				millisecondsToWait = (long)((nextMilliseconds - currentMilliseconds) / speedMultiplier);
				System.out.println("Waiting " + millisecondsToWait + " milliseconds.");
			}
			else
			{
				return;
			}
			
			try {
				Thread.sleep(millisecondsToWait);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				
				Thread.currentThread().interrupt();
				
			}
		}
	}
	
	public void setSpeedMultiplier(double multiplier)
	{
		speedMultiplier = multiplier;
	}
	
	public void sendKQMLMessage(Scene s)
	{
		KQMLPerformative response = new KQMLPerformative("tell");
		KQMLList responseContent = s.getKQMLRepresentation();
		response.setParameter(":CONTENT", responseContent);
		wrapper.sendKQMLPerformative(response);
	}
	
}
