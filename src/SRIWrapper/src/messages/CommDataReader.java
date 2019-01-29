package messages;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import java.io.OutputStream;
import java.net.InetSocketAddress;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import environment.Block;
import environment.Scene;
import goals.GoalStateHandler;
import utilities.JsonReader;
import utilities.TextToSpeech;

public class CommDataReader implements Runnable {

	public volatile boolean stop = false;
	private String apiIp;
	private GoalStateHandler gsh;
	
	public CommDataReader(String apiIp, GoalStateHandler gsh)
	{
		this.apiIp = apiIp;
		this.gsh = gsh;
	}
	
	@Override
	public void run() {
		
		JSONObject streamInfoObject = JsonReader.readURL("http://" + apiIp + ":" + 
				NetworkConfiguration.apiPort + "/human-comm-api/stream-info.json");
		String url = null;
		String hostName = null;
		int portNumber = NetworkConfiguration.subscriptionPort;
		if (streamInfoObject == null)
		{
			System.out.println("Error reading apiInfo");
			hostName = "http://" + apiIp + "/human-comm-api/comm-data.json";
			System.out.println("Hostname set to: " + hostName );
		}
		else
		{
			url = (String)streamInfoObject.get("streaming_url");
			hostName = url.split(":")[0];
			portNumber = Integer.parseInt(url.split(":")[1]);
			System.out.println(streamInfoObject.toJSONString());
		}
		
		try {
			System.out.println("Starting server");
			HttpServer server = HttpServer.create(new InetSocketAddress(NetworkConfiguration.subscriptionPort), 0);
	        server.createContext("/human-comm-api/comm-data.json", new CommMessageHandler(gsh.sriWrapper));
	        server.setExecutor(null); // creates a default executor
	        server.start();
	        System.out.println("Server started");
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		
		System.out.println("Sending subscription");
		NetworkConfiguration.sendDefaultSubscriptionMessage();
	
		try (
				
                //Socket commSocket = new Socket(hostName, portNumber);
				Socket commSocket = new Socket(hostName, 1236);
                BufferedReader in = new BufferedReader(
                    new InputStreamReader(commSocket.getInputStream()));
            ) {

                String fromServer;
                String fromUser;
                JSONParser parser = new JSONParser();
                
                while (!stop) {
                	
                	JSONObject commData = (JSONObject) parser.parse(in);
			System.out.println("Got comm data");
                	JSONObject ASR = (JSONObject)commData.get("speech_text");
                	JSONArray gestures = (JSONArray)commData.get("gestures");
                	Set<Integer> targets = new HashSet<Integer>();
                	for (Object gesture : gestures)
                	{
                		JSONObject gestureJson = (JSONObject)gesture;
                		if (gestureJson.containsKey("deictic_targets"))
                		{
					System.out.println("Found targets");
                			for (Object target : (JSONArray)(gestureJson.get("deictic_targets")))
                				targets.add(Integer.parseInt((String)((JSONObject)target).get("target_id")));
                			
                			Scene.currentScene.pointedTo = Scene.currentScene.integerBlockMapping.get(targets.iterator().next());
                			break;
                		}
                		
                	}
                	System.out.println("Text:" + (String)ASR.get("value"));
                	handleSpeech((String)ASR.get("value"), targets);
                }
            } catch (UnknownHostException e) {
                System.err.println("Don't know about host " + hostName);
            } catch (IOException e) {
                System.err.println("Couldn't get I/O for the connection to " +
                    hostName);
            } catch (ParseException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		
	}
	
	public static void handleSpeech(String text, Collection<Integer> targets)
	{
		String lowercaseText = text.toLowerCase();
		System.out.println("ASR: " + text);
		if (lowercaseText.contains("what is") && targets != null)
		{
			int blockNumber = targets.iterator().next();
			Block b = Scene.currentScene.integerBlockMapping.get(blockNumber);
			TextToSpeech.say("That is the " + b.label + " block.");
		}
		else if (lowercaseText.contains("hello"))
		{
			TextToSpeech.say("Hello");
		}
		else if (lowercaseText.contains("how are you"))
		{
			TextToSpeech.say("I'm fine, thank you.");
		}
		else if (lowercaseText.contains("what are we"))
		{
			TextToSpeech.say("We're going to make a staircase.");
		}
	}

}
