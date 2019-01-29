package messages;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

public class NetworkConfiguration {

	public static String apiIp = "10.0.0.3";
	public static final String apiPort = "1236";
	public static final int subscriptionPort = 5000;
	public static final String ttsIp = "localhost";
	public static final int ttsPort = 1528;
	public static boolean simulated = true;
	
	public static void sendSubscriptionMessage(String type, List<String> subscriptions)
	{
		JSONObject subscriptionRequest = new JSONObject();
		
		subscriptionRequest.put("type", type);
		try {
			subscriptionRequest.put("address", InetAddress.getLocalHost().getHostAddress());
		} catch (UnknownHostException e1) {
			subscriptionRequest.put("address", "");
		}
		subscriptionRequest.put("port", NetworkConfiguration.subscriptionPort);
		JSONArray jsonSubscriptions = new JSONArray();
		jsonSubscriptions.addAll(subscriptions);
		subscriptionRequest.put("subscriptions", jsonSubscriptions);
		
		String request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
				"/msg-api/subscribe.json";
		String unescapedRequestData = subscriptionRequest.toString().replace("\\", "");
		try {
			BlockMessageSender.sendPostRequest(unescapedRequestData,request, true);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}
	
	public static void sendDefaultSubscriptionMessage()
	{
		ArrayList<String> subscriptions = new ArrayList<String>();
		subscriptions.add("world-api/block-state.json");
		subscriptions.add("human-comm-api/comm-data.json");
		NetworkConfiguration.sendSubscriptionMessage("BW-MachineComm-Agent", subscriptions);
	}
}
