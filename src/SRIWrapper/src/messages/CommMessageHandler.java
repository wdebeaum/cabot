package messages;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import TRIPS.SRIWrapper.SRIWrapper;
import utilities.JsonReader;

	
public class CommMessageHandler implements HttpHandler {

	public SRIWrapper sriWrapper;
	
	public CommMessageHandler(SRIWrapper sriWrapper) {
		this.sriWrapper = sriWrapper;
	}
	
	@Override
	public void handle(HttpExchange exchange) throws IOException {
		System.out.println(exchange.getRequestMethod());
		InputStreamReader isr = new InputStreamReader(exchange.getRequestBody());
		BufferedReader br = new BufferedReader(isr);
		System.out.println("HTTP Handler called");
		int b;
		StringBuilder sb = new StringBuilder();
		while ((b = br.read()) != -1)
			sb.append((char)b);
		
		br.close();
		isr.close();
		System.out.println(sb.toString());
		JSONParser jsonParser = new JSONParser();
		JSONObject json;
		try {
			json = (JSONObject) jsonParser.parse(sb.toString());
			if (json.containsKey("speech_text"))
			{
				JSONObject speechText = (JSONObject) json.get("speech_text");
				sriWrapper.sendText((String)speechText.get("value"));
			}
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
		
		
	}

}
