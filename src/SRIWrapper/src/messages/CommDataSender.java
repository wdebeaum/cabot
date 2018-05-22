package messages;

import java.io.IOException;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

import org.jblas.DoubleMatrix;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import utilities.FormatConversion;
import environment.Block;

public class CommDataSender {

	public static void sendSpeechPostRequest(String text) throws IOException
	{
		String request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
				"/machine-comm-api/comm-data.json";
		
		String urlParameters  = getSpeechJSON(text);
		System.out.println("Sending:");
		System.out.println(urlParameters);
		System.out.println(" to ");
		System.out.println(request);
		byte[] postData       = urlParameters.getBytes( StandardCharsets.UTF_8 );
		int    postDataLength = postData.length;
		URL    url            = new URL( request );
		HttpURLConnection conn= (HttpURLConnection) url.openConnection();   
		System.out.println("Connection opened");
		conn.setDoOutput( true );
		//conn.setInstanceFollowRedirects( false );
		conn.setRequestMethod( "POST" );
		//conn.setRequestProperty( "Content-Type", "application/json"); 
		conn.setRequestProperty( "Content-Type", "application/x-www-form-urlencoded"); 
		conn.setRequestProperty("Accept","*/*");
		
		//conn.setRequestProperty( "charset", "utf-8");
		conn.setRequestProperty( "Content-Length", Integer.toString( postDataLength ));
		//conn.setUseCaches( false );
		conn.connect();
		

		OutputStream os = conn.getOutputStream();
		os.write(postData);
		os.flush();
		os.close();
        
		System.out.println("Response: " + conn.getResponseCode());
		
		conn.disconnect();

	}
	
	public static String getSpeechJSON(String text)
	{
		JSONObject jsonBlock = new JSONObject();
		//jsonBlock.put("timestamp", "2015-08-17T01:45:33.134001");
		
		JSONObject speechTextBlock = new JSONObject();
		
		speechTextBlock.put("source", "ASR");
		speechTextBlock.put("value", text);
		//speechTextBlock.put("start_timestamp", "2015-08-17T01:43:30.0");
		//speechTextBlock.put("duration", "0");
		//speechTextBlock.put("conf", "1");

		jsonBlock.put("speech_text", speechTextBlock);
		System.out.println(jsonBlock.toString());
		
		return jsonBlock.toString();
	}
	
	public static void sendCurlPostSpeechRequest(String text)
	{
		String urlParameters  = getSpeechJSON(text);
		System.out.println("Sending:");
		System.out.println(urlParameters);
		byte[] postData       = urlParameters.getBytes( StandardCharsets.UTF_8 );
		int    postDataLength = postData.length;
		String request        = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
										"/machine-comm-api/comm-data.json";
		System.out.println("To:" + request);
		ProcessBuilder pb = new ProcessBuilder(
	            "curl",
	            "-X",
	            "POST",
	            "-d",
	            "'" + urlParameters + "'",
	            request);
		
		
		//System.out.println();
		Process p = null;
		try {
			p = pb.start();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		if (p != null)
			try {
				System.out.println("Error code: " + p.waitFor());
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}


	}
	
}
