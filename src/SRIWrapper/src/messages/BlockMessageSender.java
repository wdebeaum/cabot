package messages;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.jblas.DoubleMatrix;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

import environment.Block;

public class BlockMessageSender {

	public static boolean ENABLED = true;
	// From http://stackoverflow.com/a/4206094
	public static void sendPostRequest(Block b) throws IOException
	{
		if (ENABLED == false)
			return;
		
		String request;
		if (NetworkConfiguration.simulated)
			request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
			"/plan/block-state.json";
		else
			request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
			"/machine-manip/action.json";
		
		sendPostRequest(b.getJSONRepresentation().toString(),request);
	}
	
	public static void sendPostRequest(DoubleMatrix position) throws IOException
	{
		Block b = new Block(position);
		sendPostRequest(b);
	}
	
	public static void sendPostRequest(Collection<Block> blocks) throws IOException
	{
		if (ENABLED == false)
			return;
		
		String request;
		if (NetworkConfiguration.simulated)
		{
			request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
			"/plan/block-state.json";
			JSONObject blockStateObject = new JSONObject();
			String timeStamp = new SimpleDateFormat("yyyy-MM-dd'T'HH.mm.ss.SSS").format(new Date());
			blockStateObject.put("Timestamp", timeStamp);
			blockStateObject.put("IsStable", "true");
			
			JSONArray array = new JSONArray();
			int i = 1;
			for (Block b : blocks)
			{
				array.add(b.getJSONRepresentation(i));
				i++;
			}
			blockStateObject.put("BlockStates", array);
			
			sendPostRequest(blockStateObject.toString(),request);
		}
		else
		{
			request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
			"/machine-manip/action.json";
			for (Block b : blocks)
				sendPostRequest(b.getJSONRepresentation().toString(),request);
		}
		
	}
	
	public static void clearBlocks() throws IOException
	{
		String request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
				"/plan/block-state.json";
		System.out.println("Sending clear block request");
		JSONObject blockStateObject = new JSONObject();
		JSONArray array = new JSONArray();
		blockStateObject.put("BlockStates", array);
		
		sendPostRequest(blockStateObject.toString(),request);
	}
	
	public static void sendPostRequest(String content, String request) throws IOException
	{
		sendPostRequest(content,request,false);
	}
	
	public static void sendPostRequest(String content, String request, boolean json) throws IOException
	{

		System.out.println("Sending:");
		System.out.println(content);
		System.out.println("to:");
		System.out.println(request);
		byte[] postData       = content.getBytes( StandardCharsets.UTF_8 );
		int    postDataLength = postData.length;
		URL    url            = new URL( request );
		HttpURLConnection conn= (HttpURLConnection) url.openConnection();   
		System.out.println("Connection opened");
		conn.setDoOutput( true );
		//conn.setInstanceFollowRedirects( false );
		conn.setRequestMethod( "POST" );
		if (json)
			conn.setRequestProperty( "Content-Type", "application/json"); 
		else
			conn.setRequestProperty( "Content-Type", "application/x-www-form-urlencoded"); 
		//conn.setRequestProperty("Accept","*/*");
		//conn.setRequestProperty( "charset", "utf-8");
		//conn.setRequestProperty( "Content-Length", Integer.toString( postDataLength ));
		//conn.setUseCaches( false );
		conn.connect();
		

		OutputStream os = conn.getOutputStream();
		DataOutputStream dos = new DataOutputStream(os);
		//os.write(postData);
		dos.writeBytes(content);
		dos.flush();
		dos.close();
		
		//os.flush();
		//os.close();
        
		System.out.println("Response: " + conn.getResponseCode());
		
		conn.disconnect();
		//os.close();
//		//try( DataOutputStream wr = new DataOutputStream( conn.getOutputStream())) {
//		   wr.write( postData );
//		   wr.close();
//		}
//		catch (IOException e)
//		{
//			e.printStackTrace();
//		}
	}
	
	public void sendCurlPostRequest(Block b)
	{
		String urlParameters  = b.getJSONRepresentation().toString();
		System.out.println("Sending:");
		System.out.println(urlParameters);
		byte[] postData       = urlParameters.getBytes( StandardCharsets.UTF_8 );
		int    postDataLength = postData.length;
		String request        = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
										"/machine-manip/action.json";
		System.out.println("To:" + request);
		ProcessBuilder pb = new ProcessBuilder(
	            "curl",
	            "--request",
	            "POST",
	            request,
	            "--data",
	            "\"" + urlParameters + "\"");
		
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
