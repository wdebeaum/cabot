package messages;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;

import org.jblas.DoubleMatrix;

import environment.Block;

public class BlockMessageSender {

	
	// From http://stackoverflow.com/a/4206094
	public static void sendPostRequest(Block b) throws IOException
	{
		String request = "http://" + NetworkConfiguration.apiIp + ":" + NetworkConfiguration.apiPort +
				"/machine-manip/action.json";
		
		sendPostRequest(b,request);
	}
	
	public static void sendPostRequest(DoubleMatrix position) throws IOException
	{
		Block b = new Block(position);
		sendPostRequest(b);
	}
	
	public static void sendPostRequest(Block b, String request) throws IOException
	{
		String urlParameters  = b.getJSONRepresentation();
		System.out.println("Sending:");
		System.out.println(urlParameters);
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
		//conn.setRequestProperty( "Content-Length", Integer.toString( postDataLength ));
		//conn.setUseCaches( false );
		conn.connect();
		

		OutputStream os = conn.getOutputStream();
		os.write(postData);
		os.flush();
		os.close();
        
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
		String urlParameters  = b.getJSONRepresentation();
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
