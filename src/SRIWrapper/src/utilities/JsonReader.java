package utilities;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.Charset;

import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class JsonReader {
	
	public static JSONObject readURL(String url)
	{
		JSONParser parser = new JSONParser();
		InputStream is = null;
		try {
			is = new URL(url).openStream();
	    
			BufferedReader rd = new BufferedReader(new InputStreamReader(is, 
	    		  									Charset.forName("UTF-8")));
			String escaped = getEscapedJSONString(rd);
			JSONObject json = (JSONObject)parser.parse(escaped);
			if (is != null)
				is.close();
			return json;
	    } catch (IOException | ParseException e) {
			System.out.println("Could not connect to: " + url);
		} finally {
		    try {
		    		if (is != null)
		    			is.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.out.println("Could not close connection to: " + url);
			}
	    }
		return null;
	}
	
	private static String getEscapedJSONString(BufferedReader br) throws IOException
	{
		String line;
		StringBuilder sb = new StringBuilder();
		while ((line = br.readLine()) != null)
		{
			sb.append(line.replace("\\", "" ).replace("\"{", "{").replace("}\"", "}"));
		}
		
		return sb.toString();
	}


}