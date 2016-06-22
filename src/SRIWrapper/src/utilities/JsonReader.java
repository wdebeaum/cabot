package utilities;

import java.io.BufferedReader;
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
			JSONObject json = (JSONObject)parser.parse(rd);
			return json;
	    } catch (IOException | ParseException e) {
			System.out.println("Could not connect to: " + url);
		} finally {
		    try {
				is.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.out.println("Could not close connection to: " + url);
			}
	    }
		return null;
	}


}