package utilities;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import messages.NetworkConfiguration;


// Example taken from http://lukealderton.co.uk/blog/posts/2013/december/using-marytts-or-openmary-in-java/

public class TextToSpeech {
	
	public static boolean speechEnabled = false;
    
    public static void say(String input)
    {
    	if (!speechEnabled)
    	{
    		System.out.println(input);
    		return;
    	}
    	
    	Socket speechSocket = null;
		try {
                speechSocket = new Socket(NetworkConfiguration.ttsIp,
			NetworkConfiguration.ttsPort);
                PrintWriter out =
                		new PrintWriter(speechSocket.getOutputStream(), true);
            
                out.println(input);
				
            } catch (UnknownHostException e) {
                System.err.println("Don't know about host");

            } catch (IOException e) {
                System.err.println("Couldn't get I/O for the connection to ");
                
            }
		finally
		{
			try {
				if (speechSocket != null)
					speechSocket.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
    }
}
