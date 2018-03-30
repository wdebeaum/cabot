package utilities;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLString;
import TRIPS.SRIWrapper.SRIWrapper;
import messages.BlockMessageSender;
import messages.CommDataSender;
import messages.NetworkConfiguration;


// Example taken from http://lukealderton.co.uk/blog/posts/2013/december/using-marytts-or-openmary-in-java/

public class TextToSpeech {
	
	public static boolean SPEECH_ENABLED = true;
	public static boolean APPARATUS_ENABLED = true;
	public static boolean MARYTTS_ENABLED = false;
	public static String lastSpoken = "";
	public static SRIWrapper SRIWRAPPER = null;
	private static int nextUttnum = 1;
	
    
    public static void say(String input)
    {
	    	sendUtterance(input);
	    	System.out.println("Trying to say: " + input);
	    	if (!SPEECH_ENABLED)
	    	{
	    		return;
	    	}
	    	
	    	if (APPARATUS_ENABLED)
	    	{
		    	try {
					CommDataSender.sendSpeechPostRequest(input);
					System.out.println(input);
					return;
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
	    	}
	    	
	    	else if (MARYTTS_ENABLED)
	    	{
	    	
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
    
    private static void sendUtterance(String utterance)
    {
	    	KQMLPerformative performative = new KQMLPerformative("TELL");
	    	KQMLList content = new KQMLList();
	    	content.add("spoken");
	    	content.add(":what");
	    	content.add(new KQMLString(utterance));
	    	
	    	performative.setParameter(":CONTENT", content);
	    	
	    	SRIWRAPPER.sendKQMLPerformative(performative);
    }
    
    public static void sayWithoutRepeating(String input)
    {
	    	if (input.equals(lastSpoken) || input.equals("Sorry, an error occurred in generation."))
	    		return;
	    	
	    	lastSpoken = input;
	    	say(input);
    }
}
