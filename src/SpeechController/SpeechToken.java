/*
 * File: SpeechToken.java
 * Creator: George Ferguson
 * Created: Tue May 13 13:33:54 2008
 * Time-stamp: <Tue May 13 13:34:08 EDT 2008 ferguson>
 */

package TRIPS.SpeechController;

public class SpeechToken {
    public String word;
    public int start;
    public int end;
    public SpeechToken(String w, int s, int e) {
	word = w;
	start = s;
	end = e;
    }
}
