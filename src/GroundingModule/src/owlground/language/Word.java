package owlground.language;

import java.util.Collection;
import java.util.HashSet;

import owlground.objects.AbstractDescriptor;

public class Word extends AbstractDescriptor {

	private HashSet<String> posStrings;
	
	public Word(String name) {
		super(name);
		posStrings = new HashSet<String>();
	}
	
	public HashSet<String> getPosStrings()
	{
		return new HashSet<String>(posStrings);
	}
	
	public void addPosStrings(Collection<String> newPosStrings)
	{
		for (String posString : posStrings)
			posString = posString.trim();
		posStrings.addAll(newPosStrings);
	}
	
	public boolean isDescriptiveWord()
	{
		for (String s: posStrings)
		{
			
			if (s.equals("W::N") ||
				s.equals("W::ADJ"))
				return true;
		}
		return false;
	}
	
	public boolean isNounWord()
	{
		for (String s: posStrings)
		{
			if (s.equals("W::N"))
				return true;
		}
		return false;
	}
	
	public String toString()
	{
		String result = super.toString();
		result += "\nPOS tags: ";
		for (String pos: posStrings)
		{
			result += pos + " ";
		}
		
		return result;
	}
	
	public void prunePerceptClusters(int numberOfPerceptClustersToPruneTo)
	{
		
	}

}
