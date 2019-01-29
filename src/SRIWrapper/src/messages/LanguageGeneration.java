package messages;

public class LanguageGeneration {

	public LanguageGeneration() {
		// TODO Auto-generated constructor stub
	}
	
	public static String ordinalString(int ordinal)
	{
		if (ordinal < 1)
			return "";
		switch (ordinal)
		{
		case 0:
			return "zeroth";
		case 1:
			return "first";
		case 2:
			return "second";
		case 3:
			return "third";
		default:
			return ordinal + "th";
		}
	}

}
