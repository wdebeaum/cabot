package spatialreasoning;

public enum PredicateType {
	ABOVE("ONT::ABOVE"), 
	BELOW("ONT::BELOW"), 
	NEXTTO("ONT::ADJACENT"),
	TOUCHING("ONT::CONNECTED"),
	ONGROUND("GROUND"),
	ONTOPOF("ONT::ON"),
	ATSAMEHEIGHT("ONT::LEVEL"),
	ATSAMEY("SAMEY"),
	BOTTOM("ONT::BOTTOM-LOCATION-VAL"),
	TOP("ONT::TOP-LOCATION-VAL"),
	LEFT("ONT::LEFT"),
	LEFTLOC("ONT::LEFT-LOC"),
	RIGHT("ONT::RIGHT"),
	RIGHTLOC("ONT::RIGHT-LOC"),
	HIGHER("ONT::MORE-VAL", "W::HIGH"),
	LOWER("ONT::MORE-VAL", "W::LOW");
	
	private String ontType;
	private String lex;
	
	PredicateType(String ontType)
	{
		this.ontType = ontType;
		this.lex = "*";
	}
	
	PredicateType(String ontType, String lex)
	{
		this.ontType = ontType;
		this.lex = lex;
	}
	
	public static PredicateType fromString(String ontType, String lex)
	{
		for (PredicateType p : PredicateType.values())
		{
			if (ontType.equals(p.ontType) && p.lex.equals("*"))
				return p;
			if (ontType.equals(p.ontType) && p.lex.equals(lex))
				return p;
		}
		
		return null;
	}
}
