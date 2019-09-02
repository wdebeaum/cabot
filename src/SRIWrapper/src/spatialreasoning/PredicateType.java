package spatialreasoning;

public enum PredicateType {
	ABOVE("ONT::ABOVE"), 
	BELOW("ONT::BELOW"), 
	NEXTTO("ONT::ADJACENT"),
	TOUCHING("ONT::MODIFIER", "W::TOUCHING"),
	ATTACHED("ONT::ATTACH"),
	CONNECTED("ONT::CONNECTED"),
	FILLED("ONT::FILL-CONTAINER"),
	SEPARATE("ONT::SEPARATION"),
	TOGETHER("ONT::ADJACENT"),
	ONGROUND("GROUND"),
	ONTOPOF("ONT::ON"),
	START("ONT::STARTPOINT"),
	END("ONT::ENDPOINT"),
	ATSAMEHEIGHT("ONT::LEVEL"),
	ATSAMEY("SAMEY"),
	BOTTOM("ONT::BOTTOM-LOCATION-VAL"),
	TOP("ONT::TOP-LOCATION-VAL"),
	MIDDLE("ONT::MIDDLE-LOCATION-VAL"),
	SIDE("ONT::SIDE-LOCATION-VAL"),
	SIDELOC("ONT::SIDE-LOCATION"),
	SIDEPRED("ONT::PREDICATE", "W::ON-THE-SIDE"),
	BETWEEN("ONT::BETWEEN"),
	CENTER("ONT::CENTER"),
	LEFT("ONT::LEFT"),
	LEFTLOC("ONT::LEFT-LOC"),
	LEFTMOST("ONT::MAX-VAL", "W::LEFT"),
	RIGHT("ONT::RIGHT"),
	RIGHTMOST("ONT::MAX-VAL", "W::RIGHT"),
	CORRECT("ONT::CORRECT"),
	RIGHTLOC("ONT::RIGHT-LOC"),
	HIGHER("ONT::MORE-VAL", "W::HIGH"),
	LOWER("ONT::MORE-VAL", "W::LOW"),
	ANYWHERE("ONT::LOCATION", "W::ANYWHERE"),
	WHEREVER("ONT::PREDICATE", "W::WHEREVER"),
	INNER("ONT::INTERNAL"),
	OUTER("ONT::EXTERNAL");
	
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
	
	public static boolean isBinary(PredicateType pt)
	{
		switch (pt)
		{
		case ONTOPOF:
		case HIGHER:
		case LOWER:
		case BETWEEN:
		case BELOW:
			return true;
		default:
			return false;	
		}
	}
}
