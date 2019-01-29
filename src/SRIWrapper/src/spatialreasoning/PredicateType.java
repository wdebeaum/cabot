package spatialreasoning;

public enum PredicateType {
	ABOVE("ONT::ABOVE"), 
	BELOW("ONT::BELOW"), 
	NEXTTO("ONT::ADJACENT"),
	TOUCHING("ONT::CONNECTED"),
	SEPARATE("ONT::SEPARATION"),
	TOGETHER("ONT::MODIFIER", "W::TOGETHER"),
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
	RIGHT("ONT::RIGHT"),
	RIGHTLOC("ONT::RIGHT-LOC"),
	HIGHER("ONT::MORE-VAL", "W::HIGH"),
	LOWER("ONT::MORE-VAL", "W::LOW"),
	ANYWHERE("ONT::WH-LOCATION", "W::ANYWHERE"),
	WHEREVER("ONT::PREDICATE", "W::WHEREVER");
	
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
