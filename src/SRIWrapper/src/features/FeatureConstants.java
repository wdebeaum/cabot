package features;

public class FeatureConstants {

	public static final String HEIGHT = "ONT::HEIGHT-SCALE";
	public static final String WIDTH = "ONT::WIDTH-SCALE";
	public static final String SIZE = "ONT::SIZE-SCALE";
	public static final String CENTER = "ONT::CENTER";
	public static final String CENTEROFMASS = "ONT::LOC-AS-POINT";
	public static final String LOCATION = "ONT::LOCATION";
	public static final String ORIGIN = "ONT::STARTPOINT";
	public static final String END = "ONT::ENDPOINT";
	public static final String TOP = "ONT::TOP-LOCATION";
	public static final String BOTTOM = "ONT::BOTTOM-LOCATION";
	public static final String LEFT = "ONT::LEFT";
	public static final String LEFTLOC = "ONT::LEFT-LOC";
	public static final String RIGHT = "ONT::RIGHT";
	public static final String CORRECT = "ONT::CORRECT"; // Hack
	public static final String RIGHTLOC = "ONT::RIGHT-LOC";
	
	public static final String ROW = "ONT::ROW-FORMATION";
	public static final String COLUMN = "ONT::COLUMN-FORMATION";
	
	public static final String DIRECTION = "ONT::DIRECTION";
	
	public static final String HORIZONTAL = "ONT::HORIZONTAL";
	public static final String VERTICAL = "ONT::VERTICAL";
	public static final String LINE = "ONT::LINEAR-GROUPING";
	
	public static final String NUMBER = "ONT::NUMBER";
	public static final String QUANTITY = "ONT::QUANTITY-ABSTR";
	
	public static final String GROUPING = "ONT::GROUP-OBJECT";
	public static final String SEQUENCE = "ONT::SEQUENCE";
	public static final String BLOCK = "ONT::BLOCK";
	public static final String SPACE = "ONT::LOC-AS-AREA";
	public static final String SET = "ONT::SET";
	public static final String GAP = "ONT::PHYS-SHAPE";
	public static final String OTHER = "ONT::OTHER";
	public static final String REF_SEM = "ONT::REFERENTIAL-SEM";
	
	public static final String MIN = "W::MIN";
	public static final String MAX = "W::MAX";
	public static final String MORETHAN = "W::MORE-THAN";
	public static final String LESSTHAN = "W::LESS-THAN";
	
	public static final String UNIVERSAL = "ONT::UNIVERSAL";
	
	public static final String LAST = "ONT::LAST-VAL";
	public FeatureConstants() {
		// TODO Auto-generated constructor stub
	}
	
	public static boolean isNumericAttributeFeature(String feature)
	{
		switch (feature)
		{
		case HEIGHT:
		case WIDTH:
		case NUMBER:
			return true;
			
		default:
			return false;
		}
	}

}
