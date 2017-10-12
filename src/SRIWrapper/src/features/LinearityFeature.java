package features;

public class LinearityFeature extends DecimalFeature {

	public static String[] indicatorGlosses = {"(:* ONT::GROUPING W::LINE)", "(:* ONT::LINEAR-GROUPING W::LINE)"};
	public static String[] indicatorClasses = {"W::LINE", "ONT::LINEAR-GROUPING"};

	public LinearityFeature(String name) {
		super(name);
	}

}
