package features;

public class CountFeature extends Feature<Integer> {

	public static String[] indicatorGlosses = {"NUMBER"};
	public static String[] indicatorClasses = {};
	
	private int value;

	
	
	public CountFeature(String name) {
		super(name);
		value = 0;
		// TODO Auto-generated constructor stub
	}

	@Override
	public Integer getValue() {
		// TODO Auto-generated method stub
		return value;
	}

	@Override
	public void setValue(Integer newValue) {
		// TODO Auto-generated method stub
		value = newValue;
	}
	
	public String toString()
	{
		return "Count: " + value;
	}

}
