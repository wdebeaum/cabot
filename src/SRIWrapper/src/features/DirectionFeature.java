package features;

import org.jblas.DoubleMatrix;

public class DirectionFeature extends Feature<DoubleMatrix> {

	private DoubleMatrix value;
	
	public DirectionFeature(String name) {
		super(name);
		value = new DoubleMatrix();
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public DoubleMatrix getValue() {
		// TODO Auto-generated method stub
		return value;
	}

	@Override
	public void setValue(DoubleMatrix newValue) {
		// TODO Auto-generated method stub
		value = newValue;
	}
	
	public int getMaxAxis()
	{
		double maxAxisValue = 0;
		int maxAxis = 0;
		for (int i = 0; i < value.length; i++)
		{
			if (Math.abs(value.data[i]) > maxAxisValue)
			{
				maxAxis = i;
				maxAxisValue = Math.abs(value.data[i]);
			}
				
		}
		
		return maxAxis;
	}

}
