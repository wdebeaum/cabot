package utilities;

import org.jblas.DoubleMatrix;

import TRIPS.KQML.KQMLList;

public class FormatConversion {

	
	public static KQMLList doubleMatrixToKQMLList(DoubleMatrix input)
	{
		KQMLList result = new KQMLList();
		for (double value : input.data)
		{
			result.add("" + value);
		}
		
		return result;
	}
	
	public static String doubleMatrixToJSONString(DoubleMatrix input)
	{
		if (input == null)
			return null;
		
		StringBuilder sb = new StringBuilder();

		for (int i = 0; i < input.length; i++)
		{
			sb.append("" + input.get(i));
			if (i < input.length - 1)
				sb.append(",");
		}

		return sb.toString();
	}
	
	public static DoubleMatrix stringToDoubleMatrix(String input)
	{
		String[] inputElements = input.split(",");
		double[] inputElementDoubles = new double[inputElements.length];
		for(int i = 0; i < inputElements.length; i++) 
			inputElementDoubles[i] = Double.parseDouble(inputElements[i]); 
		return new DoubleMatrix(inputElementDoubles);
	}
}
