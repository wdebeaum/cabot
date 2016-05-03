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
}
