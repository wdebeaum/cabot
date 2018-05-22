package environment;

import org.jblas.DoubleMatrix;
import org.json.simple.JSONObject;

public class Space extends Block {

	public Space(JSONObject jsonState) {
		super(jsonState);
		// TODO Auto-generated constructor stub
	}

	public Space(String positionString) {
		super(positionString);
		// TODO Auto-generated constructor stub
	}

	public Space(DoubleMatrix position) {
		super(position);
		// TODO Auto-generated constructor stub
	}

}
