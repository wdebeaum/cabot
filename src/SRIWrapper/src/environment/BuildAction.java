package environment;

import org.jblas.DoubleMatrix;

import models.StructureModel;
import features.DirectionFeature;
import features.TemporalSequenceFeature;

public class BuildAction extends Action {

	private DirectionFeature direction;
	private TemporalSequenceFeature buildSequence;
	
	public BuildAction() {
		direction = new DirectionFeature("direction");
		direction.setValue(new DoubleMatrix(new double[]{1,0,0}));
		buildSequence = new TemporalSequenceFeature("buildsequence");
	}
	
	public void generateBuildPlan(StructureModel model, String specification)
	{
		
	}

}
