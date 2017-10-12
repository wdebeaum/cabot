package environment;

import org.jblas.DoubleMatrix;

import models.StructureModel;
import features.DirectionFeature;
import features.TemporalSequenceFeature;
import features.UnorderedGroupingFeature;

public class BuildAction extends Action {

	private DirectionFeature direction;
	private TemporalSequenceFeature buildSequence;
	
	public BuildAction() {
		direction = new DirectionFeature("direction");
		direction.setValue(new DoubleMatrix(new double[]{1,0,0}));
		features.put(direction.getName(),direction);
		buildSequence = new TemporalSequenceFeature("buildsequence");
		buildSequence.getDirectionFeature().setValue(direction.getValue());
		features.put(buildSequence.getName(),buildSequence);
	}
	
	public void generateBuildPlan(StructureModel model, String specification)
	{
		
	}
	
	public TemporalSequenceFeature getBuildSequence()
	{
		return buildSequence;
	}
	
	public void projectFrom(UnorderedGroupingFeature objectsToBuild)
	{
		buildSequence = objectsToBuild.projectOnto(buildSequence);
	}

}
