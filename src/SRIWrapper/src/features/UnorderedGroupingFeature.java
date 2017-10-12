package features;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jblas.DoubleMatrix;

public class UnorderedGroupingFeature extends Feature<List> {

	protected List<FeatureGroup> elements;
	protected CountFeature count;
	protected DistanceFeature heightFeature;
	
	public UnorderedGroupingFeature(String name) {
		super(name);
		count = new CountFeature("count");
		heightFeature = new DistanceFeature("ONT::HEIGHT-SCALE");
		elements = new ArrayList<FeatureGroup>();
	}

	@Override
	public List getValue() {
		// TODO Auto-generated method stub
		return elements;
	}

	@Override
	public void setValue(List newValue) {
		elements = newValue;
	}
	
	public CountFeature getCountFeature()
	{
		count.setValue(elements.size());
		return count;
	}
	
	
	private void generateHeightFeature()
	{
		double maxHeight = 0;
		for (FeatureGroup fg : elements)
		{
			double height = 0;
			if (fg instanceof BlockFeatureGroup)
			{
				Map<String, Feature> features = ((BlockFeatureGroup)fg).getFeatures();
				double originY = 0;
				double blockHeight = 0;
				
				if (features.containsKey("ONT::HEIGHT-SCALE"))
					blockHeight = (Double)features.get("ONT::HEIGHT-SCALE").getValue();
				
				if (features.containsKey("ONT::LOCATION"))
					originY = ((DoubleMatrix)features.get("ONT::LOCATION").getValue()).get(2);
				
				height = originY + height / 2;

			}
			else if (fg instanceof UnorderedGroupingFeature)
			{
				height = getHeightFeature().getValue();
			}

			if (height > maxHeight)
				maxHeight = height;
		}

		heightFeature.setValue(maxHeight);
		
	}
	
	public DistanceFeature getHeightFeature()
	{
		generateHeightFeature();
		return heightFeature;
	}
	
	public TemporalSequenceFeature projectOnto(TemporalSequenceFeature onto)
	{
		for (FeatureGroup element : elements)
			onto.addToSequence(element);
		
		return onto;
	}
	
	private void generateFeatures()
	{
		generateHeightFeature();
	}
	
	public Map<String,Feature> getFeatures()
	{
		HashMap<String,Feature> result = new HashMap<String,Feature>();
		result.putAll(super.getFeatures());
		result.put(count.name, count);
		result.put(heightFeature.name, heightFeature);
		return result;
	}

}
