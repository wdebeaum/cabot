package owlground;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.jblas.DoubleMatrix;

import owlground.language.Word;
import owlground.objects.ObjectClass;
import owlground.objects.Property;
import owlground.spaces.FeatureSpace;

public class RepresentativeFeatureSelection {

	
	/*
	 * Choose representative features for class names according to variance
	 * @param minUtterances The minimum number of utterances required to consider a property
	 */	
	public static void chooseRepresentativeClassNameFeatures(Map<FeatureSpace, Double> averageScaledSumOfSquares, Collection<ObjectClass> objectClasses,
													double featureThreshold, int minUtterances)
	{
		
		// Choose features for property that are less than and farthest from mean
		for (ObjectClass oc : objectClasses)
		{
			if (oc.getPerceptClusters().size() == 0 || oc.getUtterances().size() < minUtterances)
				continue;
			
			Set<FeatureSpace> representativeFeatures = new HashSet<FeatureSpace>();
			Map<FeatureSpace, Double> scaledSumOfSquares = oc.getScaledSumOfSquaresForAllFeatures();
			
			for (FeatureSpace featureSpace : scaledSumOfSquares.keySet())
			{
				double featureQuality = scaledSumOfSquares.get(featureSpace) / 
						averageScaledSumOfSquares.get(featureSpace);
				oc.getFeatureRatios().put(featureSpace, featureQuality);
				if (featureQuality < featureThreshold)
				{
					representativeFeatures.add(featureSpace);
				}
			}
			
			oc.getRepresentativeFeatures().clear();
			oc.getRepresentativeFeatures().addAll(representativeFeatures);
		}		
	}
	
	
	/*
	 * Choose representative features for properties according to variance
	 * @param minUtterances The minimum number of utterances required to consider a property
	 */
	public static void chooseRepresentativeFeatures(Map<FeatureSpace,Double> averageScaledSumOfSquares, Collection<Property> properties,
												double featureThreshold, int minUtterances)
	{

		for (Property p : properties)
		{
			p.updatePerceptClusters();
		}
		
		// Choose features for property that are less than and farthest from mean
		for (Property p : properties)
		{
			if (p.getPerceptClusters().size() == 0)
				continue;
			
			if (p.getUtterances().size() < minUtterances)
				continue;
			
			System.out.println("Property: " + p.getName());
			
			double minFeatureQuality = Double.MAX_VALUE;
			FeatureSpace bestFeature = null;
			Map<FeatureSpace, Double> scaledSumOfSquares = p.getScaledSumOfSquaresForAllFeatures();
			
			for (FeatureSpace featureSpace : scaledSumOfSquares.keySet())
			{
				System.out.println("Variance for feature " + featureSpace + ": " + scaledSumOfSquares.get(featureSpace));
				System.out.println("Average variance: " + averageScaledSumOfSquares.get(featureSpace));
				double featureQuality = scaledSumOfSquares.get(featureSpace) / 
						averageScaledSumOfSquares.get(featureSpace);
				p.getFeatureRatios().put(featureSpace, featureQuality);
				if (featureQuality < minFeatureQuality && featureQuality < featureThreshold)
				{
					minFeatureQuality = featureQuality;
					bestFeature = featureSpace;
				}
			}
			//System.out.println("Best feature: " + bestFeature);
			p.setRepresentativeFeature(bestFeature);
		}
	}
	
	/*
	 * Choose representative features for properties according to variance
	 * @param minUtterances The minimum number of utterances required to consider a property
	 */
	public static void chooseRepresentativeWordFeatures(Map<FeatureSpace,Double> averageScaledSumOfSquares, Collection<Word> words,
			double featureThreshold, int minUtterances)
	{

		for (Word w : words)
		{
			w.updatePerceptClusters();
		}

		
		
		// Choose features for property that are less than and farthest from mean
		for (Word w : words)
		{
			if (w.getPerceptClusters().size() == 0)
				continue;
			
			if (w.getUtterances().size() < minUtterances)
				continue;
			
			//System.out.println("Word: " + w.getName());
			
			double minFeatureQuality = Double.MAX_VALUE;
			FeatureSpace bestFeature = null;
			Map<FeatureSpace, Double> scaledSumOfSquares = w.getScaledSumOfSquaresForAllFeatures();
			
			for (FeatureSpace featureSpace : scaledSumOfSquares.keySet())
			{
				//System.out.println("Variance for feature " + featureSpace + ": " + scaledSumOfSquares.get(featureSpace));
				//System.out.println("Average variance: " + averageScaledSumOfSquares.get(featureSpace));
				double featureQuality = scaledSumOfSquares.get(featureSpace) / 
						averageScaledSumOfSquares.get(featureSpace);
				w.getFeatureRatios().put(featureSpace, featureQuality);
				if (featureQuality < minFeatureQuality && featureQuality < featureThreshold)
				{
					minFeatureQuality = featureQuality;
					bestFeature = featureSpace;
				}
			}
			//System.out.println("Best feature: " + bestFeature);
			//w.setRepresentativeFeature(bestFeature);
		}
	}
	
	/*
	 * Choose representative features according to minimum variance within feature vectors.
	 * @param minUtterances The minimum number of utterances required to consider a property
	 */
	public static void chooseRepresentativeFeaturesWithVectorVariance(Map<FeatureSpace,Double> averageVectorVariance, Collection<Property> properties,
			double featureThreshold, int minUtterances)
	{

		for (Property p : properties)
		{
			p.updatePerceptClusters();
		}
		
		// Choose features for property that are less than and farthest from mean
		for (Property p : properties)
		{
			if (p.getPerceptClusters().size() == 0)
				continue;
			
			if (p.getUtterances().size() < minUtterances)
				continue;
			
			//System.out.println("Property: " + p.getName());
			
			double minFeatureQuality = Double.MAX_VALUE;
			FeatureSpace bestFeature = null;
			Map<FeatureSpace, DoubleMatrix> vectorVariance = p.getVectorVarianceForAllFeatures();
			
			for (FeatureSpace featureSpace : vectorVariance.keySet())
			{
				//System.out.println("Variance for feature " + featureSpace + ": " + vectorVariance.get(featureSpace));
				//System.out.println("Average variance: " + averageVectorVariance.get(featureSpace));
				DoubleMatrix featureQualityVector = vectorVariance.get(featureSpace).div(averageVectorVariance.get(featureSpace));
				double featureQuality = featureQualityVector.min();
				p.getFeatureRatios().put(featureSpace, featureQuality);
				if (featureQuality < minFeatureQuality && featureQuality < featureThreshold)
				{
					minFeatureQuality = featureQuality;
					bestFeature = featureSpace;
				}
			}
			//System.out.println("Best feature: " + bestFeature);
			p.setRepresentativeFeature(bestFeature);
		}
	}
	
	private void resolveRepresentativeFeatureConflicts()
	{
		
	}
}
