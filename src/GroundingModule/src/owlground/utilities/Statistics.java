package owlground.utilities;

import org.apache.commons.math3.distribution.ChiSquaredDistribution;

public class Statistics {

	public static double mahalanobisToProbability(double squaredMahalanobisDistance, int dimensions)
	{
		ChiSquaredDistribution chi = new ChiSquaredDistribution(dimensions);
		return 1 - chi.cumulativeProbability(squaredMahalanobisDistance);
	}
}
