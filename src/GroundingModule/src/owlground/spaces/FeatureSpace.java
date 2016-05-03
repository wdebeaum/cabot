package owlground.spaces;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import javax.xml.stream.*;

import org.jblas.DoubleMatrix;
import org.jblas.Solve;
import org.apache.commons.math3.geometry.*;
import org.apache.commons.math3.geometry.euclidean.oned.Euclidean1D;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;

import owlground.objects.AbstractDescriptor;
import owlground.perception.Percept;
import owlground.regions.DataRegion;
import owlground.regions.KNearestRegion;

/*
 * Used to represent a feature space. Ties in with the ontology, but that
 * part isn't important at the moment.
 */

public abstract class FeatureSpace implements Comparable<FeatureSpace>, Space{
	protected String name;
	protected String conceptId;
	protected String featureTag;
	protected int dimension;
	protected HashMap<AbstractDescriptor,DataRegion> regionMap;
	protected Set<DoubleMatrix> unlabeledData;
	protected SpaceManager spaceManager;
	protected boolean dirty;
	protected DoubleMatrix mean;
	protected DoubleMatrix covariance;
	protected DoubleMatrix precision;
	
	public FeatureSpace(String name, int dim)
	{
		this.name = name;
		this.dimension = dim;
		this.regionMap = new HashMap<AbstractDescriptor, DataRegion>();
		this.dirty = true;
		this.unlabeledData = new HashSet<DoubleMatrix>();
	}

	/**
	 * Read an XML element as a space definition, and return a new Space object.
	 */
	public static FeatureSpace fromXML(XMLStreamReader r) throws XMLStreamException
	{
		String name = r.getLocalName().toLowerCase();
		int dim = 0;
		String type = null;
		while (r.hasNext()) {
			switch (r.next()) {
				case XMLStreamConstants.START_ELEMENT:
					if (r.getLocalName().equals("dim"))
						dim = Integer.parseInt(r.getElementText());
					else if (r.getLocalName().equals("type"))
						type = r.getElementText().toLowerCase();
					else
						throw new RuntimeException("expected <dim> or <type> in space, but got <" + r.getLocalName() + ">");
					break;
				case XMLStreamConstants.END_ELEMENT:
					if (r.getLocalName().equalsIgnoreCase(name))
					{
						if (type == null)
							throw new RuntimeException("space <" + name + "> missing <type>");
						else if (type.equals("euclidean"))
							return new EuclideanSpace(name, dim);
						else
							throw new RuntimeException("unknown space type " + type + " for space <" + name + ">");
					}
					/* ignore other end tags */
				/* ignore other events */
			}
		}
		throw new RuntimeException("unterminated space element <" + name + ">");
	}

	public String getFeatureTag() {
		return featureTag;
	}
	
	public String getName() {
		return name;
	}
	public String getConceptId() {
		return conceptId;
	}
	
	public DataRegion getRegion(AbstractDescriptor descriptor)
	{
		return regionMap.get(descriptor);
	}
	
	public void addRegion(AbstractDescriptor descriptor, DataRegion region)
	{
		//if (getRegion(descriptor) != null)
		//	System.out.println("Warning: Overwriting region for " + descriptor.getName());
		regionMap.put(descriptor, region);
		dirty = true;
	}
	
	public DoubleMatrix getMean()
	{
		if (dirty)
			recalculate();
		
		return mean;
	}
	
	public DoubleMatrix getPrecision()
	{
		if (dirty)
			recalculate();
		
		return precision;
	}
	
	private void calculatePrecision()
	{
		double ridgeEpsilon = .0000001;

		//Ridging for singular matrices
		DoubleMatrix ridgedCovariance = covariance.add(DoubleMatrix.eye(covariance.columns).mul(ridgeEpsilon));

		precision = Solve.solveSymmetric(ridgedCovariance, DoubleMatrix.eye(ridgedCovariance.rows));
	}
	
	private void calculateMean()
	{
		DoubleMatrix result = null;
		Collection<DoubleMatrix> values = getPerceptValues();
		values.addAll(unlabeledData);
		
		for (DoubleMatrix value : values)
		{
			if (result == null)
			{
				result = DoubleMatrix.zeros(dimension);
			}
			
			result.addi(value);
		}
		result.divi(values.size());
		
		mean = result;
	}
	
	public DoubleMatrix getCovariance()
	{
		if (dirty)
			recalculate();
		
		return covariance;
	}
	
	private void calculateCovariance()
	{
		DoubleMatrix covarianceMatrix = DoubleMatrix.zeros(mean.length, mean.length);
		Collection<DoubleMatrix> values = getPerceptValues();
		values.addAll(unlabeledData);
		
		for (int i = 0; i < mean.length; i++)
		{
			for(int j = 0; j < mean.length; j++)
			{
				double result = 0.0d;
				for (DoubleMatrix value : values)
				{
					result += (value.get(i) - mean.get(i)) *
							  (value.get(j) - mean.get(j));
				}
				
				covarianceMatrix.put(j + i * mean.length, result / values.size());
				
			}
		}

		covariance = covarianceMatrix;
	}
	
	/**
	 * Add feature data from a scene that does not yet have labels. Used to update the
	 * feature space statistics before labels are given.
	 * @param points A Collection of DoubleMatrices belonging to this feature space
	 */
	public void addUnlabeledData(Collection<DoubleMatrix> points)
	{
		unlabeledData.addAll(points);
	}
	
	/**
	 * Add feature data from a scene that does not yet have labels. Used to update the
	 * feature space statistics before labels are given.
	 * @param points A Collection of DoubleMatrices belonging to this feature space
	 */
	public void addUnlabeledData(DoubleMatrix point)
	{
		unlabeledData.add(point);
	}
	
	public double getMahalanobisDistance(DoubleMatrix point1, DoubleMatrix point2)
	{
		return Math.sqrt(getSquaredMahalanobisDistance(point1, point2));
	}
	
	public double getSquaredMahalanobisDistance(DoubleMatrix point1, DoubleMatrix point2)
	{
		DoubleMatrix difference = point1.sub(point2);

		DoubleMatrix left = difference.transpose().mmul(getPrecision());

		return left.mmul(difference).get(0);
	}

	
	public void updateRegions()
	{
		for (DataRegion dr : regionMap.values())
		{
			dr.update();
		}
		dirty = true;
	}
	
	public abstract double distance(DoubleMatrix a, DoubleMatrix b);
	
	public String toString()
	{
		return getName();
	}
	
	public Collection<Percept> getPercepts()
	{
		HashSet<Percept> percepts = new HashSet<Percept>();
		for (DataRegion dr : regionMap.values())
		{
			percepts.addAll(dr.getPercepts());
		}
		
		return percepts;
	}
	
	/**
	 * Get a collection of the percept values for this feature space.
	 * @return a collection of the percept matrices
	 */
	public Collection<DoubleMatrix> getPerceptValues()
	{
		HashSet<DoubleMatrix> perceptValues = new HashSet<DoubleMatrix>();
		for (DataRegion dr : regionMap.values())
		{
			perceptValues.addAll(dr.getPerceptValues());
		}
		
		return perceptValues;
	}
	
	/**
	 * Add a default region (K-Nearest Neighbor) corresponding to the given descriptor.
	 * @param descriptor The descriptor label for the region
	 */
	public void addDefaultRegion(AbstractDescriptor descriptor)
	{
/*		if (getDimension() == 1)
			addRegion(descriptor, new PolytopePerceptualDataRegion<Euclidean1D>(this, descriptor));
		if (getDimension() == 2)
			addRegion(descriptor, new PolytopePerceptualDataRegion<Euclidean2D>(this, descriptor));
		if (getDimension() == 3)
			addRegion(descriptor, new PolytopePerceptualDataRegion<Euclidean3D>(this, descriptor));
		else*/
			addRegion(descriptor, new KNearestRegion(this,descriptor));
	}
	
	
	@Override
	public int compareTo(FeatureSpace s)
	{
		return getName().compareTo(s.getName());
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (!(o instanceof FeatureSpace))
			return false;
		
		return getName().equals(((FeatureSpace)o).getName());
	}

	public SpaceManager getSpaceManager() {
		return spaceManager;
	}

	public void setSpaceManager(SpaceManager spaceManager) {
		this.spaceManager = spaceManager;
	}
	
	public void recalculate()
	{
		calculateMean();
		calculateCovariance();
		calculatePrecision();
		dirty = false;
	}
	
	
	
}
