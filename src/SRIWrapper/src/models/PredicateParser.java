package models;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import spatialreasoning.Predicate;
import spatialreasoning.PredicateType;
import utilities.KQMLUtilities;

public class PredicateParser {

	KQMLList eventTerm;
	KQMLList context;
	ReferringExpression headReferringExpression;
	
	public PredicateParser(KQMLList eventTerm, KQMLList context, 
			ReferringExpression headReferringExpression) {
		this.eventTerm = eventTerm;
		this.context = context;
		this.headReferringExpression = headReferringExpression;
	}
	
	/*
	 * Extracts a set of PredicateConstraints from the children of a LF node in the graph 
	 */
	public static Set<PredicateConstraint> extractChildPredicateConstraints(
									ReferringExpression subject,
									String variable, 
									KQMLList context)
	{
		HashSet<PredicateConstraint> predicateConstraints = 
				new HashSet<PredicateConstraint>();
		KQMLList nextTerm = KQMLUtilities.findTermInKQMLList(variable, context);
		PredicateConstraint lastPredicateConstraint = null;
		while (nextTerm != null)
		{
			String instanceType = nextTerm.getKeywordArg(":INSTANCE-OF")
											.stringValue();
			Predicate p = Predicate.predicateFromTerm(nextTerm);
			if (p != null)
			{
				lastPredicateConstraint = new PredicateConstraint(subject,p);
				predicateConstraints.add(lastPredicateConstraint);
			}
			
			if (ReferringExpression.isReferredObject(nextTerm))
			{
				ReferringExpression refExp = 
						new ReferringExpression(nextTerm,context);
				
				if (lastPredicateConstraint != null)
				{
					lastPredicateConstraint.addObject(refExp);
				}
					
			}
			
			if (nextTerm.getKeywordArg(":FORMAL") != null)
			{
				String formalVariable = nextTerm.getKeywordArg(":FORMAL")
											.stringValue();
				nextTerm = KQMLUtilities.findTermInKQMLList(
							formalVariable, context);
			}
			else if (nextTerm.getKeywordArg(":GROUND") != null)
			{
				String formalVariable = nextTerm.getKeywordArg(":GROUND")
						.stringValue();
				nextTerm = KQMLUtilities.findTermInKQMLList(
							formalVariable, context);
			}
			else if (nextTerm.getKeywordArg(":NEUTRAL1") != null)
			{
				String formalVariable = nextTerm.getKeywordArg(":NEUTRAL1")
						.stringValue();
				nextTerm = KQMLUtilities.findTermInKQMLList(
							formalVariable, context);
			}
			// This might be a problem but needed for finding subselected refexps
			else if (nextTerm.getKeywordArg(":FIGURE") != null)
			{
				String formalVariable = nextTerm.getKeywordArg(":FIGURE")
						.stringValue();
				nextTerm = KQMLUtilities.findTermInKQMLList(
							formalVariable, context);
			}
			else
				nextTerm = null;
		}
		
		// Remove "on" for phrases like "on the left"
		if (predicateConstraints.size() > 1)
		{
			Iterator<PredicateConstraint> it = predicateConstraints.iterator();
			while (it.hasNext())
			{
				PredicateConstraint pc = it.next();
				if (pc.getPredicate().getPredicateType().equals(PredicateType.ONTOPOF))
					it.remove();
			}
		}
		
		return predicateConstraints;
	}
	

	public Set<Constraint> extractPredicateConstraints()
	{
		HashSet<Constraint> constraints = new HashSet<Constraint>();
		// Get predicates from formal or neutral1 arguments
		if (eventTerm.getKeywordArg(":FORMAL") != null && headReferringExpression != null)
		{
			
			String formalVariable = eventTerm.getKeywordArg(":FORMAL").stringValue();
			
			Set<PredicateConstraint> predicateConstraints = 
					PredicateParser.extractChildPredicateConstraints(headReferringExpression, 
													formalVariable, context);
			
			constraints.addAll(predicateConstraints);
		}
		else if (eventTerm.getKeywordArg(":NEUTRAL1") != null && headReferringExpression != null)
		{
			String formalVariable = eventTerm.getKeywordArg(":NEUTRAL1").stringValue();
			
			Set<PredicateConstraint> predicateConstraints = 
					PredicateParser.extractChildPredicateConstraints(headReferringExpression, 
													formalVariable, context);
			
			constraints.addAll(predicateConstraints);
		}
		else if (eventTerm.getKeywordArg(":GROUND") != null && headReferringExpression != null)
		{
			String formalVariable = eventTerm.getKeywordArg(":GROUND").stringValue();
			System.out.println("Predicate ground: " + formalVariable);
			Set<PredicateConstraint> predicateConstraints = 
					PredicateParser.extractChildPredicateConstraints(headReferringExpression, 
													formalVariable, context);
			
			constraints.addAll(predicateConstraints);
		}
		else if (eventTerm.getKeywordArg(":AFFECTED") != null && headReferringExpression != null)
		{
			String affectedVariable = eventTerm.getKeywordArg(":AFFECTED").stringValue();
			System.out.println("Predicate affected: " + affectedVariable);
			// These need to send the term itself to find the predicate
			Set<PredicateConstraint> predicateConstraints = 
					PredicateParser.extractChildPredicateConstraints(headReferringExpression, 
													eventTerm.get(KQMLUtilities.VARIABLE).stringValue(), context);
			
			constraints.addAll(predicateConstraints);
		}
		else if (eventTerm.getKeywordArg(":AFFECTED-RESULT") != null && headReferringExpression != null)
		{
			String affectedVariable = eventTerm.getKeywordArg(":AFFECTED-RESULT").stringValue();
			System.out.println("Predicate affected-result: " + affectedVariable);
			// These need to send the term itself to find the predicate
			Set<PredicateConstraint> predicateConstraints = 
					PredicateParser.extractChildPredicateConstraints(headReferringExpression, 
													eventTerm.get(KQMLUtilities.VARIABLE).stringValue(), context);
			
			constraints.addAll(predicateConstraints);
		}
		
		return constraints;
	}

	public static List<Predicate> extractPredicatesFromAnswer(KQMLList context)
	{
		List<Predicate> results = new ArrayList<Predicate>();
		for (KQMLObject term : context)
		{
			KQMLList termList = (KQMLList)term;
			String ontType = "";
			String lex = "*";
			if (termList.getKeywordArg(":INSTANCE-OF") != null)
				ontType = termList.getKeywordArg(":INSTANCE-OF").stringValue();
			if (termList.getKeywordArg(":LEX") != null)
				lex = termList.getKeywordArg(":LEX").stringValue();
			
			PredicateType result = PredicateType.fromString(ontType, lex);
			
			if (result != null)
				results.add(new Predicate(result));
		}
		// Remove "on" for phrases like "on the left"
		if (results.size() > 1)
		{
			Iterator<Predicate> it = results.iterator();
			while (it.hasNext())
			{
				Predicate p = it.next();
				if (p.getPredicateType().equals(PredicateType.ONTOPOF))
					it.remove();
			}
		}
		return results;
	}

}
