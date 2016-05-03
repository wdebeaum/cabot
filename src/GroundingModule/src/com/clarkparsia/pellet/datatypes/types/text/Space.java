/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.clarkparsia.pellet.datatypes.types.text;

import aterm.ATermAppl;
import com.clarkparsia.pellet.datatypes.AbstractBaseDatatype;
import com.clarkparsia.pellet.datatypes.Datatype;
import com.clarkparsia.pellet.datatypes.RestrictedDatatype;
import com.clarkparsia.pellet.datatypes.exceptions.InvalidLiteralException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import org.mindswap.pellet.utils.ATermUtils;
import org.mindswap.pellet.utils.SetUtils;

/**
 *
 * @author jansen
 */

public class Space extends AbstractBaseDatatype<ATermAppl> {

	protected static final Space			instance;
	protected static final RDFPlainLiteral	RDF_PLAIN_LITERAL;

	static {
		RDF_PLAIN_LITERAL = RDFPlainLiteral.getInstance();

		instance = new Space();
		RestrictedTextDatatype.addPermittedDatatype( instance.getName() );
	}

	public static Space getInstance() {
		return instance;
	}

	protected final RestrictedDatatype<ATermAppl>	dataRange;

	private Space() {
		super( ATermUtils.makeTermAppl( "http://www.cs.rochester.edu/u/jorfan/space.xds#space" ) );
		dataRange = new RestrictedSpace( this, false );
	}

	public RestrictedDatatype<ATermAppl> asDataRange() {
		return dataRange;
	}

	public ATermAppl getCanonicalRepresentation(ATermAppl input) throws InvalidLiteralException {
		return getValue( input );
	}

	public ATermAppl getLiteral(Object value) {
		throw new UnsupportedOperationException();
	}

	public Datatype<?> getPrimitiveDatatype() {
		return RDF_PLAIN_LITERAL;
	}

	public ATermAppl getValue(ATermAppl literal) throws InvalidLiteralException {
		final String lexicalForm = getLexicalForm( literal );
		return RDF_PLAIN_LITERAL.getCanonicalRepresentation(
				ATermUtils.makePlainLiteral( lexicalForm ) );
	}

	public boolean isPrimitive() {
		return false;
	}
   
}
/*
public class Space extends RestrictedTextDatatype {

    public Space(Datatype<ATermAppl> dt, boolean allowLang) {
        super(dt, allowLang);
    }

    public Space(Datatype<ATermAppl> dt, String pattern) {
        super(dt, pattern);
    }

    protected Space(Datatype<ATermAppl> dt, Set<Pattern> patterns, boolean allowLang, Set<Object> excludedValues) {
        super(dt, patterns, allowLang, excludedValues);
    }

    public boolean contains(Object value) {
        if (value instanceof ATermAppl) {
            final ATermAppl a = (ATermAppl) value;

            if (excludedValues.contains(a)) {
                return false;
            }

            if (ATermUtils.isLiteral(a)
                    && permittedDts.contains(a.getArgument(ATermUtils.LIT_URI_INDEX))) {
                if (!allowLang
                        && !ATermUtils.EMPTY.equals(a.getArgument(ATermUtils.LIT_LANG_INDEX))) {
                    return false;
                }

                if (!patterns.isEmpty()) {
                    String litValue = ((ATermAppl) a.getArgument(ATermUtils.LIT_VAL_INDEX)).getName();
                    for (Pattern pattern : patterns) {
                        if (!pattern.matcher(litValue).matches()) {
                            return false;
                        }
                    }
                }

                return true;
            }
        }
        return false;
    }

    public RestrictedDatatype<ATermAppl> exclude(Collection<?> values) {
        Set<Object> newExcludedValues = new HashSet<Object>(values);
        newExcludedValues.addAll(excludedValues);
        return new RestrictedTextDatatype(dt, patterns, allowLang, newExcludedValues);
    }

    public Datatype<? extends ATermAppl> getDatatype() {
        return dt;
    }

    protected <T> List<T> concatLists(List<T> l1, List<T> l2) {
        if (l1.isEmpty()) {
            return l2;
        }
        if (l2.isEmpty()) {
            return l1;
        }

        List<T> newList = new ArrayList<T>(l1.size() + l2.size());
        newList.addAll(l1);
        newList.addAll(l2);

        return newList;
    }

    public RestrictedDatatype<ATermAppl> intersect(RestrictedDatatype<?> other, boolean negated) {
        if (other instanceof RestrictedTextDatatype) {
            RestrictedTextDatatype that = (RestrictedTextDatatype) other;

            return new Space(dt, SetUtils.union(this.patterns, that.patterns), this.allowLang
                    && that.allowLang,
                    SetUtils.union(this.excludedValues, that.excludedValues));
        } else {
            throw new IllegalArgumentException();
        }
    }
}
*/
