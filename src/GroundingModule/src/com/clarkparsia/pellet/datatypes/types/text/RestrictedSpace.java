package com.clarkparsia.pellet.datatypes.types.text;

import aterm.ATerm;
import static java.lang.String.format;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import com.clarkparsia.pellet.datatypes.Facet.XSD;
import org.mindswap.pellet.utils.ATermUtils;
import org.mindswap.pellet.utils.SetUtils;
import aterm.ATermAppl;

import com.clarkparsia.pellet.datatypes.Datatype;
import com.clarkparsia.pellet.datatypes.EmptyRestrictedDatatype;
import com.clarkparsia.pellet.datatypes.Facet;
import com.clarkparsia.pellet.datatypes.RestrictedDatatype;
import com.clarkparsia.pellet.datatypes.exceptions.InvalidConstrainingFacetException;
import owlground.GroundedReasoner;
import java.util.Scanner;

/**
 * 
 * Adapted from:  Restricted Text Datatype
 * 
 * @author Jansen
 */
public class RestrictedSpace implements RestrictedDatatype<ATermAppl> {

    private static final String NCNAMESTARTCHAR = "[A-Z]|_|[a-z]|[\u00C0-\u00D6]|[\u00D8-\u00F6]|[\u00F8-\u02FF]|[\u0370-\u037D]|[\u037F-\u1FFF]|[\u200C-\u200D]|[\u2070-\u218F]|[\u2C00-\u2FEF]|[\u3001-\uD7FF]|[\uF900-\uFDCF]|[\uFDF0-\uFFFD]";
    private static final String NCNAMECHAR = NCNAMESTARTCHAR + "|-|\\.|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]";
    protected static final String NCNAME = "(" + NCNAMESTARTCHAR + ")(" + NCNAMECHAR + ")*";
    private static final String NAMESTARTCHAR = ":|" + NCNAMESTARTCHAR;
    private static final String NAMECHAR = NAMESTARTCHAR + "|-|\\.|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]";
    protected static final String NAME = "(" + NAMESTARTCHAR + ")(" + NAMECHAR + ")*";
    protected static final String NMTOKEN = "(" + NAMECHAR + ")+";
    protected static final String TOKEN = "([^\\s])(\\s([^\\s])|([^\\s]))*";
    protected static final String LANGUAGE = "[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*";
    protected static final String NORMALIZED_STRING = "([^\\r\\n\\t])*";
    protected static final Set<ATermAppl> permittedDts;
    protected final Set<Object> excludedValues;
    protected final Set<Pattern> patterns;
    String rep;
    //GroundedReasoner gR;
    static {
        permittedDts = new HashSet<ATermAppl>(Arrays.asList(ATermUtils.EMPTY));
    }

    /*
     * TODO: This is awkward.
     */
    public static boolean addPermittedDatatype(ATermAppl dt) {
        return permittedDts.add(dt);
    }
    protected final boolean allowLang;
    protected final Datatype<ATermAppl> dt;

    public RestrictedSpace(Datatype<ATermAppl> dt, boolean allowLang) {
        this(dt, Collections.<Pattern>emptySet(), allowLang, Collections.emptySet());
    }

    public RestrictedSpace(Datatype<ATermAppl> dt, String pattern) {
        this(dt, Collections.singleton(Pattern.compile(pattern)), false, Collections.emptySet());
    }

    protected RestrictedSpace(Datatype<ATermAppl> dt, Set<Pattern> patterns, boolean allowLang, Set<Object> excludedValues) {
       
        this.dt = dt;
        this.allowLang = allowLang;
        this.excludedValues = excludedValues;
        this.patterns = patterns;
    }

    protected RestrictedSpace(Datatype<ATermAppl> dt, Set<Pattern> patterns, boolean allowLang, Set<Object> excludedValues, String r) {
        this(dt, patterns, allowLang, excludedValues);
        rep = r;
    }

    /**
     *
     * @param facet
     * @param value
     * @return
     * @throws InvalidConstrainingFacetException
     */
    public RestrictedDatatype<ATermAppl> applyConstrainingFacet(ATermAppl facet, Object value)
            throws InvalidConstrainingFacetException {
        /*
         * FIXME throw correct exception type here
         */

        /*
         * Check the facet
         */
        Facet f = Facet.Registry.get(facet);
        {
        }
        if (f == null || !((value instanceof ATermAppl))) {
            final String msg = format(
                    "Attempt to constrain datatype (%s) with unsupported constraining facet ('%s' , '%s')",
                    getDatatype(), facet, value);
            //log.severe( msg );
            throw new IllegalArgumentException(msg);
        }
        String v = ((aterm.pure.ATermApplImpl) value).getArgument(0).toString();

        /*
         * Check the value
         */
        String n;
        if (f.equals(XSD.PATTERN)) {


            return new RestrictedSpace(dt, patterns, allowLang, excludedValues, v);
        } else {
            throw new UnsupportedOperationException();
        }


    }

    /**
     * Is this a valid space
     * @param value
     * @return
     */
    public boolean contains(Object value) {

        if (value instanceof ATermAppl) {
            if (rep == null) {
                return true;
            }

            final ATermAppl a = (ATermAppl) value;
            System.err.println("\t\tContains( " + rep + ", " + a.getArgument(0) + ")");
            return GroundedReasoner.contains(rep, a.getArgument(0).toString());
        }
        return false;
    }

    public boolean containsAtLeast(int n) {
        return true;
    }

    /**
     * Space minus values
     * @param values
     * @return
     */
    public RestrictedDatatype<ATermAppl> exclude(Collection<?> values) {
        System.err.println("\t\texclude: " + values);
        Collection<String> c = new HashSet<String>();
        for (Object o : values) {
            System.err.println(o);
            c.add(o.toString());
        }

        String ret = GroundedReasoner.exclude(rep, c);
        if (ret == null) {
            return new EmptyRestrictedDatatype(this.getDatatype());
        }
        return new RestrictedSpace(dt, patterns, allowLang, excludedValues, ret);
    }

    public Datatype<? extends ATermAppl> getDatatype() {
        return dt;
    }

    public ATermAppl getValue(int i) {
        throw new UnsupportedOperationException();
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


        if (other instanceof RestrictedSpace) {

            RestrictedSpace that = (RestrictedSpace) other;


            if (that.rep == null) {
                return this;
            }
            if (this.rep == null) {
                return that;
            }

            String y = that.rep;
            if (negated) {
                y = "not( " + y + " )"; //change convention as needed

            }
            String ret = GroundedReasoner.intersection(this.rep, y);
            if (ret == null) {
                return new EmptyRestrictedDatatype(this.getDatatype());
            }
            return new RestrictedSpace(dt, patterns, allowLang, excludedValues, ret);


            // return new RestrictedSpace(dt, SetUtils.union(this.patterns, that.patterns), this.allowLang     && that.allowLang, SetUtils.union(this.excludedValues, that.excludedValues));
        } else {
            throw new IllegalArgumentException();
        }
    }

    public boolean isEmpty() {
        return false;


    }

    public boolean isEnumerable() {
        return false;


    }

    public boolean isFinite() {
        return rep != null;


    }

    public int size() {
        throw new IllegalStateException();


    }

    public RestrictedDatatype<ATermAppl> union(RestrictedDatatype<?> other) {
        System.err.println("\t\tunion: " + other);




        if (other instanceof RestrictedSpace) {

            RestrictedSpace that = (RestrictedSpace) other;


            if (that.rep == null) {
                return that;
            }
            if (this.rep == null) {
                return this;
            }



            String ret = GroundedReasoner.union(this.rep, that.rep);
            if (ret == null) {
                return new EmptyRestrictedDatatype(this.getDatatype());
            }
            return new RestrictedSpace(dt, patterns, allowLang, excludedValues, ret);


            // return new RestrictedSpace(dt, SetUtils.union(this.patterns, that.patterns), this.allowLang     && that.allowLang, SetUtils.union(this.excludedValues, that.excludedValues));
        } else {
            throw new IllegalArgumentException();
        }
    }

    public Iterator<ATermAppl> valueIterator() {
        throw new IllegalStateException();


    }

    public String toString() {
        if (rep == null) {
            return "null";


        }
        return rep.toString();

    }
}
