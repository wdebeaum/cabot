
package backEnd;

import java.util.*;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.*;
import org.semanticweb.owlapi.vocab.OWLFacet;
import org.semanticweb.owlapi.reasoner.*;
import org.semanticweb.owlapi.expression.*;
import org.coode.owlapi.manchesterowlsyntax.*;
import uk.ac.manchester.cs.owlapi.dlsyntax.DLSyntaxObjectRenderer;
import org.semanticweb.owlapi.vocab.OWLFacet;
/**
 *
 * @author jansen
 */
public class OWLBackend {

// <editor-fold defaultstate="collapsed" desc="Members">
    /**
     * holds a representation of the classTree for the GUI
     */
    /**
     * the OWLAPI OWLOntologyManager for this backend
     */
    public OWLOntologyManager manager;
    /**
     * The primary ontology
     */
    OWLOntology mainOnt;
    /* TODO: Handle >1 ontology file */
    /**
     * the OWLAPI OWLDataFactory for this backend
     */
    OWLDataFactory factory;
    /**
     * the OWLAPI OWLReasoner for this backend
     */
    OWLReasoner reasoner;
    /**
     * the OWLAPI parser for this backend, uses Manchester OWL Syntax
     */
    ManchesterOWLSyntaxClassExpressionParser parser;
    /**
     * Renders classes in description logic
     */
    DLSyntaxObjectRenderer renderer;
    /**
     * Keeps track of IRI prefixes so we just use simple names to look up classes
     */
    Map<String, String> simpleNameMap;
    /**
     * default name space
     */
    String dPrefix;
    /**
     * Holds changes to to avoid commit overhead
     */
    List<OWLOntologyChange> pendingChanges = new ArrayList<OWLOntologyChange>();
    OWLClass thing;
    OWLClass nothing;
    // </editor-fold>
// <editor-fold defaultstate="collapsed" desc="Setup/misc">

    /**
     * No full support for >1 ontology
     * @return A List of the IRI's loaded by this backend
     */
    /*
    public List<IRI> getLoadedIRIs() {
    LinkedList<IRI> l = new LinkedList<IRI>();
    for (OWLOntology o : manager.getOntologies()) {
    l.add(o.getOntologyID().getOntologyIRI());
    }
    return l;
    }
     *
     */
    /**
     * Loads a single Ontology using it's IRI (in practice it is typically just
     * a file name and not the true IRI) and sets it as the main ontology
     * @param onts the physical IRIs of the ontologies to be loaded
     * @return The IRIs that could not be loaded
     */
    public OWLOntology loadOntology(IRI onts) {
        System.out.println(onts);
        try {
            mainOnt = manager.loadOntologyFromOntologyDocument(onts);
        } catch (OWLOntologyCreationException e) {
            System.err.println(e.getMessage());
        } catch (IllegalArgumentException e) {
            System.err.println(e.getMessage());
        }
        //MapIRI???
        System.out.println("Setting ");
        System.out.println("Loading Complete");
        initiateReasoner(mainOnt);
        generateSimpleNameMap(mainOnt);
        dPrefix = mainOnt.getOntologyID().getOntologyIRI().getStart();
        return mainOnt;
    }

    public OWLBackend() {
        initiate();
    }

    /**
     * convenience constructor same as calling OWLBackend() and then loadOntologies(onts)
     * @param onts
     */
    public OWLBackend(IRI ont) {
        initiate();
        loadOntology(ont);

    }

    public OWLBackend(String ont) {
        this(IRI.create(ont));
    }

    /**
     * instantiates  the manager data factory and reasoner provided by OWLAPI and Pellet
     */
    private void initiate() {
        manager = OWLManager.createOWLOntologyManager();
        factory = manager.getOWLDataFactory();
        thing = factory.getOWLThing();
        nothing = factory.getOWLNothing();
    }

    /**
     * instantiates the parser then loads the necessary ontologies into the reasoners
     */
    void initiateReasoner(OWLOntology o) {
        ConsoleProgressMonitor progressMonitor = new ConsoleProgressMonitor();
        OWLReasonerConfiguration config = new SimpleConfiguration(progressMonitor);
        renderer = new DLSyntaxObjectRenderer();
        parser = new ManchesterOWLSyntaxClassExpressionParser(factory, new ShortFormEntityChecker(new BidirectionalShortFormProviderAdapter(manager.getOntologies(), new SimpleShortFormProvider())));
        try {
            System.out.println("Loading " + o.getOntologyID().getOntologyIRI().toString() + " into the Pellet Reasoner...");
            reasoner = new com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory().createReasoner(o, config);

        } catch (UnsupportedOperationException e) {
            System.err.println("Failed to load onotologies into reasoner.\n" + e.getMessage());
            System.exit(1);
        }
    }

    private void generateSimpleNameMap(OWLOntology mainOnt) {
        if (simpleNameMap == null) {
            simpleNameMap = new HashMap<String, String>();
        }


        for (OWLOntology ont : manager.getOntologies()) {

            for (OWLEntity e : ont.getSignature()) {

                String simple = e.getIRI().getFragment();
                if (simpleNameMap.containsKey(simple) && !e.getIRI().toString().equals(simpleNameMap.get(simple))) {
                    System.out.println("Simple Name Map: Name conflict between" + simpleNameMap.get(simple) + " and " + e.getIRI() + ". Using " + simpleNameMap.get(simple));
                } else {
                    simpleNameMap.put(simple, e.getIRI().toString());
                }

            }
        }
    }

    /**
     * Generates an OWLEntity based on a string in Manchester OWL Syntax
     * @param s the query
     * @return An OWLEntity generated from that query
     * @throws ParserException Improper formatting
     */
    OWLClassExpression parse(String s) throws ParserException {
        return parser.parse(s);
    }

    public void saveOntology(String f) {
        try {
            manager.saveOntology(mainOnt, IRI.create(f));
        } catch (OWLOntologyStorageException e) {
            System.err.println(e);
        }
    }
    // </editor-fold>
// <editor-fold defaultstate="collapsed" desc="Class Reasoning">

    Set<OWLClass> getNamedEquivalentClasses(OWLClassExpression d) {
        try {
            return reasoner.getEquivalentClasses(d).getEntities();
        } catch (UnsupportedOperationException e) {
            System.err.println("Reasoner failed:\n" + e.getMessage());
            e.printStackTrace();
            System.exit(1);

        }
        return null;
    }

    Set<OWLClass> getSubClasses(OWLClassExpression d) {
        return getSuperClasses(d, true);
    }

    Set<OWLClass> getSubClasses(OWLClassExpression d,
            boolean direct) {
        try {
            return reasoner.getSubClasses(d, direct).getFlattened();
        } catch (UnsupportedOperationException e) {
            System.err.println("Reasoner failed:\n" + e.getMessage());
            e.printStackTrace();
            System.exit(1);

        }
        return null;
    }

    Set<OWLClass> getSuperClasses(OWLClassExpression d) {
        return getSuperClasses(d, true);
    }

    Set<OWLClass> getSuperClasses(OWLClassExpression d,
            boolean direct) {
        try {
            return reasoner.getSuperClasses(d, direct).getFlattened();
        } catch (UnsupportedOperationException e) {
            System.err.println("Reasoner failed:\n" + e.getMessage());
            e.printStackTrace();
            System.exit(1);

        }
        return null;
    }

    Set<OWLNamedIndividual> getIndividuals(OWLClassExpression d) {
        try {
            return reasoner.getInstances(d, false).getFlattened();
        } catch (UnsupportedOperationException e) {
            System.err.println("Reasoner failed:\n" + e.getMessage());
            e.printStackTrace();
            System.exit(1);

        }
        return null;
    }

    Set<OWLClassExpression> getAnonymousDirectEquivalentClasses(OWLClassExpression e) {
//get features
        Set<OWLClassExpression> f = new HashSet<OWLClassExpression>();
        Set<OWLClass> c = reasoner.getEquivalentClasses(e).getEntities();
        if (e.getClassExpressionType().equals(ClassExpressionType.OWL_CLASS)) {
            c.add(e.asOWLClass());
            c.addAll(reasoner.getSuperClasses(e, true).getFlattened());
        } else {
            f.addAll(e.asConjunctSet());
        }

        f.addAll(reasoner.getSuperClasses(e, true).getFlattened());



        for (OWLClass i : c) {
            for (OWLEquivalentClassesAxiom a : mainOnt.getEquivalentClassesAxioms(i)) {

                for (OWLClassExpression j : a.getClassExpressions()) {
                    f.addAll(j.asConjunctSet());
                }
            }

        }

        //   Set <OWLClassExpression> complete = new HashSet<OWLClassExpression>();

        f.remove(e);
        f.removeAll(reasoner.getEquivalentClasses(e).getEntities());
        //   f = flattenExpression(f);
        if (f.isEmpty() || f.contains(factory.getOWLThing())) {
            f.clear();
            f.add(e);
        }
        return f;
    }

    Set<OWLClassExpression> getAnonymousDirectSuperClasses(OWLClassExpression e) {
//get features
        Set<OWLClassExpression> f = new HashSet<OWLClassExpression>();
        Set<OWLClass> c = reasoner.getEquivalentClasses(e).getEntities();
        if (e.getClassExpressionType().equals(ClassExpressionType.OWL_CLASS)) {
            c.add(e.asOWLClass());
            c.addAll(reasoner.getSuperClasses(e, true).getFlattened());
        } else {
            f.addAll(e.asConjunctSet());
        }

        f.addAll(reasoner.getSuperClasses(e, true).getFlattened());



        for (OWLClass i : c) {
            for (OWLSubClassOfAxiom a : mainOnt.getSubClassAxiomsForSubClass(i)) {


                f.addAll(a.getSuperClass().asConjunctSet());

            }

        }

        //   Set <OWLClassExpression> complete = new HashSet<OWLClassExpression>();

        f.remove(e);
        f.removeAll(reasoner.getEquivalentClasses(e).getEntities());
        //   f = flattenExpression(f);
        if (f.isEmpty() || f.contains(factory.getOWLThing())) {
            f.clear();
            f.add(e);
        }
        return f;
    }
     // </editor-fold>
            // <editor-fold defaultstate="collapsed" desc="data access">
            /**
             * Checks if the specfied class is in the ontology
             * @param s
             * @param p
             * @return true iff the class with name s and prefix p exists
             */


    public boolean classInOntology(String s, String p) {
        return mainOnt.containsClassInSignature(IRI.create(p + "#" + s));
    }

    /**
     * Checks if a class with the simple name s is in the ontology
     * @param s
     * @return true iff a class with simple name s exists
     */
    public boolean classInOntology(String s) {
        return simpleNameMap.containsKey(s);
    }

    //gets the named class if it exists creates one otherwise,
    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s simple name of the class
     * @return a known class with simple name s if it exists or a new class with the default prefix
     */
    public OWLClass getClass(String s) {
        OWLClass ret = createClass(s);
        if (ret == null) {
            return getNamedClass(s);
        }
        return ret;
    }

    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s
     * @param p
     * @return a known class with if it exists or a new class
     */
    public OWLClass getClass(String s, String p) {
        OWLClass ret = createClass(s, p);
        if (ret == null) {
            return getNamedClass(s, p);
        }
        return ret;
    }
//use deafult name space if none is specifed

    /**
     * Gets a class if it does not exist in the ont
     * Does not added the class to the ontology
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it does not exist, null otherwise
     */
    public OWLClass createClass(String s, String p) {
        IRI i = IRI.create(p + "#" + s);
        if (!classInOntology(s, p)) {
            return factory.getOWLClass(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Creates a class if the simple name does not exist in the ont. The prefix used is the same as the main ontology's IRI's prefix
     * Does not added the class to the ontology
     * @param s simple name
     * @return an OWLClass with the simple name s if it does not exists, null otherwise
     */
    public OWLClass createClass(String s) {
        if (!classInOntology(s)) {

            return factory.getOWLClass(IRI.create(dPrefix + "#" + s));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }

    /**
     * Gets a class if it exists in the ont
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it exists, null otherwise
     */
    public OWLClass getNamedClass(String s, String p) {
        IRI i = IRI.create(p + "#" + s);
        if (classInOntology(s, p)) {
            return factory.getOWLClass(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Gets a class if it exists in the ont
     * @param s simple name
     * @return an OWLClass with the simple name s if it exists, null otherwise
     */
    public OWLClass getNamedClass(String s) {
        if (simpleNameMap.containsKey(s)) {

            return factory.getOWLClass(IRI.create(simpleNameMap.get(s)));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }
//literals and ranges

    //Properties
    //data
    /**
     * Checks if the specfied class is in the ontology
     * @param s
     * @param p
     * @return true iff the class with name s and prefix p exists
     */
    public boolean dataPropertyInOntology(String s, String p) {
        return mainOnt.containsDataPropertyInSignature(IRI.create(p + "#" + s));
    }

    /**
     * Checks if a class with the simple name s is in the ontology
     * @param s
     * @return true iff a class with simple name s exists
     */
    public boolean dataPropertyInOntology(String s) {
        return simpleNameMap.containsKey(s);
    }

    //gets the named class if it exists creates one otherwise,
    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s simple name of the class
     * @return a known class with simple name s if it exists or a new class with the default prefix
     */
    public OWLDataProperty getDataProperty(String s) {
        OWLDataProperty ret = createDataProperty(s);
        if (ret == null) {
            return getNamedDataProperty(s);
        }
        return ret;
    }

    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s
     * @param p
     * @return a known class with if it exists or a new class
     */
    public OWLDataProperty getDataProperty(String s, String p) {
        OWLDataProperty ret = createDataProperty(s, p);
        if (ret == null) {
            return getNamedDataProperty(s, p);
        }
        return ret;
    }
//use deafult name space if none is specifed

    /**
     * Gets a class if it does not exist in the ont
     * Does not added the class to the ontology
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it does not exist, null otherwise
     */
    public OWLDataProperty createDataProperty(String s, String p) {
        IRI i = IRI.create(p + "#" + s);
        if (!objPropertyInOntology(s, p)) {
            return factory.getOWLDataProperty(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Creates a class if the simple name does not exist in the ont. The prefix used is the same as the main ontology's IRI's prefix
     * Does not added the class to the ontology
     * @param s simple name
     * @return an OWLClass with the simple name s if it does not exists, null otherwise
     */
    public OWLDataProperty createDataProperty(String s) {
        if (!objPropertyInOntology(s)) {

            return factory.getOWLDataProperty(IRI.create(dPrefix + "#" + s));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }

    /**
     * Gets a class if it exists in the ont
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it exists, null otherwise
     */
    public OWLDataProperty getNamedDataProperty(String s, String p) {

        if (objPropertyInOntology(p, s)) {
            return factory.getOWLDataProperty(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Gets a class if it exists in the ont
     * @param s simple name
     * @return an OWLClass with the simple name s if it exists, null otherwise
     */
    public OWLDataProperty getNamedDataProperty(String s) {
        if (simpleNameMap.containsKey(s)) {

            return factory.getOWLDataProperty(IRI.create(simpleNameMap.get(s)));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }

    //obj
    /**
     * Checks if the specfied class is in the ontology
     * @param s
     * @param p
     * @return true iff the class with name s and prefix p exists
     */
    public boolean objPropertyInOntology(String s, String p) {
        return mainOnt.containsObjectPropertyInSignature(IRI.create(p + "#" + s));
    }

    /**
     * Checks if a class with the simple name s is in the ontology
     * @param s
     * @return true iff a class with simple name s exists
     */
    public boolean objPropertyInOntology(String s) {
        return simpleNameMap.containsKey(s);
    }

    //gets the named class if it exists creates one otherwise,
    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s simple name of the class
     * @return a known class with simple name s if it exists or a new class with the default prefix
     */
    public OWLObjectProperty getObjProperty(String s) {
        OWLObjectProperty ret = createObjProperty(s);
        if (ret == null) {
            return getNamedObjProperty(s);
        }
        return ret;
    }

    /**
     *
     * @see createClass
     * @see getNamedClass
     * @param s
     * @param p
     * @return a known class with if it exists or a new class
     */
    public OWLObjectProperty getObjProperty(String s, String p) {
        OWLObjectProperty ret = createObjProperty(s, p);
        if (ret == null) {
            return getNamedObjProperty(s, p);
        }
        return ret;
    }
//use deafult name space if none is specifed

    /**
     * Gets a class if it does not exist in the ont
     * Does not added the class to the ontology
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it does not exist, null otherwise
     */
    public OWLObjectProperty createObjProperty(String s, String p) {
        IRI i = IRI.create(p + "#" + s);
        if (!objPropertyInOntology(s, p)) {
            return factory.getOWLObjectProperty(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Creates a class if the simple name does not exist in the ont. The prefix used is the same as the main ontology's IRI's prefix
     * Does not added the class to the ontology
     * @param s simple name
     * @return an OWLClass with the simple name s if it does not exists, null otherwise
     */
    public OWLObjectProperty createObjProperty(String s) {
        if (!objPropertyInOntology(s)) {

            return factory.getOWLObjectProperty(IRI.create(dPrefix + "#" + s));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }

    /**
     * Gets a class if it exists in the ont
     * @param s name
     * @param p prefix
     * @return an OWLClass with the iri p+"#"+s if it exists, null otherwise
     */
    public OWLObjectProperty getNamedObjProperty(String s, String p) {

        if (objPropertyInOntology(p, s)) {
            return factory.getOWLObjectProperty(IRI.create(p + "#" + s));
        }
        return null;
    }

    /**
     * Gets a class if it exists in the ont
     * @param s simple name
     * @return an OWLClass with the simple name s if it exists, null otherwise
     */
    public OWLObjectProperty getNamedObjProperty(String s) {
        if (simpleNameMap.containsKey(s)) {

            return factory.getOWLObjectProperty(IRI.create(simpleNameMap.get(s)));
        } else {
//            return getNamedClass(s, dPrefix);
            return null;
        }

    }

    /**
     * Adds a super class axiom to the pending changes with an option to commit them
     * @param c the class
     * @param superClass the super class
     * @param commit if true then the changes will be committed and the reasoner flushed
     */
    public void addSuperClass(OWLClassExpression c, OWLClassExpression superClass, boolean commit) {
        OWLAxiom ax = factory.getOWLSubClassOfAxiom(c, superClass);
        pendingChanges.addAll(manager.addAxiom(mainOnt, ax));
        if (commit) {
            applyPendingChanges();
        }
    }

    public void addSubClass(OWLClassExpression c, OWLClassExpression subClass, boolean commit) {
        addSuperClass(subClass, c, commit);
    }

    /**
     * Adds an axiom that asserts the two expressions are equivalent to the pending changes with an option to commit them
     * @param c1
     * @param c2
     * @param commit if true then the changes will be committed and the reasoner flushed
     */
    public void addEquivalentClass(OWLClassExpression c1, OWLClassExpression c2, boolean commit) {
        OWLAxiom ax = factory.getOWLEquivalentClassesAxiom(c1, c2);
        pendingChanges.addAll(manager.addAxiom(mainOnt, ax));
        if (commit) {
            applyPendingChanges();
        }
    }

    public void addDisjointClass(OWLClassExpression c1, OWLClassExpression c2, boolean commit) {

        OWLAxiom ax = factory.getOWLEquivalentClassesAxiom(c1, c2);
        pendingChanges.addAll(manager.addAxiom(mainOnt, ax));
        if (commit) {
            applyPendingChanges();
        }
    }

    /**
     * applies pending changes and updates the reasoner
     */
    public void applyPendingChanges() {
        manager.applyChanges(pendingChanges);
        reasoner.flush();
        pendingChanges.clear();
    }

    public OWLClassExpression getConjunct(Set<OWLClassExpression> conj) {
        return factory.getOWLObjectIntersectionOf(conj);
    }

    public OWLClassExpression getDisjunct(Set<OWLClassExpression> dis) {
        return factory.getOWLObjectUnionOf(dis);
    }

    /**
     *
     * @param p property
     * @param q quantifier
     * @param c class
     * @return an object SELF restriction
     */
    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, Quantifier q) {
        if (q.equals(Quantifier.SELF)) {
            return getObjRestriction(p, q, thing, 0);
        } else {
            System.err.println("Quantifier: !=SELF");
            return null;
        }
    }

    /**
     *
     * @param p property
     * @param q quantifier
     * @param c class
     * @return a non-Cardinality object restriction
     */
    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, Quantifier q, OWLClassExpression c) {
        if (!(q.equals(Quantifier.MAX) || q.equals(Quantifier.MIN) || q.equals(Quantifier.EXACT))) {
            return getObjRestriction(p, q, c, 0);
        }
        System.err.println("Quantifier: needs cardinality");
        return null;
    }

    /**
     *
     * @param p property
     * @param q quantifier
     * @param c class
     * @param i cardinality
     * @return an object restriction
     */
    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, Quantifier q, OWLClassExpression c, int i) {
        switch (q) {
            case ONLY:
                return factory.getOWLObjectAllValuesFrom(p, c);
            //break;
            case SOME:
                return factory.getOWLObjectSomeValuesFrom(p, c);
            //break;
            case MIN:
                return factory.getOWLObjectMinCardinality(i, p, c);
            //break;
            case MAX:
                return factory.getOWLObjectMaxCardinality(i, p, c);
//                break;
            case EXACT:
                return factory.getOWLObjectExactCardinality(i, p, c);
            //              break;

            case SELF:
                return factory.getOWLObjectHasSelf(p);
            //            break;
            default:
                return null;
            //          break;
        }
    }

    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, OWLIndividual c) {

        return factory.getOWLObjectHasValue(p, c);

    }

    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, Quantifier q, OWLIndividual c) {
        if (q.equals(q.VALUE)) {
            return factory.getOWLObjectHasValue(p, c);
        } else {
            return null;
        }
    }

    public OWLClassExpression getObjRestriction(OWLObjectPropertyExpression p, Quantifier q, OWLIndividual c, int i) {
        if (q.equals(q.VALUE)) {
            return factory.getOWLObjectHasValue(p, c);
        } else {
            return null;
        }
    }

    /**
     *
     * @param p property
     * @param q quantifier
     * @param c class
     * @return a non-Cardinality object restriction
     */
    public OWLClassExpression getDataRestriction(OWLDataPropertyExpression p, Quantifier q, OWLDataRange c) {
        if (!(q.equals(Quantifier.MAX) || q.equals(Quantifier.MIN) || q.equals(Quantifier.EXACT))) {
            return getDataRestriction(p, q, c, 0);
        }
        System.err.println("Quantifier: needs cardinality");
        return null;
    }

    /**
     *
     * @param p property
     * @param q quantifier
     * @param c class
     * @param i cardinality
     * @return an object restriction
     */
    public OWLClassExpression getDataRestriction(OWLDataPropertyExpression p, Quantifier q, OWLDataRange c, int i) {
        switch (q) {
            case ONLY:
                return factory.getOWLDataAllValuesFrom(p, c);
            //break;
            case SOME:
                return factory.getOWLDataSomeValuesFrom(p, c);
            //break;
            case MIN:
                return factory.getOWLDataMinCardinality(i, p, c);
            //break;
            case MAX:
                return factory.getOWLDataMaxCardinality(i, p, c);
//                break;
            case EXACT:
                return factory.getOWLDataExactCardinality(i, p, c);
            //              break;


            default:
                return null;
            //          break;
        }

    }

    public OWLClassExpression getDataRestriction(OWLDataPropertyExpression p, OWLLiteral c) {

        return factory.getOWLDataHasValue(p, c);

    }

    public OWLClassExpression getDataRestriction(OWLDataPropertyExpression p, Quantifier q, OWLLiteral c) {
        if (q.equals(q.VALUE)) {
            return factory.getOWLDataHasValue(p, c);
            
        } else {
            return null;
        }
    }

    public OWLClassExpression getDataRestriction(OWLDataPropertyExpression p, Quantifier q, OWLLiteral c, int i) {
        if (q.equals(q.VALUE)) {
            return factory.getOWLDataHasValue(p, c);
        } else {
            return null;
        }
    }

/**
 * Encodes the region encoding in OWL
 * @param e encoding of the region
 * @return OWLDataRange representing this region
 */
    public OWLDataRange getRegionEncodingRestriction(String e){
        return factory.getOWLDatatypeRestriction(factory.getOWLDatatype(IRI.create("http://www.cs.rochester.edu/u/jorfan/space.xds#space")), OWLFacet.PATTERN, factory.getOWLLiteral(e, factory.getOWLDatatype(IRI.create("http://www.cs.rochester.edu/u/jorfan/space.xds#space"))));
    }
// @todo the same for properties and individuals
    // </editor-fold>
    //how do I correctly place pizza above hasTopping some Thing and not a more spefic version of has topping
// <editor-fold defaultstate="collapsed" desc="Graph building">
}
