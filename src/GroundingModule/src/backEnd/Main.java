package backEnd;




import java.io.IOException;

import javax.xml.stream.XMLStreamException;

import org.jblas.DoubleMatrix;
import org.semanticweb.owlapi.expression.ParserException;
import org.semanticweb.owlapi.model.*;

import owlground.FeatureManager;
import owlground.perception.PerceptCluster;
import owlground.spaces.SpaceManager;
import owlground.utilities.Conversions;


/**
 *
 * @author jansen
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    
    public static void main(String[] args) throws OWLOntologyStorageException, ParserException {
      
    	SpaceManager sm = new SpaceManager();
    	FeatureManager fm = new FeatureManager(sm);
    	
    	try {
			fm.loadTranscriptFile("../Systems/bolt/dryrun_transcript");
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
    	
    	try {
			fm.loadInputXmlFile("../Systems/bolt/dryrun_features.xml");

		} catch (XMLStreamException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    	for (PerceptCluster pc : fm.getAttendedPerceptClusters().values())
    	{
    		System.out.println(pc.toString());
    	}
    	
    	fm.generateWordPerceptClusters();
    	
    	System.out.println(Conversions.convertRgbToHsv(new DoubleMatrix(new double[]{20f,210f,10f})));
    	
    	System.out.println("Color for red");
    	System.out.println(fm.getColorForWord("red"));
    	System.out.println("Color for yellow");
    	System.out.println(fm.getColorForWord("yellow"));
    	System.out.println("Color for blue");
    	System.out.println(fm.getColorForWord("blue"));

//
//    	//OWLBackend back = new OWLBackend("data.owl");
//     OWLBackend back = new OWLBackend("file:///c:/Users/Ian/Documents/Java/workspace/OWLGround/data.owl");
//
//
//        //representing a data range encoded as "<min> <max>"
//        OWLClassExpression r1 = back.getDataRestriction(back.getDataProperty("space_Encoding"),Quantifier.ONLY,back.getRegionEncodingRestriction("10 18"));
//        OWLClassExpression r2 = back.getDataRestriction(back.getDataProperty("space_Encoding"),Quantifier.ONLY,back.getRegionEncodingRestriction("1 20"));
//     
//        boolean ans1 = back.reasoner.isEntailed(back.factory.getOWLSubClassOfAxiom(r1, r2));
//        //won't answer correctly until the reasoner has been modified
//        System.err.println("r1 -> r2 = "+ans1+"\n");
//
//        ans1 = back.reasoner.isEntailed(back.factory.getOWLSubClassOfAxiom(r2, r1));
//        System.err.println("r2 -> r1 = "+ans1);
//        //System.out.println("Parents of RGB");
//        //for (OWLClass c : back.getSubClasses(r2, true)) {  System.out.println("\t" + back.renderer.render(c));}
//
//



/*            //load ontology
            //OWLBackend back = new OWLBackend("file:///home/jansen/Documents/work/summer12/DLLA/mock.owl");
    		//OWLBackend back = new OWLBackend("http://cs.rochester.edu/~iperera/mock.owl");
    		OWLBackend back = new OWLBackend("file:///e:/mock.owl");
    		
            System.out.println("\n\n");
            //expect some messages about name conflicts. I'll probably add a logger later. All this means is that you should specfiy the full IRI when refering to them

            //get a class we know exists:
            OWLClass rgb1 = back.getNamedClass("RGB");
            OWLClass addressee1 = back.getNamedClass("addressee");
            OWLClass space = back.getNamedClass("Space");
            //alternatively, use this if more than one namespace has the simple name "RGB"
            OWLClass rgb2 = back.getNamedClass("RGB", "http://www.cs.rochester.edu/u/jorfan/lfandgum/Ontology1319244798415.owl");
             OWLClass addressee2 = back.getNamedClass("addressee", "http://trips.ihmc.us/ont");


            System.out.println("RGB1 "+ (rgb1.equals(rgb2)?"==":"=/=") +" RGB2");
            System.out.println("addressee1 "+ (addressee1.equals(addressee2)?"==":"=/=") +" addressee2");
            

            //direct super classes
            System.out.println("Parents of RGB1");
            for(OWLClass c: back.getSuperClasses(rgb1, true))
                System.out.println("\t"+back.renderer.render(c));

            //indirect subclasses of Space
System.out.println("\nDecendants of Space");
            for(OWLClass c: back.getSubClasses(space, false))
                System.out.println("\t"+back.renderer.render(c));
//creating a new class:
            //uses the same namespace as the ontology

            OWLClass curved = back.createClass("Curved");

            //make curved a subclass of space
            back.addSuperClass(curved, space, false);
            //note that this doesn't actually affect the ontology yet
            System.out.println("Ancestors of curved:" + back.getSuperClasses(curved, false).size());
            back.applyPendingChanges();
            System.out.println("*apply changes*");
            System.out.println("Ancestors of curved:" + back.getSuperClasses(curved, false).size());
            for(OWLClass c: back.getSuperClasses(curved, false))
                System.out.println("\t"+back.renderer.render(c));


            //adding some restrictions:
            //get restrictions
            OWLClassExpression r1 = back.getObjRestriction(back.getNamedObjProperty("hasSimFunction"), Quantifier.ONLY, back.getNamedClass("sim"));
            //I haven't figured out an elegant way to abstract the data literals and ranges so just go through the data factory for now.

            OWLClassExpression r2 = back.getDataRestriction(back.getNamedDatProperty("dimensions"), Quantifier.VALUE, back.factory.getOWLLiteral(3));

            back.addSuperClass(curved, r1, false);
            //can set the flag to true if you want to commit the changes when you're done
            back.addSuperClass(curved, r2, true);
   System.out.println("\n\n Restrictions on curved:");
            for(OWLClassExpression c: back.getAnonymousDirectSuperClasses(curved))
                System.out.println("\t"+back.renderer.render(c));
//If you know manchester owl syntax then:
             OWLClass curved2 = back.createClass("CurvedEQV");
             back.addSuperClass(curved2, back.parser.parse("Space and hasSimFunction only sim and dimensions value 3"),true);

//regions: back.getRegionEncodingRestriction(String e)

  //representing a data range encoded as "<min> <max>"

 OWLClassExpression r1 = back.getDatRestriction(back.getDatProperty("space_Encoding"),Quantifier.ONLY,back.getRegionEncodingRestriction("10 18"));
        OWLClassExpression r2 = back.getDatRestriction(back.getDatProperty("space_Encoding"),Quantifier.ONLY,back.getRegionEncodingRestriction("1 20"));
     
        boolean ans1 = back.reasoner.isEntailed(back.factory.getOWLSubClassOfAxiom(r1, r2));
        //won't answer correctly until the reasoner has been modifer
        System.err.println("r1 -> r2 = "+ans1+"\n");

        ans1 = back.reasoner.isEntailed(back.factory.getOWLSubClassOfAxiom(r2, r1));
        System.err.println("r2 -> r1 = "+ans1);
        //System.out.println("Parents of RGB");
        //for (OWLClass c : back.getSubClasses(r2, true)) {  System.out.println("\t" + back.renderer.render(c));}




//save it and inspect it in protege if you'd like
back.saveOntology("file:///e:/mock2.owl");*/
            
        }





    }
   

    
   

//}

