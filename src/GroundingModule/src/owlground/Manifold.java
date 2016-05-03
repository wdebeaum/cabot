package owlground;

//import org.shogun.*;

public class Manifold {

	  static {
		    try{
		    	System.load("/usr/lib/jni/libmodshogun.so");
		    	//System.loadLibrary("modshogun");
		      System.out.println("Loaded modshogun API");
		    } catch(UnsatisfiedLinkError e) {
		      System.out.println("Couldn't load modshogun");
		      System.out.println(e.getMessage());
		    }
	  }
		    
	public Manifold() {
		//modshogun.init_shogun_with_defaults();
		//modshogun.exit_shogun();
	}

}
