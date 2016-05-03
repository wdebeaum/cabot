/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package owlground;

import java.util.Collection;
import java.util.Scanner;

import org.jblas.DoubleMatrix;

/**
 * Won't be used yet. Ties into the Ontology.
 *
 */
public class GroundedReasoner {

    /**
     * 
     * @param x
     * @param y
     * @return X intersect Y or null if X ^ Y = 0
     */
   static public String intersection(String x, String y) {
/*        System.out.println("intersection");
        System.out.println(x);
        System.out.println(y);
        BoxRegion br1 = new BoxRegion(x);
        BoxRegion br2 = new BoxRegion(y);

        BoxRegion result = (BoxRegion) br1.intersection(br2);
        String resultString = result.minCorner.get(0) + " " + result.maxCorner.get(0);
        System.out.println(resultString);
        return resultString;
*/
	   return null;
    }

    /**
     *
     * @param x
     * @param y
     * @return
     */
 static   public  String union(String x, String y) {
     System.out.println("union");
     System.out.println(x);
     System.out.println(y);
     return "";
    }

    /**
     *
     * @param x
     * @param y
     * @return true if x contains y (y->x)
     */
 static   public  boolean contains(String x, String y) {
/*     System.out.println("contains");

     BoxRegion br1 = new BoxRegion(x);
     BoxRegion br2 = new BoxRegion(y);
     System.out.println(x);
     System.out.println(y);
     System.out.println(br1.contains(br2));
     return br1.contains(br2);*/

	 	return false;
    }

    /**
     * X - C
     * @param x
     * @param c
     * @return
     */

   static public  String exclude(String x, Collection<String> c) {
          throw new UnsupportedOperationException("Not yet implemented");
        
    }


}
