/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package owlground.geometry;

import java.util.ArrayList;
import java.util.List;
import org.apache.commons.math3.geometry.euclidean.threed.Euclidean3D;
import org.apache.commons.math3.geometry.euclidean.threed.Plane;
import org.apache.commons.math3.geometry.euclidean.threed.PolyhedronsSet;
import org.apache.commons.math3.geometry.euclidean.threed.SubPlane;
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D;
import org.apache.commons.math3.geometry.euclidean.twod.Euclidean2D;
import org.apache.commons.math3.geometry.euclidean.twod.PolygonsSet;
import org.apache.commons.math3.geometry.euclidean.twod.SubLine;
import org.apache.commons.math3.geometry.euclidean.twod.Vector2D;
import org.apache.commons.math3.geometry.partitioning.AbstractSubHyperplane;
import org.apache.commons.math3.geometry.partitioning.BSPTree;
import org.apache.commons.math3.geometry.partitioning.BoundaryAttribute;
import org.apache.commons.math3.geometry.partitioning.Region;
import org.apache.commons.math3.geometry.partitioning.SubHyperplane;

/**
 *
 * @author arwillis
 */
public class BSPMesh2 {

    BSPTree<Euclidean3D> tree = null;

    public BSPMesh2(float[] coords, int[] indices) {
        double size;
        ArrayList<SubHyperplane<Euclidean3D>> subHyperplaneList = new ArrayList();
        for (int idx = 0; idx < indices.length; idx += 3) {
            int idxA = indices[idx] * 3;
            int idxB = indices[idx + 1] * 3;
            int idxC = indices[idx + 2] * 3;
            Vector3D v_1 = new Vector3D(coords[idxA], coords[idxA + 1], coords[idxA + 2]);
            Vector3D v_2 = new Vector3D(coords[idxB], coords[idxB + 1], coords[idxB + 2]);
            Vector3D v_3 = new Vector3D(coords[idxC], coords[idxC + 1], coords[idxC + 2]);
            Vector3D[] vertices = {v_1, v_2, v_3};
            Plane polyPlane = new Plane(v_1, v_2, v_3);
            ArrayList<SubHyperplane<Euclidean2D>> lines = new ArrayList();

            Vector2D[] projPts = new Vector2D[vertices.length];
            for (int ptIdx = 0; ptIdx < projPts.length; ptIdx++) {
                projPts[ptIdx] = polyPlane.toSubSpace(vertices[ptIdx]);
            }

            SubLine lineInPlane = null;
            for (int ptIdx = 0; ptIdx < projPts.length; ptIdx++) {
                lineInPlane = new SubLine(projPts[ptIdx], projPts[(ptIdx + 1) % projPts.length]);
                lines.add(lineInPlane);
            }
            Region<Euclidean2D> polyRegion = new PolygonsSet(lines);
            SubPlane polygon = new SubPlane(polyPlane, polyRegion);
            size = polyRegion.getSize();
            Vector3D[][] verticesTest = getVertices(polygon);
            subHyperplaneList.add(polygon);
        }
        PolyhedronsSet polyhedronsSet = new PolyhedronsSet(subHyperplaneList);
        BSPTree<Euclidean3D> myTree = polyhedronsSet.getTree(true);
        size = polyhedronsSet.getSize();
        size = polyhedronsSet.getBoundarySize();
        tree = myTree;
        Vector3D[][] vertices = getVertices((SubPlane) ((BoundaryAttribute) tree.getAttribute()).getPlusOutside());
        System.out.println("END");
    }

    Vector3D[][] getVertices(SubPlane subPlane) {
        Region<Euclidean2D> region = ((AbstractSubHyperplane) subPlane).getRemainingRegion();
        PolygonsSet polygonset = (PolygonsSet) region;
        Vector2D[][] vertices = polygonset.getVertices();
        Vector2D curVertex = null;
        Plane plane = (Plane) subPlane.getHyperplane();
        Vector3D[][] allVertices = new Vector3D[vertices.length][0];
        for (int polygonIdx = 0; polygonIdx < vertices.length; polygonIdx++) {
            Vector3D[] vertices3D = new Vector3D[vertices[polygonIdx].length];
            for (int vertexIdx = 0; vertexIdx < vertices3D.length; vertexIdx++) {
                curVertex = vertices[0][vertexIdx];
                if (curVertex != null) {
                    vertices3D[vertexIdx] = plane.toSpace(curVertex);
                }
            }
            allVertices[polygonIdx] = vertices3D;
        }
        return allVertices;
    }

/*    public static void main(String[] args) {
        float[] tetCoords = {1, 0, 0,
            2, 0, 0,
            1, 1, 0,
            1, 0, 1};
        int[] tetIndices = {0, 2, 1,
            0, 1, 3,
            0, 3, 2,
            2, 3, 1};
        float[] cubeCoords1 = {1, -1, -1,
            1, -1, 1,
            -1, -1, 1,
            -1, -1, -1,
            1, 1, -1,
            1, 1, 1,
            -1, 1, 1,
            -1, 1, -1
        };
        float[] cubeCoords = {1, -1, -1,
            1, -1, 1,
            -1, -1, 1,
            -1, -1, -1,
            1, 1, -1f,
            1f, 1, 1.000001f,
            -1, 1, 1,
            -1, 1, -1
        };
        int[] cubeIndices = {0, 1, 2, 1, 2, 3,
            4, 7, 6, 7, 6, 5,
            0, 4, 5, 4, 5, 1,
            1, 5, 6, 5, 6, 2,
            2, 6, 7, 6, 7, 3,
            4, 0, 3, 0, 3, 7};
        //float[] coords = cubeCoords1; // doesn't have error
        float[] coords = cubeCoords; // has error
        int[] coordIndices = cubeIndices;
        BSPMesh2 bspTree = new BSPMesh2(coords, coordIndices);
    }*/
}
