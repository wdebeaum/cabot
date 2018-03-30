package geometry;

public interface Extents3D {

	public Extents1D getX();
	public Extents1D getY();
	public Extents1D getZ();
	
	public Extents2D getXY();
	public Extents2D getXZ();
	public Extents2D getYZ();
}
