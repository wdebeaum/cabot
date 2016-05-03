package owlground.utilities;

import org.jblas.DoubleMatrix;

public class Conversions {

	public static DoubleMatrix convertRgbToLab(DoubleMatrix rgb) {
		
		final int RED = 0;
		final int GREEN = 1;
		final int BLUE = 2;
		
		final double X_n = .950456f;
		final double Z_n = 1.088754f;
		final int delta = 128;
		
		double x,y,z;
		double fx,fy,fz;
		double l_star, a_star,b_star;
		double r = rgb.get(RED) / 255.0f;
		double g = rgb.get(GREEN) / 255.0f;
		double b = rgb.get(BLUE) / 255.0f;
		
		x = .412453f * r + .357580f * g + .180423f * b;
		y = .212671f * r + .715160f * g + .072169f * b;
		z = .019334f * r + .119193f * g + .950227f * b;
		
		x = x / X_n;
		z = z / Z_n;
		
		if (y > .008856f)
			l_star = (double)(116 * Math.pow(y, 1.0 / 3.0) - 16);
		else
			l_star = 903.3f * y;
		
		fx = (x > .008856 ? (double)Math.pow(x, 1.0 / 3.0) : 7.787f * x + 16.0f/116);
		fy = (y > .008856 ? (double)Math.pow(y, 1.0 / 3.0) : 7.787f * y + 16.0f/116);
		fz = (z > .008856 ? (double)Math.pow(z, 1.0 / 3.0) : 7.787f * z + 16.0f/116);
		
		a_star = 500 * (fx - fy);
		b_star = 200 * (fy - fz);
		
		DoubleMatrix lab = DoubleMatrix.zeros(3);
		lab.put(0, l_star);
		lab.put(1, a_star);
		lab.put(2, b_star);
		
		return lab;
	}
	
	public static DoubleMatrix convertRgbToLab2(DoubleMatrix rgb) {
		//http://www.brucelindbloom.com
		final int RED = 0;
		final int GREEN = 1;
		final int BLUE = 2;
		
		double r, g, b, X, Y, Z, fx, fy, fz, xr, yr, zr;
		double Ls, as, bs;
		double eps = 216.f/24389.f;
		double k = 24389.f/27.f;
		   
		double Xr = 0.964221f;  // reference white D50
		double Yr = 1.0f;
		double Zr = 0.825211f;
		
		// RGB to XYZ
		r = rgb.get(RED) / 255.f; //R 0..1
		g = rgb.get(GREEN) / 255.f; //G 0..1
		b = rgb.get(BLUE) / 255.f; //B 0..1
		
		// assuming sRGB (D65)
		if (r <= 0.04045)
			r = r/12.92f;
		else
			r = (double) Math.pow((r+0.055)/1.055,2.4);
		
		if (g <= 0.04045)
			g = g/12.92f;
		else
			g = (double) Math.pow((g+0.055)/1.055,2.4);
		
		if (b <= 0.04045)
			b = b/12.92f;
		else
			b = (double) Math.pow((b+0.055)/1.055,2.4);
		
		
		X =  0.436052025f*r     + 0.385081593f*g + 0.143087414f *b;
		Y =  0.222491598f*r     + 0.71688606f *g + 0.060621486f *b;
		Z =  0.013929122f*r     + 0.097097002f*g + 0.71418547f  *b;
		
		// XYZ to Lab
		xr = X / Xr;
		yr = Y / Yr;
		zr = Z / Zr;
				
		if ( xr > eps )
			fx =  (double) Math.pow(xr, 1/3.);
		else
			fx = (double) (k * xr + (16. / 116.));
		 
		if ( yr > eps )
			fy =  (double) Math.pow(yr, 1/3.);
		else
		fy = (double) (k * yr + (16. / 116.));
		
		if ( zr > eps )
			fz =  (double) Math.pow(zr, 1/3.);
		else
			fz = (double) (k * zr + (16. / 116));
		
		Ls = ( 116 * fy ) - 16;
		as = 500*(fx-fy);
		bs = 200*(fy-fz);
		
		DoubleMatrix result = DoubleMatrix.zeros(3);
		result.put(0, (int)(Ls + .5));
		result.put(1, (int) (as + .5)); 
		result.put(2, (int) (bs + .5));
		
		return result;
	} 
	//http://www.cs.rit.edu/~ncs/color/t_convert.html#RGB%20to%20HSV%20&%20HSV%20to%20RGB
	public static DoubleMatrix convertRgbToHsv(DoubleMatrix rgb)
	{
		rgb.divi(255.0f);
		
		double min, max, delta;
		min = rgb.min();
		max = rgb.max();
		DoubleMatrix hsv = DoubleMatrix.zeros(3);
		hsv.put(2, max);
		delta = max - min;
		
		if (max > 0.0f)
			hsv.put(1, delta / max);
		else
			return null;
		
		if (rgb.get(0) == max)
			hsv.put(0, (rgb.get(1) - rgb.get(2)) / delta);
		else if (rgb.get(1) == max)
			hsv.put(0, 2 + (rgb.get(2) - rgb.get(0)) / delta);
		else
			hsv.put(0, 4 + (rgb.get(0) - rgb.get(1)) / delta);
		
		hsv.put(0, hsv.get(0) * 60.0f);
		
		if (hsv.get(0) < 0)
			hsv.put(0, hsv.get(0) + 360.0f);
		
		return hsv;
	}
}
