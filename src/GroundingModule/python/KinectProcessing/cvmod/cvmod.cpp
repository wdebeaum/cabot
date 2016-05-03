#include <cstdio>
#include <cmath>
#include <windows.h>
#include <GL/gl.h>
#include <opencv/cv.h>
#include <opencv/highgui.h>

unsigned char *g_rgb = NULL;
unsigned short *g_depth = NULL;
unsigned char *g_adjacency = NULL;
unsigned char *g_blobmask = NULL;
double *g_vars = NULL;
float gx1, gx2, gy1, gy2, gz1, gz2, gd1, gd2, g_sa;

void set_rgb(unsigned char* arr, int n)
{
    if(n == 0) g_rgb = NULL;
    else g_rgb = arr;
}

void set_depth(unsigned short* arr, int n)
{
    if(n == 0) g_depth = NULL;
    else g_depth = arr;
}

void set_adjacency(unsigned char* arr, int n)
{
    if(n == 0) g_adjacency = NULL;
    else g_adjacency = arr;
}

void set_blobmask(unsigned char* arr, int n)
{
    if(n == 0) g_blobmask = NULL;
    else g_blobmask = arr;
}

void set_vars(double* arr, int n)
{
    if(n == 0) { 
        g_vars = NULL;
        return;
    }
    g_vars = arr;
    gx1 = g_vars[0];
    gx2 = g_vars[1];
    gy1 = g_vars[2];
    gy2 = g_vars[3];
    gz1 = g_vars[4];
    gz2 = g_vars[5];
    gd1 = g_vars[6];
    gd2 = g_vars[7];
    g_sa = g_vars[8];
}

void
depth2skel(int x, int y, float d, float *ix, float *iy, float *iz)
{
    float fZ = float(d)/1000.;
    float fX = (float(x)/320. - .5) * (3.501e-3 * fZ) * 320;
    float fY = (.5 - float(y)/240.) * (3.501e-3 * fZ) * 240;
    *ix = fX;
    *iy = fY;
    *iz = fZ;
}

int
is_incrop(float nx, float ny, float nz)
{
    if(gx1 < nx && nx < gx2 && gy1 < ny && ny < gy2 && gz1 < nz && nz < gz2) {
        return 1;
    }
    return 0;
}

float
rad(float deg)
{
    return deg*(3.14159265/180.);
}

void
conv1(float x, float y, float z, float *nx, float *ny, float *nz, float deg)
{
    float c_a = cos(rad(deg));
    float s_a = sin(rad(deg));
    float fx = -x;
    float fy = y;
    float fz = z;
    //*nx = -(fz*s_a+fx*c_a);
    //*ny = fy;
    //*nz = fz*c_a+fx*s_a;
    *nx = -fx;
    *ny = -fz*s_a+fy*c_a;
    *nz = fz*c_a+fy*s_a;
}

void
conv2(float x, float y, float z, float *nx, float *ny, float *nz, float deg)
{
    float c_a = cos(rad(deg));
    float s_a = sin(rad(deg));
    float fx = -x;
    float fy = y;
    float fz = z;
    //*nx = -(fz*s_a+fx*c_a);
    //*ny = fy;
    //*nz = fz*c_a+fx*s_a;
    *nx = -(-fy*s_a+fx*c_a);
    *ny = fy*c_a+fx*s_a;
    *nz = fz;
}


void
output3d(void)
{
    if(g_depth == NULL) {
        return;
    }
    if(g_vars == NULL) {
        return;
    }
    int i, j;
    float d;
    float ix, iy, iz, nx, ny, nz;
    glBegin(GL_POINTS);
    glColor3f(1., 1., 1.);
    for(j = 0; j < 480; j++) {
        for(i = 0; i < 640; i++) {
            d = g_depth[j*640+i];
            depth2skel(i/2., j/2., d, &ix, &iy, &iz);
            //nx = ix;
            //ny = iy;
            //nz = iz;
            conv1(ix, iy, iz, &nx, &ny, &nz, gd1);
            ix = nx;
            iy = ny;
            iz = nz;
            conv2(ix, iy, iz, &nx, &ny, &nz, gd2);
            if(is_incrop(nx, ny, nz) == 1) {
                glColor3f(0., 1., 0.);
                glVertex3f(-nx, ny, nz);
            }
            else if(g_sa != 0.) {
                glColor3f(1., 1., 1.);
                glVertex3f(-nx, ny, nz);
            }
        }
    }
    glEnd();
}

float
distsq(float x1, float y1, float z1, float x2, float y2, float z2)
{
    float dX = abs((double)(x1 - x2));
    float dY = abs((double)(y1 - y2));
    float dZ = abs((double)(z1 - z2));
    float dsq = dX*dX+dY*dY+dZ*dZ;
    return dsq;
}

void
process_blob(void)
{
    int i, j;
    int i2, j2;
    float d, d2;
    float ix, iy, iz, nx, ny, nz;
    float ix2, iy2, iz2, nx2, ny2, nz2;
    float dsq;
    for(j = 1; j < 480-1; j++) {
        for(i = 1; i < 640-1; i++) {
            d = g_depth[j*640+i];
            depth2skel(i/2., j/2., d, &ix, &iy, &iz);
            //nx = ix;
            //ny = iy;
            //nz = iz;
            conv1(ix, iy, iz, &nx, &ny, &nz, gd1);
            ix = nx;
            iy = ny;
            iz = nz;
            conv2(ix, iy, iz, &nx, &ny, &nz, gd2);
            if(is_incrop(nx, ny, nz) == 1) {
                // pixel above
                i2 = i;
                j2 = j-1;
                d2 = g_depth[j2*640+i2];
                depth2skel(i2/2., j2/2., d2, &ix2, &iy2, &iz2);
                //nx2 = ix2;
                //ny2 = iy2;
                //nz2 = iz2;
                conv1(ix2, iy2, iz2, &nx2, &ny2, &nz2, gd1);
                ix2 = nx2;
                iy2 = ny2;
                iz2 = nz2;
                conv2(ix2, iy2, iz2, &nx2, &ny2, &nz2, gd2);
                dsq = distsq(nx, ny, nz, nx2, ny2, nz2);
                if(dsq < .0010) {
                    //g_adjacency[j2*(640-1)+i2] = 255;
                    g_adjacency[j2*(640)+i2] = 255;
                }
                else {
                    //g_adjacency[j2*(640-1)+i2] = 0;
                    g_adjacency[j2*(640)+i2] = 0;
                }
                // pixel to the left
                i2 = i-1;
                j2 = j;
                d2 = g_depth[j2*640+i2];
                depth2skel(i2/2., j2/2., d2, &ix2, &iy2, &iz2);
                //nx2 = ix2;
                //ny2 = iy2;
                //nz2 = iz2;
                conv1(ix2, iy2, iz2, &nx2, &ny2, &nz2, gd1);
                ix2 = nx2;
                iy2 = ny2;
                iz2 = nz2;
                conv2(ix2, iy2, iz2, &nx2, &ny2, &nz2, gd2);
                dsq = distsq(nx, ny, nz, nx2, ny2, nz2);
                if(dsq < .0010) {
                    //g_adjacency[j2*(640-1)+i2] = 255;
                    g_adjacency[j2*(640)+i2] = 255;
                }
                else {
                    //g_adjacency[j2*(640-1)+i2] = 0;
                    g_adjacency[j2*(640)+i2] = 0;
                }
            }
            else {
                i2 = i;
                j2 = j-1;
                //g_adjacency[j2*(640-1)+i2] = 0;
                g_adjacency[j2*(640)+i2] = 0;
                i2 = i-1;
                j2 = j;
                //g_adjacency[j2*(640-1)+i2] = 0;
                g_adjacency[j2*(640)+i2] = 0;
            }
        }
    }
}

IplImage*
createiplimage8(int w, int h, unsigned char* d)
{
    IplImage* hdr = cvCreateImageHeader(cvSize(w, h), IPL_DEPTH_8U, 1);
    hdr->imageData = (char *)d;
    return hdr;
}

int
createblobmask(void)
{
    IplImage* mask = createiplimage8(640-1, 480-1, g_adjacency);
    cvShowImage("HELLLO", mask);
    cvWaitKey(10);
    return 0;
    /*
    int i, j;
    unsigned char d;
    int blob_idx = 0;
    for(j = 0; j < 480-1; j++) {
        //printf("j= %d\n", j);
        for(i = 0; i < 640-1; i++) {
            //printf("i= %d\n", i);
            d = g_adjacency[j*(640-1)+i];
            if(d != 255) {
                continue;
            }
            blob_idx += 1;
            if(blob_idx == 255) {
                break;
            }
            printf("bi= %d\n", blob_idx);
            cvFloodFill(mask, cvPoint(i, j), cvScalar(blob_idx));
            //cvFloodFill(mask, cvPoint(i, j), cvScalar(blob_idx), cvScalarAll(0), cvScalarAll(0), NULL, 4, NULL);
        }
    }
    return blob_idx;
    */
}

void
helloworld(void) {
    /*
    if (g_vars == NULL) {
        printf("Hello World NO VARS\n");
    }
    else {
        printf("Hello World %f\n", g_vars[0]);
    }
    */
    int i, j;
    for(j = 0; j < 541; j++) {
        for(i = 0; i < 960; i++) {
            g_rgb[j*960+i] = 0;
        }
    }

    IplImage* hdr = cvCreateImageHeader(cvSize(960, 541), IPL_DEPTH_8U, 1);
    hdr->imageData = (char *)g_rgb;
    cvCircle(hdr, cvPoint(200, 100), 100, cvScalar(255), -1);
}
