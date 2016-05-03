%module nimod

%{
    #define SWIG_FILE_WITH_INIT
    #include "nimod.h"
%}

%include "numpy.i"

%init %{
    import_array();
%}

%apply (unsigned short** ARGOUTVIEW_ARRAY1, int* DIM1) {(unsigned short** arr, int* n)}
%apply (unsigned char** ARGOUTVIEW_ARRAY1, int* DIM1) {(unsigned char** arr, int* n)}

%apply (unsigned char* INPLACE_ARRAY1, int DIM1) {(unsigned char* arr, int n)}
%apply (unsigned short* INPLACE_ARRAY1, int DIM1) {(unsigned short* arr, int n)}
%apply (double* INPLACE_ARRAY1, int DIM1) {(double* arr, int n)}
%apply (float* INPLACE_ARRAY1, int DIM1) {(float* arr, int n)}

%apply (int* ARGOUT_ARRAY1, int DIM1) {(int* arr, int n)}
%include "nimod.h"
