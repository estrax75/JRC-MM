#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sys/param.h>
#include <errno.h>
#include <string.h>
#include <math.h>

#include <assert.h>

#include "mfhdf.h"
#include "hdf.h"

#include <gsl/gsl_linalg.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_vector.h>

#include "level_3_index.h"
#include "readwrite.h"
#include "inout.h"
#include "flag.h"

/* Max. number of files as inputs */
#define NbInputMax 31

#define RankMax 3

#define NMAX 256
#define NMAX2 25

#define TSTOP(x) { printf ("%s\n",x); exit(-1); }

#define R2D 57.29577951
#define PI 3.14159265358979323846

#define PROJECTION_RECTANGULAR "Equidistant Cylindrical"
#define PROJECTION_SINUSOIDAL "Sinusoidal"

#define NLINE_STEP_MAX 100
#define NbChunkMax 200

#define MaxValueByte 255


//void assert_1(void* p1, void* p2)
//{
//    assert(p1==p2);
//}
//
//void assert_n(int n, void** p1, void** p2)
//{
//    int i;
//    for(i=0; i<n; ++i)
//    {
//        assert(p1[i]==p2[i]);
//    }
//}
//
//#define ASSERT_1(p)   assert_1((void*)c_##p, (void*)p)
//#define ASSERT_N(n,p) assert_n(n,(void**)c_##p, (void**)p)

/* Function to compute the Mahalanobis Distance of a point */
//int FindMinMahDistance
//(const double [], /* Input 1st vector */
//        const double [], /* Input 2nd vector */
//        const double [], /* Input 3rd vector */
//        const size_t, /* Input size of vectors */
//        const size_t, /* Input 2 or 3 vectors */
//        double *, /* Output vector of distances */
//        double *, /* Output stdev 1st vec. */
//        double *, /* Output stdev 2nd vec. */
//        double *, /* Output stdev 3rd vec. */
//        int *); /* Output index of point with min Mah Dist. */

/* Function to compute and return the minimum euclidean distance of a point */
int FindMinEuclDistance(
    const double fpar[],
    const double rred[],
    const double rnir[],
    int n,
    int nv,
    double* dist,
    double* fpar_s_p,
    double* rred_s_p,
    double* rnir_s_p,
    int* imin_p);

int FindMinEuclDistanceUncert(
    const double fpar[],
    const double rred[],
    const double rnir[],
    const double fpar_u[],
    const double rred_u[],
    const double rnir_u[],
    int n,
    int nv,
    double* dist,
    double* fpar_s_p,
    double* rred_s_p,
    double* rnir_s_p,
    int* imin_p);



/* Update flag value with the different days available */
void CompositeFlag(int, int, int, int, int, uint8 **, uint8 *, uint8 *, uint8 *, int16 *);

/* Compute vegetation index and related variables */
//void ComputeFPAR(int, int, int, int, int, uint8 **, uint8,
//        uint8 **, uint8 *, uint8 *, uint8 *, uint8 *, uint8 *);

/* Compute vegetation index and related variables using also the rectified BRFs*/
void ComputeFPARwRect(int NbInput, int line, int nelem, int elem1, int elem2,
        uint8 *p_FAPAR[NbInputMax],    /* Input FPAR */
        int16 *p_brf_r[NbInputMax],    /* Input Rect R */
        int16 *p_brf_n[NbInputMax],    /* Input Rect NIR */

        uint8 badvalue,                /* Input bad value */
        uint8 *p_flag[NbInputMax],     /* Input flag */

        uint8 *FAPAR,                  /* Output selected FPAR */
        int16 *brf_r,                  /* Output selected rect R */
        int16 *brf_n,                  /* Output selected rect NIR */

        uint8 *dev_FAPAR,              /* output stdev FPAR */
        int16 *dev_brf_r,              /* Output stdev rect R */
        int16 *dev_brf_n,              /* Output stdev rect NIR */

        uint8 *p_NbValue,              /* Output nb of values used */
        uint8 *p_ValueNb);             /* Output file number selected */

void ComputeFPARwRectUncert(int NbInput, 
        int line, int nelem, int elem1, int elem2,
        uint8 *p_FAPAR[NbInputMax],    /* Input FPAR */
        int16 *p_brf_r[NbInputMax],    /* Input Rect R */
        int16 *p_brf_n[NbInputMax],    /* Input Rect NIR */

        int16 *p_FAPAR_u[NbInputMax],  /* Input Uncertainty FPAR */
        int16 *p_brf_r_u[NbInputMax],  /* Input Uncertainty Rect R */
        int16 *p_brf_n_u[NbInputMax],  /* Input Uncertainty Rect NIR */

        uint8 badvalue,                /* Input bad value */
        uint8 *p_flag[NbInputMax],     /* Input flag */

        uint8 *FAPAR,                  /* Output selected FPAR */
        int16 *brf_r,                  /* Output selected rect R */
        int16 *brf_n,                  /* Output selected rect NIR */

        int16 *FAPAR_u,                /* Output selected Uncertainty FPAR */
        int16 *brf_r_u,                /* Output selected Uncertainty rect R */
        int16 *brf_n_u,                /* Output selected Uncertainty rect NIR */

        uint8 *dev_FAPAR,              /* output stdev FPAR */
        int16 *dev_brf_r,              /* Output stdev rect R */
        int16 *dev_brf_n,              /* Output stdev rect NIR */
        
        uint8 *p_NbValue,                /* Output nb of values used */
        uint8 *p_ValueNb);                /* Output file number selected */

/* Choose the variable from the different input files according to
   the result of Compute_Land */

void Select_BRF_TOA(int, int, int, int, uint8 *, uint8,
        int16 **, int16 **, int16 **, int16 **,
        int16 *, int16 *, int16 *, int16 *);

//void Select_BRF_TOA2(int, int, int, int, uint8 *, uint8,
//        int16 **, int16 **, int16 **, int16 **, int16 **, int16 **, int16 **, int16 **,
//        int16 *, int16 *, int16 *, int16 *, int16 *, int16 *, int16 *, int16 *);

//void Select_BRF_Rec(int, int, int, int, uint8 *, uint8,
//        int16 **, int16 **, int16 *, int16 *);


void Select_Angles(int, int, int, int, uint8 *, uint8,
        int16 **, int16 **, int16 **,
        int16 *, int16 *, int16 *);

void* allocate(const char* what, size_t count, size_t size);
char* strclone(char* s);

double mean(int, const double*);
double stdv(int, const double*, double);
double sq(double x);


/* --------------------------------------------------------------------*/
/*
   l3time : routine to compute the time-composite for land applications.
   
 type               I           type of surface: 1: land (and full calculations), 
                                0: sea, calculations for FPAR and flag only.
 NbFiles            I           number of files.
 filename           I           list of the input file names.   
 indexFile          I           link the number of the file to its actual day 
                                (from 1 to 10 or 28-to-31).
 outfile            I           output file name.
 nline              I           number of lines, constant for all files.
 nelem              I           number of grid points for each line.
 projection         I           projection of the map.
 landmask           I           land mask array for the map considered (bathymetry).
 latmin             I           minimum latitude fo the map.
 latmax             I           maximum latitude fo the map. 
 lonmin             I           minimum longitude fo the map.   
 lonmax             I           maximum longitude fo the map.
 compute_u          I           if compute the uncertainty


 Outputs : none (writes outputs in output file). 

   Implementation: F. Melin - JRC-SAI/ME, 01/2000
   Modification 08/2001
   JRC-IES/GEM, 12/2008
 */

/* --------------------------------------------------------------------*/

void l3time(int NbFiles,
        char *inputfiles[NbInputMax],
        uint8 indexfiles[NbInputMax],
        char *outfile,
        int nline, int nelem,
        char *projection, int16 *landmask,
        float latmin, float lonmin, float latmax, float lonmax,
        int compute_u)
 {
    /* Used for read/write with HDF files */
    int32 sd_id, sd_id1, sd_id2;
    int32 status;
    int32 index, attrType, count;

    char name[NMAX];
    float xscale, xoffset;
    uint8 usemin_ui8, usemax_ui8;
    int16 usemin_i16, usemax_i16;

    uint8 bad_ui8;
    int16 bad_i16;

    char unit[NMAX2];
    
    /* -------------------------------------------------------- */
    /* Fields for each file */

    uint8 * p_FPAR[NbInputMax]; /* Vegetation index  */
    int16 * p_brf_r[NbInputMax]; /* Rectified BRF Red */
    int16 * p_brf_n[NbInputMax]; /* Rectified BRF NIR */

    int16 * p_FPAR_u[NbInputMax]; /* Uncertainty on Vegetation index  */
    int16 * p_brf_r_u[NbInputMax]; /* Uncertainty on Rectified BRF Red */
    int16 * p_brf_n_u[NbInputMax]; /* Uncertainty on Rectified BRF NIR */

    uint8 * p_flag[NbInputMax]; /* Flag value */

    int16 * p_brf_412[NbInputMax]; /* BRF TOA at 412nm */
    int16 * p_brf_443[NbInputMax]; /* BRF TOA at 443nm */
    int16 * p_brf_490[NbInputMax]; /* BRF TOA at 490nm */
    int16 * p_brf_510[NbInputMax]; /* BRF TOA at 510nm */
    int16 * p_brf_555[NbInputMax]; /* BRF TOA at 555nm */
    int16 * p_brf_670[NbInputMax]; /* BRF TOA at 670nm */
    int16 * p_brf_765[NbInputMax]; /* BRF TOA at 765nm */
    int16 * p_brf_865[NbInputMax]; /* BRF TOA at 865nm */

    int16 * p_sat_zenith[NbInputMax]; /* Satellite zenith angle */
    int16 * p_sun_zenith[NbInputMax]; /* Solar zenith angle */
    int16 * p_rel_azimuth[NbInputMax]; /* Relative azimuth */

    /* -------------------------------------------------------- */
    /* Fields for the output */

    uint8 *FPAR;  /* FAPAR  */
    int16 *brf_r; /* Rectified BRF Red */
    int16 *brf_n; /* Rectified BRF NIR */

    int16 *FPAR_u;  /* FAPAR  */
    int16 *brf_r_u; /* Rectified BRF Red */
    int16 *brf_n_u; /* Rectified BRF NIR */

    uint8 *MeanFPAR, *DevFPAR; /* Mean and mean deviation */
    int16 *DevRectR, *DevRectNIR; /* Mean and mean deviation */
    uint8 *FPARNb, *NbFPAR;
    uint8 *flag; /* Flag value */
    uint8 *ValueNb;

    int16 *brf_412; /* BRF TOA at 412nm */
    int16 *brf_443; /* BRF TOA at 443nm */
    int16 *brf_490; /* BRF TOA at 490nm */
    int16 *brf_510; /* BRF TOA at 510nm */
    int16 *brf_555; /* BRF TOA at 555nm */
    int16 *brf_670; /* BRF TOA at 670nm */
    int16 *brf_765; /* BRF TOA at 765nm */
    int16 *brf_865; /* BRF TOA at 865nm */

    int16 *sat_zenith; /* Satellite zenith angle */
    int16 *sun_zenith; /* Solar zenith angle */
    int16 *rel_azimuth; /* Relative azimuth */
    
    /* -------------------------------------------------------- */
    /* Duplicate variables for output */
    
    uint8 *ptr_FPAR; /* FAPAR  */
    int16 *ptr_brf_r; /* Rectified BRF Red */
    int16 *ptr_brf_n; /* Rectified BRF NIR */

    int16 *ptr_FPAR_u; /* Uncertainty for FAPAR  */
    int16 *ptr_brf_r_u; /* Uncertainty for Rectified BRF Red */
    int16 *ptr_brf_n_u; /* Uncertainty for Rectified BRF NIR */

    uint8 *ptr_DevFPAR; /* mean deviation */
    int16 *ptr_DevRectR; /* mean deviation */
    int16 *ptr_DevRectNIR; /* mean deviation */
    uint8 *ptr_FPARNb, *ptr_NbFPAR;
    uint8 *ptr_flag; /* Flag value */

    int16 *ptr_brf_412; /* BRF TOA at 412nm */
    int16 *ptr_brf_443; /* BRF TOA at 443nm */
    int16 *ptr_brf_490; /* BRF TOA at 490nm */
    int16 *ptr_brf_510; /* BRF TOA at 510nm */
    int16 *ptr_brf_555; /* BRF TOA at 555nm */
    int16 *ptr_brf_670; /* BRF TOA at 670nm */
    int16 *ptr_brf_765; /* BRF TOA at 765nm */
    int16 *ptr_brf_865; /* BRF TOA at 865nm */

    int16 *ptr_sat_zenith; /* Satellite zenith angle */
    int16 *ptr_sun_zenith; /* Solar zenith angle */
    int16 *ptr_rel_azimuth; /* Relative azimuth */

    int16 ilimit;

    /* -------------------------------------------------------- */
    int f, i;
    int line, elem;
    int line1, line2, line12;

    /* Variables to define output arrays */
    int rank, ndim[RankMax], len;
    char *dim_name[RankMax];
    int elem1, elem2;
    int *selem, *eelem, *NbRow, *NbBad;
    double dlat;
    int ntotal;
    float lat;

    /* Variables for chunks */
    int NbChunk;
    int NbLinePerChunk[NbChunkMax];
    int StartChunkLine[NbChunkMax];
    int EndChunkLine[NbChunkMax];
    int NLINE_STEP;
    int ichunk;

    /* -------------------------------------------------------------------------- */
    /* Use the projection to know what is the domain concerned by the calculation */

    selem = (int*) allocate("selem", nline, sizeof (int));
    eelem = (int*) allocate("eelem", nline, sizeof (int));
    NbRow = (int*) allocate("NbRow", nline, sizeof (int));
    NbBad = (int*) allocate("NbBad", nline, sizeof (int));

    if (strcmp(projection, PROJECTION_RECTANGULAR) == 0) {
        for (line = 0; line < nline; line++) {
            selem[line] = 0;
            eelem[line] = nelem;
            NbBad[line] = 0;
            NbRow[line] = nelem;
        }
    }

    if (strcmp(projection, PROJECTION_SINUSOIDAL) == 0) {
        dlat = ((double) (latmax - latmin));
        for (line = 0; line < nline; line++) {

            lat = (float) (((double) (line)) / ((double) (nline)) * dlat + (double) (latmin) + dlat / 2.0 / ((double) (nline)));

            NbRow[line] = (int) (cos(lat / R2D) * ((float) (nelem)) + 0.5);

            /* Make sure nb_elem is always even */
            if ((NbRow[line]) % 2 != 0) NbRow[line] = NbRow[line] + 1;
            if ((NbRow[line]) > nelem) NbRow[line] = nelem;
            if ((NbRow[line]) < 2) NbRow[line] = 2;

            NbBad[line] = (int) ((float) (nelem - (NbRow[line])) / 2.0 + 0.1);

            /* Check the consistency : good and bad rows should equal the total number of rows defined by nelem */
            ntotal = NbBad[line] + NbBad[line] + NbRow[line];

            if (ntotal != nelem) {
                printf(" ntotal= %d, nelem= %d at line %d\n", ntotal, nelem, line);
                exit(-1);
            }

            selem[line] = NbBad[line];
            eelem[line] = NbBad[line] + NbRow[line];
        }
    }

    /* --------------------------------------------------------------------------------------------- */
    /* Determine double loop over number of lines */

    NLINE_STEP = NLINE_STEP_MAX;

    for (ichunk = 0; ichunk < NbChunkMax; ichunk++) {
        NbLinePerChunk[ichunk] = -1;
        StartChunkLine[ichunk] = -1;
        EndChunkLine[ichunk] = -1;
    }

    if (nline > NLINE_STEP_MAX) {
        line = nline;
        NbChunk = 0;
        while (line > 0) {
            if (line >= NLINE_STEP_MAX) NbLinePerChunk[NbChunk] = NLINE_STEP_MAX;
            else NbLinePerChunk[NbChunk] = line;
            line = line - NLINE_STEP_MAX;
            NbChunk = NbChunk + 1;
        }

        StartChunkLine[0] = 1;
        EndChunkLine[0] = NbLinePerChunk[0];

        for (ichunk = 1; ichunk < NbChunk; ichunk++) {
            StartChunkLine[ichunk] = EndChunkLine[ichunk - 1] + 1;
            EndChunkLine[ichunk] = StartChunkLine[ichunk] + NbLinePerChunk[ichunk] - 1;
        }

    } else {

        NbChunk = 1;
        NbLinePerChunk[0] = nline;
        NLINE_STEP = nline;

    }

    StartChunkLine[0] = 1;
    EndChunkLine[0] = NbLinePerChunk[0];

    /* --------------------------------------------------------------------------------------------- */


    /* ------------------------------------------ */
    /* Build the dimensions needed for the definition of the variable */
    /* Assumes that only 2-D variables are written */

    rank = 2;

    ndim[0] = nline;
    ndim[1] = nelem;
    ndim[2] = 0;

    //len = strlen(LINE_3S) + 1;
    //dim_name[0] = (char *) malloc(len * sizeof(char));
    //strcpy(dim_name[0], LINE_3S);
    dim_name[0] = strclone(LINE_3S);

    //len = strlen(NSAMP_3S) + 1;
    //dim_name[1] = (char *) malloc(len * sizeof(char));
    //strcpy(dim_name[1], NSAMP_3S);
    dim_name[1] = strclone(NSAMP_3S);

    //dim_name[2] = (char *) malloc(1 * sizeof(char));
    //strncpy(dim_name[2], " ", 1);
    dim_name[2] = strclone(" ");

    /* ------------------------------------------ */
    /* Define new variables, that were not defined previously,
       since they are not in the inputs. */

    sd_id = SDstart(outfile, DFACC_RDWR);

    /* Variable for the day chosen */

    DefineUInt8(sd_id, D_FAPAR_3S, D_FAPAR_NAME, rank, ndim, dim_name, UNITLESS, out_bad_ui8, D_FAPAR_SCALE, D_FAPAR_OFFSET, D_FAPAR_MIN, D_FAPAR_MAX);

    /* Variable for deviation */

    DefineUInt8(sd_id, SD_FAPAR_3S, SD_FAPAR_NAME, rank, ndim, dim_name, UNITLESS, out_bad_ui8, SD_FAPAR_SCALE, SD_FAPAR_OFFSET, SD_FAPAR_MIN, SD_FAPAR_MAX);

    /* Variable for RECT R deviation */

    DefineInt16(sd_id, SD_RECTR_3S, SD_RECTR_NAME, rank, ndim, dim_name, UNITLESS, out_bad_i16, SD_RECTR_SCALE, SD_RECTR_OFFSET, SD_RECTR_MIN, SD_RECTR_MAX);

    /* Variable for RECT NIR deviation */

    DefineInt16(sd_id, SD_RECTNIR_3S, SD_RECTNIR_NAME, rank, ndim, dim_name, UNITLESS, out_bad_i16, SD_RECTNIR_SCALE, SD_RECTNIR_OFFSET, SD_RECTNIR_MIN, SD_RECTNIR_MAX);

    /* Variable for the number of observations */

    DefineUInt8(sd_id, NB_FAPAR_3S, NB_FAPAR_NAME, rank, ndim, dim_name, UNITLESS, out_bad_ui8, NB_FAPAR_SCALE, NB_FAPAR_OFFSET, NB_FAPAR_MIN, NB_FAPAR_MAX);

    status = SDend(sd_id);
    //if (status < 0) TSTOP("ERROR: l3time - problem for closing HDF file\n");

    /* ------------------------------------------ */
    /* Complete memory allocation for the actual number of Input files */
    
    for (f = 0; f < NbFiles; f++) {

        p_FPAR[ f] = (uint8*) allocate("p_FPAR",  nelem*NLINE_STEP, sizeof (uint8));
        p_brf_r[f] = (int16*) allocate("p_brf_r", nelem*NLINE_STEP, sizeof (int16));
        p_brf_n[f] = (int16*) allocate("p_brf_n", nelem*NLINE_STEP, sizeof (int16));
        
        if (compute_u) {
        p_FPAR_u[ f] = (int16*) allocate("p_FPAR_u",  nelem*NLINE_STEP, sizeof (int16));
        p_brf_r_u[f] = (int16*) allocate("p_brf_r_u", nelem*NLINE_STEP, sizeof (int16));
        p_brf_n_u[f] = (int16*) allocate("p_brf_n_u", nelem*NLINE_STEP, sizeof (int16));
        }

        p_sun_zenith[f]  = (int16*) allocate("p_sun_zenith",  nelem*NLINE_STEP, sizeof (int16));
        p_rel_azimuth[f] = (int16*) allocate("p_rel_azimuth", nelem*NLINE_STEP, sizeof (int16));

        p_brf_443[f] = (int16*) allocate("p_brf_443", nelem*NLINE_STEP, sizeof (int16));
        p_brf_555[f] = (int16*) allocate("p_brf_555", nelem*NLINE_STEP, sizeof (int16));
        p_brf_670[f] = (int16*) allocate("p_brf_670", nelem*NLINE_STEP, sizeof (int16));
        p_brf_865[f] = (int16*) allocate("p_brf_865", nelem*NLINE_STEP, sizeof (int16));

        /*
        if(( p_brf_412[f] = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: ProcessLand - malloc error for brf_412");
        if(( p_brf_490[f] = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: ProcessLand - malloc error for brf_490");
        if(( p_brf_510[f] = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: ProcessLand - malloc error for brf_510");
        if(( p_brf_765[f] = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: ProcessLand - malloc error for brf_765");
         */

        p_sat_zenith[f] = (int16*) allocate("p_sat_zenith", nelem*NLINE_STEP, sizeof (int16));
        p_flag[f]       = (uint8*) allocate("p_flag",       nelem*NLINE_STEP, sizeof (uint8));
    }

    /* ------------------------------------------ */
    /* Output file */

    ptr_FPAR  = (uint8*) allocate("ptr_FPAR",  nelem*NLINE_STEP, sizeof (uint8));
    ptr_brf_r = (int16*) allocate("ptr_brf_r", nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_n = (int16*) allocate("ptr_brf_n", nelem*NLINE_STEP, sizeof (int16));
    
    /* Memory allocation for the outputs */
    if (compute_u) {
    ptr_FPAR_u  = (int16*) allocate("ptr_FPAR_u",  nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_r_u = (int16*) allocate("ptr_brf_r_u", nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_n_u = (int16*) allocate("ptr_brf_n_u", nelem*NLINE_STEP, sizeof (int16));
    }
    
    
    ptr_DevFPAR    = (uint8*) allocate("ptr_DevFPAR",    nelem*NLINE_STEP, sizeof (uint8));
    ptr_DevRectR   = (int16*) allocate("ptr_DevRectR",   nelem*NLINE_STEP, sizeof (int16));
    ptr_DevRectNIR = (int16*) allocate("ptr_DevRectNIR", nelem*NLINE_STEP, sizeof (int16));
    ptr_NbFPAR     = (uint8*) allocate("ptr_NbFPAR",     nelem*NLINE_STEP, sizeof (uint8));
    ptr_FPARNb     = (uint8*) allocate("ptr_FPARNb",     nelem*NLINE_STEP, sizeof (uint8));

    ptr_brf_443 = (int16*) allocate("ptr_brf_443", nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_555 = (int16*) allocate("ptr_brf_555", nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_670 = (int16*) allocate("ptr_brf_670", nelem*NLINE_STEP, sizeof (int16));
    ptr_brf_865 = (int16*) allocate("ptr_brf_865", nelem*NLINE_STEP, sizeof (int16));

    /*
    if(( ptr_brf_412 = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: l3time - malloc error for brf_412");
    if(( ptr_brf_490 = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: l3time - malloc error for brf_490");
    if(( ptr_brf_510 = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: l3time - malloc error for brf_510");
    if(( ptr_brf_765 = (int16*) malloc(sizeof(int16)*nelem*NLINE_STEP)) == NULL) TSTOP("ERROR: l3time - malloc error for brf_765");
     */

    ptr_sat_zenith  = (int16*) allocate("ptr_sat_zenith",  nelem*NLINE_STEP, sizeof (int16));
    ptr_sun_zenith  = (int16*) allocate("ptr_sun_zenith",  nelem*NLINE_STEP, sizeof (int16));
    ptr_rel_azimuth = (int16*) allocate("ptr_rel_azimuth", nelem*NLINE_STEP, sizeof (int16));

    ptr_flag = (uint8*) allocate("ptr_flag", nelem*NLINE_STEP, sizeof (uint8));

    /* ------------------------------------------ */
    /* row processing */

    /* Memory allocation for the outputs , on a line basis */
    FPAR  = (uint8*) allocate("FPAR",  nelem, sizeof (uint8));
    brf_r = (int16*) allocate("brf_r", nelem, sizeof (int16));
    brf_n = (int16*) allocate("brf_n", nelem, sizeof (int16));

    if (compute_u) {
    FPAR_u  = (int16*) allocate("FPAR_u",  nelem, sizeof (int16));
    brf_r_u = (int16*) allocate("brf_r_u", nelem, sizeof (int16));
    brf_n_u = (int16*) allocate("brf_n_u", nelem, sizeof (int16));
    }

    DevFPAR    = (uint8*) allocate("DevFPAR",    nelem, sizeof (uint8));
    DevRectR   = (int16*) allocate("DevRectR",   nelem, sizeof (int16));
    DevRectNIR = (int16*) allocate("DevRectNIR", nelem, sizeof (int16));

    brf_412 = (int16*) allocate("brf_412", nelem, sizeof (int16));
    brf_443 = (int16*) allocate("brf_443", nelem, sizeof (int16));
    brf_490 = (int16*) allocate("brf_490", nelem, sizeof (int16));
    brf_510 = (int16*) allocate("brf_510", nelem, sizeof (int16));
    brf_555 = (int16*) allocate("brf_555", nelem, sizeof (int16));
    brf_670 = (int16*) allocate("brf_670", nelem, sizeof (int16));
    brf_765 = (int16*) allocate("brf_765", nelem, sizeof (int16));
    brf_865 = (int16*) allocate("brf_865", nelem, sizeof (int16));

    sat_zenith  = (int16*) allocate("sat_zenith",  nelem, sizeof (int16));
    sun_zenith  = (int16*) allocate("sun_zenith",  nelem, sizeof (int16));
    rel_azimuth = (int16*) allocate("rel_azimuth", nelem, sizeof (int16));

    flag     = (uint8*) allocate("flag", nelem, sizeof (uint8));

    MeanFPAR = (uint8*) allocate("MeanFPAR", nelem, sizeof (uint8));
    NbFPAR   = (uint8*) allocate("NbFPAR",   nelem, sizeof (uint8));
    FPARNb   = (uint8*) allocate("FPARNb",   nelem, sizeof (uint8));
    ValueNb  = (uint8*) allocate("ValueNb",  nelem, sizeof (uint8));

    
    /* ############################################################################################# */
    /* ############################################################################################# */
    /* ##############################################    Beginning of the loop over the chunks       */

    /* Limit for the satellite zenith angle, in integer */
    ilimit = (int16) ((HIGH_SAT_ZENITH - SAT_ZENITH_OFFSET) / SAT_ZENITH_SCALE + 0.1);

    for (ichunk = 0; ichunk < NbChunk; ichunk++) {

        line1 = StartChunkLine[ichunk];
        line2 = EndChunkLine[ichunk];
        line12 = NbLinePerChunk[ichunk];

        //if ((ichunk + 1) % 10 == 0)
            printf("    chunk %d/%d\n", ichunk + 1, NbChunk);

        /* Read the data for the line for each file */
        for (f = 0; f < NbFiles; f++) {

            sd_id = SDstart(inputfiles[f], DFACC_RDONLY);

            ReadUInt8(sd_id, line1, line2, 1, nelem, 1, 1, L3_FLAGS_3S, p_flag[f], unit, &bad_ui8, &xscale, &xoffset, &usemin_ui8, &usemax_ui8);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, SAT_ZENITH_3S, p_sat_zenith[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, SUN_ZENITH_3S, p_sun_zenith[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, REL_AZIMUTH_3S, p_rel_azimuth[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);

            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_443_3S, p_brf_443[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_555_3S, p_brf_555[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_670_3S, p_brf_670[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_865_3S, p_brf_865[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);

            /*
            Read_Field_Int16(sd_id,line1,line2,1,nelem,1,1,BRF_412_3S,p_brf_412[f],unit,&bad_i16,&xscale,&xoffset,&usemin_i16,&usemax_i16);
            Read_Field_Int16(sd_id,line1,line2,1,nelem,1,1,BRF_490_3S,p_brf_490[f],unit,&bad_i16,&xscale,&xoffset,&usemin_i16,&usemax_i16);
            Read_Field_Int16(sd_id,line1,line2,1,nelem,1,1,BRF_510_3S,p_brf_510[f],unit,&bad_i16,&xscale,&xoffset,&usemin_i16,&usemax_i16);
            Read_Field_Int16(sd_id,line1,line2,1,nelem,1,1,BRF_765_3S,p_brf_765[f],unit,&bad_i16,&xscale,&xoffset,&usemin_i16,&usemax_i16);
             */

            ReadUInt8(sd_id, line1, line2, 1, nelem, 1, 1, FAPAR_3S, p_FPAR[f], unit, &bad_ui8, &xscale, &xoffset, &usemin_ui8, &usemax_ui8);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_R_3S, p_brf_r[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_N_3S, p_brf_n[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            //printf("Chunk: %d Read RectR: %g, Rect NIR: %g, FPAR: %g\n", ichunk, p_brf_r[f], p_brf_n[f], FPAR[f]); 

            if (compute_u) {
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, FAPAR_U_3S,     p_FPAR_u[f],  unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_R_U_3S, p_brf_r_u[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            ReadInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_N_U_3S, p_brf_n_u[f], unit, &bad_i16, &xscale, &xoffset, &usemin_i16, &usemax_i16);
            }

            status = SDend(sd_id);
            //if (status < 0) TSTOP("ERROR: l3time - problem for closing HDF file\n");
        }

        /* --------------------------------------------------------------------------------------------- */
        /* --------------------------------------------------------------------------------------------- */
        /* ----------------------------------------------    Beginning of the loop over the line number  */


        for (line = line1; line <= line2; line++) {

            elem1 = selem[line - 1];
            elem2 = eelem[line - 1];

            /* --------------------------------------------------- */
            /* Loop over the files for high satellite zenith angle */
            for (f = 0; f < NbFiles; f++) {

                if (elem1 > 0) {
                    p_sat_zenith[f] += elem1;
                    p_flag[f] += elem1;
                    p_FPAR[f] += elem1;
                }

                for (elem = elem1; elem < elem2; elem++) { /* Flag value for scan edges put to 255 */

                    if ((*p_sat_zenith[f]) > ilimit) { /* should be superfluous for now */
                        (*p_flag[f]) = out_bad_ui8;
                        (*p_FPAR[f]) = out_bad_ui8;
                    }
                    p_sat_zenith[f]++;
                    p_flag[f]++;
                    p_FPAR[f]++;
                }
                p_sat_zenith[f] -= elem2;
                p_flag[f] -= elem2;
                p_FPAR[f] -= elem2;
            }

            /* --------------------------------------------------- */

            /* Compute vegetation index and related fields, mean, standard deviation,  number of samples and file number */

            /* Old calculation of selected point, only FPAR taken into account */
            /* ComputeFPAR (NbFiles,line,nelem,elem1,elem2,p_FPAR,out_bad_ui8,p_flag,FPAR,MeanFPAR,DevFPAR,NbFPAR,FPARNb); */
            /* New calculation, rectR and NIR taken into account */
            if (compute_u)
                ComputeFPARwRectUncert(NbFiles, 
                        line, nelem, elem1, elem2, 
                        p_FPAR,   p_brf_r,   p_brf_n, 
                        p_FPAR_u, p_brf_r_u, p_brf_n_u,
                        out_bad_ui8, 
                        p_flag,
                        FPAR,   brf_r,   brf_n, 
                        FPAR_u, brf_r_u, brf_n_u,
                        DevFPAR, DevRectR, DevRectNIR, 
                        NbFPAR, FPARNb);
            else
                ComputeFPARwRect(NbFiles, 
                        line, nelem, elem1, elem2, 
                        p_FPAR, p_brf_r, p_brf_n, 
                        out_bad_ui8, 
                        p_flag,
                        FPAR, brf_r, brf_n, 
                        DevFPAR, DevRectR, DevRectNIR, 
                        NbFPAR, FPARNb);

            /* Select the flag value according to the choice of FPAR and/or the time series of
               flag values over the period */

            for (elem = 0; elem < nelem; elem++) 
                ValueNb[elem] = FPARNb[elem];

            CompositeFlag(NbFiles, line, nelem, elem1, elem2, p_flag, ValueNb, FPARNb, flag, landmask);

            /* Flag correction: after CompositeFlag, the value of the flag might be different than that selected in ComputeFPAR. */
            for (elem = elem1; elem < elem2; elem++) 
            {
                if (FPARNb[elem] == out_bad_ui8) 
                {
                    FPAR[elem] = out_bad_ui8;
                    brf_r[elem] = out_bad_i16;
                    brf_n[elem] = out_bad_i16;
                    
                    if(compute_u){
                    FPAR_u[elem]  = out_bad_i16;
                    brf_r_u[elem] = out_bad_i16;
                    brf_n_u[elem] = out_bad_i16;
                    }

                    DevFPAR[elem] = out_bad_ui8;
                    NbFPAR[elem] = 0;
                    MeanFPAR[elem] = out_bad_ui8;
                    DevRectR[elem] = out_bad_i16;
                    DevRectNIR[elem] = out_bad_i16;
                }
            }

            /* Select the value for each output field using 'ValueNb', the file number chosen in ComputeFPAR() and CompositeFlag() */
            Select_BRF_TOA(NbFiles, nelem, elem1, elem2, ValueNb, out_bad_ui8,
                    p_brf_443, p_brf_555, p_brf_670, p_brf_865,
                    brf_443, brf_555, brf_670, brf_865);

            /* Select_BRF_Rec (NbFiles,nelem,elem1,elem2,FPARNb,out_bad_ui8,p_brf_r,p_brf_n,brf_r,brf_n); */


            Select_Angles(NbFiles, nelem, elem1, elem2, ValueNb, out_bad_ui8,
                    p_sat_zenith, p_sun_zenith, p_rel_azimuth,
                    sat_zenith, sun_zenith, rel_azimuth);

            /*
            Select_BRF_TOA2 (NbFiles,nelem,elem1,elem2,ValueNb,out_bad_ui8,
                             p_brf_412,p_brf_443,p_brf_490,p_brf_510,p_brf_555,p_brf_670,p_brf_765,p_brf_865,
                             brf_412,brf_443,brf_490,brf_510,brf_555,brf_670,brf_765,brf_865);
             */

            /* ------------------------------------------------------------------------------ */

            /* Transform the file number to a day number (calendar day of the month) */
            for (elem = elem1; elem < elem2; elem++) 
            {
                f = (int) (ValueNb[elem]);
                if (ValueNb[elem] != out_bad_ui8) ValueNb[elem] = indexfiles[f] + 1;
                f = (int) (FPARNb[elem]);
                if (FPARNb[elem] != out_bad_ui8) FPARNb[elem] = indexfiles[f] + 1;
            }

            for (elem = 0; elem < nelem; elem++) 
            {

                *ptr_FPAR  = FPAR[elem];  ptr_FPAR++;
                *ptr_brf_r = brf_r[elem]; ptr_brf_r++;
                *ptr_brf_n = brf_n[elem]; ptr_brf_n++;

                if(compute_u) {
                *ptr_FPAR_u  = FPAR_u[elem];  ptr_FPAR_u++;
                *ptr_brf_r_u = brf_r_u[elem]; ptr_brf_r_u++;
                *ptr_brf_n_u = brf_n_u[elem]; ptr_brf_n_u++;
                }

                *ptr_flag = flag[elem]; ptr_flag++;

                *ptr_DevFPAR    = DevFPAR[elem];    ptr_DevFPAR++;
                *ptr_DevRectR   = DevRectR[elem];   ptr_DevRectR++;
                *ptr_DevRectNIR = DevRectNIR[elem]; ptr_DevRectNIR++;
                *ptr_FPARNb     = ValueNb[elem];    ptr_FPARNb++; /* File number selected for FPAR or else */
                *ptr_NbFPAR     = NbFPAR[elem];     ptr_NbFPAR++;

                *ptr_brf_443 = brf_443[elem]; ptr_brf_443++;
                *ptr_brf_555 = brf_555[elem]; ptr_brf_555++;
                *ptr_brf_670 = brf_670[elem]; ptr_brf_670++;
                *ptr_brf_865 = brf_865[elem]; ptr_brf_865++;

                *ptr_sat_zenith  = sat_zenith[elem];  ptr_sat_zenith++;
                *ptr_sun_zenith  = sun_zenith[elem];  ptr_sun_zenith++;
                *ptr_rel_azimuth = rel_azimuth[elem]; ptr_rel_azimuth++;
            }

            /*
               for ( elem=0; elem<nelem; elem ++ ) {
             *ptr_brf_412 = brf_412[elem];   ptr_brf_412 ++;
             *ptr_brf_490 = brf_490[elem];   ptr_brf_490 ++;
             *ptr_brf_510 = brf_510[elem];   ptr_brf_510 ++;
             *ptr_brf_765 = brf_765[elem];   ptr_brf_765 ++;
               }
             */

            /*-----------------------------------------------------    End of the loop over the line number  */
            /* --------------------------------------------------------------------------------------------- */
            /* --------------------------------------------------------------------------------------------- */
        } // loop line12 times

        /* Rewind the pointers for every file */
        for (f = 0; f < NbFiles; f++) {

            p_FPAR[f]    -= nelem*line12;
            p_brf_r[f]   -= nelem*line12;
            p_brf_n[f]   -= nelem*line12;

            p_FPAR_u[f]  -= nelem*line12;
            p_brf_r_u[f] -= nelem*line12;
            p_brf_n_u[f] -= nelem*line12;

            p_brf_443[f] -= nelem*line12;
            p_brf_555[f] -= nelem*line12;
            p_brf_670[f] -= nelem*line12;
            p_brf_865[f] -= nelem*line12;

            /*  p_brf_412[f]  -= nelem*line12; p_brf_490[f]  -= nelem*line12;  p_brf_510[f]  -= nelem*line12;  p_brf_765[f]  -= nelem*line12; */

            p_sun_zenith[f]  -= nelem*line12;
            p_rel_azimuth[f] -= nelem*line12;

            p_sat_zenith[f] -= nelem*line12;
            p_flag[f]       -= nelem*line12;
        }

        ptr_FPAR  -= nelem*line12;
        ptr_brf_r -= nelem*line12;
        ptr_brf_n -= nelem*line12;

        if(compute_u) {
        ptr_FPAR_u  -= nelem*line12;
        ptr_brf_r_u -= nelem*line12;
        ptr_brf_n_u -= nelem*line12;
        }

        ptr_flag -= nelem*line12;

        /* Write outputs for land  */

        ptr_DevFPAR    -= nelem*line12;
        ptr_DevRectR   -= nelem*line12;
        ptr_DevRectNIR -= nelem*line12;
        ptr_NbFPAR     -= nelem*line12;
        ptr_FPARNb     -= nelem*line12;

        ptr_brf_443 -= nelem*line12;
        ptr_brf_555 -= nelem*line12;
        ptr_brf_670 -= nelem*line12;
        ptr_brf_865 -= nelem*line12;

        /*
             ptr_brf_412  -= nelem*line12;
             ptr_brf_490  -= nelem*line12;
             ptr_brf_510  -= nelem*line12;
             ptr_brf_765  -= nelem*line12;
         */

        ptr_sat_zenith  -= nelem*line12;
        ptr_sun_zenith  -= nelem*line12;
        ptr_rel_azimuth -= nelem*line12;
        
//    ASSERT_N(NbFiles, p_FPAR);
//    ASSERT_N(NbFiles, p_brf_r);
//    ASSERT_N(NbFiles, p_brf_n);
//    ASSERT_N(NbFiles, p_FPAR_u);
//    ASSERT_N(NbFiles, p_brf_r_u);
//    ASSERT_N(NbFiles, p_brf_n_u);
//    
//    ASSERT_1(FPAR);
//    ASSERT_1(brf_r);
//    ASSERT_1(brf_n);
//    ASSERT_1(FPAR_u);
//    ASSERT_1(brf_r_u);
//    ASSERT_1(brf_n_u);
//
//    ASSERT_1(ptr_FPAR);
//    ASSERT_1(ptr_brf_r);
//    ASSERT_1(ptr_brf_n);
//    ASSERT_1(ptr_FPAR_u);
//    ASSERT_1(ptr_brf_r_u);
//    ASSERT_1(ptr_brf_n_u);

        /* Write outputs for the chunk */
        sd_id = SDstart(outfile, DFACC_RDWR);

        WriteUInt8(sd_id, line1, line2, 1, nelem, 1, 1, FAPAR_3S, ptr_FPAR);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_R_3S, ptr_brf_r);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_N_3S, ptr_brf_n);
        
        if(compute_u) {
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, FAPAR_U_3S, ptr_FPAR_u);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_R_U_3S, ptr_brf_r_u);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_REC_N_U_3S, ptr_brf_n_u);
        }

        WriteUInt8(sd_id, line1, line2, 1, nelem, 1, 1, L3_FLAGS_3S, ptr_flag);
        WriteUInt8(sd_id, line1, line2, 1, nelem, 1, 1, SD_FAPAR_3S, ptr_DevFPAR);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, SD_RECTR_3S, ptr_DevRectR);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, SD_RECTNIR_3S, ptr_DevRectNIR);
        WriteUInt8(sd_id, line1, line2, 1, nelem, 1, 1, D_FAPAR_3S, ptr_FPARNb);
        WriteUInt8(sd_id, line1, line2, 1, nelem, 1, 1, NB_FAPAR_3S, ptr_NbFPAR);

        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, SAT_ZENITH_3S, ptr_sat_zenith);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, SUN_ZENITH_3S, ptr_sun_zenith);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, REL_AZIMUTH_3S, ptr_rel_azimuth);

        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_443_3S, ptr_brf_443);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_555_3S, ptr_brf_555);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_670_3S, ptr_brf_670);
        WriteInt16(sd_id, line1, line2, 1, nelem, 1, 1, BRF_865_3S, ptr_brf_865);

        /*  
        WriteInt16 (sd_id,line1,line2,1,nelem,1,1,BRF_412_3S,ptr_brf_412);
        WriteInt16 (sd_id,line1,line2,1,nelem,1,1,BRF_490_3S,ptr_brf_490);
        WriteInt16 (sd_id,line1,line2,1,nelem,1,1,BRF_510_3S,ptr_brf_510);
        WriteInt16 (sd_id,line1,line2,1,nelem,1,1,BRF_765_3S,ptr_brf_765);
         */

        status = SDend(sd_id);
        //if (status < 0) TSTOP("ERROR: l3time - problem for closing HDF file\n");
    }
    /*#####################################################    End of the loop over the chunk number */
    /* ############################################################################################# */
    /* ############################################################################################# */

    /* -------------------------------------------------------------------- */
    /* Free memory */

    for (f = 0; f < NbFiles; f++) 
    {
        free(p_FPAR[f]);
        free(p_brf_r[f]);
        free(p_brf_n[f]);
        
        if(compute_u){
        free(p_FPAR_u[f]);
        free(p_brf_r_u[f]);
        free(p_brf_n_u[f]);
        }

        free(p_brf_443[f]);
        free(p_brf_555[f]);
        free(p_brf_670[f]);
        free(p_brf_865[f]);

        /*
           free (p_brf_412[f]); free (p_brf_490[f]); free (p_brf_510[f]); free (p_brf_765[f]);
         */

        free(p_sun_zenith[f]);
        free(p_rel_azimuth[f]);

        free(p_sat_zenith[f]);
        free(p_flag[f]);
    }

    // output
    {
        free(ptr_FPAR);
        free(ptr_brf_r);
        free(ptr_brf_n);

        if(compute_u){
        free(ptr_FPAR_u);
        free(ptr_brf_r_u);
        free(ptr_brf_n_u);
        }

        free(ptr_flag);

        free(ptr_DevFPAR);
        free(ptr_NbFPAR);
        free(ptr_FPARNb);
        free(ptr_DevRectR);
        free(ptr_DevRectNIR);

        free(ptr_brf_443);
        free(ptr_brf_555);
        free(ptr_brf_670);
        free(ptr_brf_865);

        /*
           free (ptr_brf_412); free (ptr_brf_490); free (ptr_brf_510); free (ptr_brf_765);
         */

        free(ptr_sat_zenith);
        free(ptr_sun_zenith);
        free(ptr_rel_azimuth);
    }

    // row
    {
        free(brf_412);
        free(brf_443);
        free(brf_490);
        free(brf_510);
        free(brf_555);
        free(brf_670);
        free(brf_765);
        free(brf_865);

        free(FPAR);
        free(brf_r);
        free(brf_n);

        if(compute_u) {
        free(FPAR_u);
        free(brf_r_u);
        free(brf_n_u);
        }

        free(DevFPAR);
        free(DevRectR);
        free(DevRectNIR);

        free(MeanFPAR);
        free(FPARNb);
        free(NbFPAR);
        free(ValueNb);

        free(sat_zenith);
        free(sun_zenith);
        free(rel_azimuth);
        free(flag);

        free(selem);
        free(eelem);

        free(NbRow);
        free(NbBad);
    }
}


/* -------------------------------------------------------------------- */
/*
 ComputeFPAR : Compute FPAR and related fields, mean, mean deviation, 
               number of samples and selected file number.


 NbInput            I           number of files (==days).
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 p_Value            I           Value for each file.
 badvalue           I           bad value associated to FPAR.
 p_flag             I           flag value for each file.
 Value              O           selected value for FPAR.
 MeanValue          O           mean value of FPAR.
 DevValue           O           mean deviation to the mean.
 NbValue            O           number of values used for the analysis.
 ValueNb            O           file number selected .
 
 NB: out_bbad is a global variable = 255.    

 Implementation: F. Melin - JRC-ME, 08/2001

 */

/* --------------------------------------------------------------------*/

//void ComputeFPAR(int NbInput, int line, int nelem, int elem1, int elem2,
//        uint8 *p_Value[NbInputMax], uint8 badvalue,
//        uint8 *p_flag[NbInputMax], uint8 *Value, uint8 *MeanValue, uint8 *DevValue, uint8 *NbValue, uint8 *ValueNb) {
//    long int diff;
//    float xval;
//
//    int f;
//    int elem;
//    int num, num2;
//    int ind;
//
//    uint8 flag[NbInputMax];
//    int index[NbInputMax];
//    int index2[NbInputMax];
//
//    int iflag;
//
//    uint8 out_bad;
//
//    out_bad = badvalue;
//
//
//    if (elem1 > 0) {
//        /* ----------------------------------------------------------------- */
//        /* Part of the map skipped */
//        for (elem = 0; elem < elem1; elem++) {
//
//            /* Initialization */
//            (*Value) = out_bad_ui8;
//            (*MeanValue) = out_bad_ui8;
//            (*DevValue) = out_bad_ui8;
//            (*NbValue) = out_bad_ui8;
//            (*ValueNb) = out_bad_ui8;
//
//            /* Go forward to next element */
//            NbValue++;
//            Value++;
//            MeanValue++;
//            DevValue++;
//            ValueNb++;
//
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_Value[f] += elem1;
//            p_flag[f] += elem1;
//        }
//        /* ----------------------------------------------------------------- */
//    }
//
//    /* ----------------------------------------------------------------- */
//    /* First loop over the number of elements to compute mean index and
//       standard deviation */
//
//    for (elem = elem1; elem < elem2; elem++) {
//
//        /*
//        for (f=0; f<NbInput; f++ ) {
//        if ( (elem == 568 || elem==567) && line == 5493) 
//              printf ("elem=%d, line=%d, f=%d, vi=%d, flag=%d\n",elem,line,f,*p_Value[f],*p_flag[f]);
//        } */
//
//        /* Initialization */
//        (*Value) = out_bad_ui8;
//        (*MeanValue) = out_bad_ui8;
//        (*DevValue) = out_bad_ui8;
//        (*NbValue) = 0;
//        (*ValueNb) = out_bad_ui8;
//
//        for (f = 0; f < NbInput; f++) {
//            index[f] = -1;
//            index2[f] = -1;
//            flag[f] = *p_flag[f];
//
//            if (flag[f] < ITHRES_CLDICE && flag[f] > ITHRES_LAND) { /* flag values for valid FPAR */
//                if (flag[f] != IFLAG_NEGRECT && (*p_Value[f]) == badvalue) {
//                    printf("WARNING: ComputeFPAR - FPAR-flag inconsistent for f=%d at line=%d, elem=%d; flag=%d\n", f, line, elem, flag[f]);
//                    flag[f] = out_bad_ui8;
//                }
//            }
//        }
//        xval = 0.0;
//        num = 0;
//
//        for (f = 0; f < NbInput; f++) {
//
//            /* Test to detect the FPAR computed negative and flagged by the code */
//            /*         if ( flag[f] == iflag_land && (*p_Value[f]) == badvalue ) {
//             *p_Value[f]=0;
//             *p_flag[f]=iflag_soil; flag[f]=iflag_soil;
//                     }
//             */
//            /* FPAR values put to 0 for brightsurface */
//            /*          if ( flag[f] == iflag_brightsurface ) *p_Value[f]=0; */
//
//            if ((*p_Value[f]) != badvalue) {
//                xval = xval + (float) (*p_Value[f]); /* Sum of the values */
//                index[num] = f;
//                num++;
//            }
//        }
//        if (num != 0) {
//            xval = xval / ((float) (num));
//            (*MeanValue) = (uint8) (xval + 0.5); /* Mean value */
//        }
//
//        xval = 0.0;
//
//        /* Compute deviation from the mean */
//        if (num > 1) {
//            for (f = 0; f < num; f++) { /* Sum of the difference to the mean */
//                ind = index[f];
//                xval = xval + (float) (abs((*p_Value[ind]) - (*MeanValue)));
//            }
//            (*DevValue) = (uint8) (xval / (float) (num) + 0.5);
//        } else {
//            if (num == 1) (*DevValue) = 0;
//        }
//
//        /* At that point, mean and standard deviation have been computed, with the number of points */
//        /* ######################################################################################## */
//        /* Go forward only if there was at least one good value */
//        if (num > 0) {
//
//            (*NbValue) = num; /* Final value for NbValue */
//
//            /* Case num=1 */
//            if (num == 1) {
//                ind = index[0];
//                (*Value) = (*MeanValue);
//                (*ValueNb) = ind;
//                /* The deviation has already been set to 0 in that case */
//            } else {
//                xval = 0.0;
//                num2 = 0;
//                /* -------------------------------------------------- */
//                /* Filter for outliers , only in case there are more than 1 value */
//                if (num > 2) {
//                    for (f = 0; f < num; f++) { /* Recompute the mean index without the values 
//                                          farther than one deviation from the mean index */
//                        ind = index[f];
//                        if (abs((int) (*p_Value[ind]) - (int) (*MeanValue)) <= (int) (*DevValue) + 1) {
//                            xval = xval + (float) (*p_Value[ind]);
//                            index2[num2] = ind;
//                            num2++;
//                        }
//                    }
//                    num = num2;
//                    /* Update the index list */
//                    for (f = 0; f < num; f++) index[f] = index2[f];
//                    xval = xval / ((float) (num)); /* New mean value */
//                    (*MeanValue) = (uint8) (xval + 0.5);
//                }
//                /* -------------------------------------------------- */
//                /* Case where only 2 values remain, after filtering or without */
//                /* the MeanValue is unchanged in that case */
//                if (num == 2) {
//                    ind = index[0];
//                    (*Value) = (*p_Value[ind]);
//                    *ValueNb = ind;
//                    ind = index[1]; /* Take highest index for num=2 */
//                    if ((*Value) < (*p_Value[ind])) { /* Use the highest value of the two */
//                        (*Value) = (*p_Value[ind]);
//                        *ValueNb = ind;
//                    }
//                } else {
//                    /* When there are more than 2 values left, look for the closest to the mean value */
//                    diff = MaxValueByte; /* start value for the difference */
//
//                    for (f = 0; f < num; f++) { /* Find closest index to mean for nn != 2 */
//                        ind = index[f];
//                        if (abs((int) (*MeanValue)-(int) (*p_Value[ind])) < diff) {
//                            diff = abs((int) (*MeanValue)-(int) (*p_Value[ind]));
//                            *Value = *p_Value[ind];
//                            *ValueNb = ind;
//                        }
//                    }
//
//                }
//                /* If the resulting value is 0, look for the lowest value of the flag and update ValueNb */
//                if ((*Value) == 0) {
//                    /* Initialize the flag value by the one of the day selected so far */
//                    iflag = (int) (flag[(*ValueNb)]);
//                    for (f = 0; f < num; f++) {
//                        ind = index[f];
//                        if (iflag > (int) (flag[ind]) && *p_Value[ind] == 0) {
//                            *ValueNb = ind;
//                            iflag = (int) (flag[ind]);
//                        }
//                    }
//                }
//
//            }
//            /* End of test for num>1 */
//        } /* End of test for num>0 */
//        /* ######################################################################################## */
//
//
//        /* Go forward to next element */
//        for (f = 0; f < NbInput; f++) {
//            p_Value[f]++;
//            p_flag[f]++;
//        }
//        NbValue++;
//        Value++;
//        MeanValue++;
//        DevValue++;
//        ValueNb++;
//
//    }
//    /* End of loop over elements */
//    /* ----------------------------------------------------------------- */
//
//    if (elem2 < nelem) {
//        /* ----------------------------------------------------------------- */
//        /* Part of the map skipped */
//        for (elem = elem2; elem < nelem; elem++) {
//
//            /* Initialization */
//            (*Value) = out_bad_ui8;
//            (*MeanValue) = out_bad_ui8;
//            (*DevValue) = out_bad_ui8;
//            (*NbValue) = out_bad_ui8;
//            (*ValueNb) = out_bad_ui8;
//
//            /* Go forward to next element */
//            NbValue++;
//            Value++;
//            MeanValue++;
//            DevValue++;
//            ValueNb++;
//
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_Value[f] += nelem - elem2;
//            p_flag[f] += nelem - elem2;
//        }
//
//        /* ----------------------------------------------------------------- */
//    }
//
//    /* ----------------------------------------------------------------- */
//    /* Go back to start element of the array */
//    /* for (f=0; f<NbInput; f++) { p_Value[f] -=nelem; p_flag[f] -=nelem; } */
//
//    /* Flag is rewinded because it is used again in CompositeFlag */
//    for (f = 0; f < NbInput; f++) {
//        p_flag[f] -= nelem;
//    }
//
//    NbValue -= nelem;
//    Value -= nelem;
//    MeanValue -= nelem;
//    DevValue -= nelem;
//    ValueNb -= nelem;
//
//    /* End of routine */
//}


/* -------------------------------------------------------------------- */
/*
 ComputeFPARwRect : Compute FPAR and related fields, mean, mean deviation, 
                    number of samples and selected file number.


 NbInput            I           number of files (==days).
 line               I           line                        
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 p_Value            I           FPAR Value for each file.
 p_Value1           I           Rect Red Value for each file.
 p_Value2           I           Rect NIR Value for each file.
 badvalue           I           bad value associated to FPAR.
 p_flag             I           flag value for each file.
 Value              O           selected value for FPAR.
 Value1             O           selected value for rect red.
 Value2             O           selected value for rect ir.
 DevValue           O           deviation with respect to the selected value.
 DevValue1          O           deviation with respect to the selected value.
 DevValue2          O           deviation with respect to the selected value.
 NbValue            O           number of values used for the analysis.
 ValueNb            O           file number selected .
 
 NB: out_bbad is a global variable = 255.    

 Implementation: I. Andredakis - JRC-GEM/SOLO, 08/2010

 */
/* --------------------------------------------------------------------*/

void ComputeFPARwRect(int NbInput, 
        int line, int nelem, int elem1, int elem2,
        uint8 *p_FAPAR[NbInputMax],     /* Input FPAR */
        int16 *p_brf_r[NbInputMax],     /* Input Rect R */
        int16 *p_brf_n[NbInputMax],     /* Input Rect NIR */

        uint8 badvalue,                 /* Input bad value */
        uint8 *p_flag[NbInputMax],      /* Input flag */

        uint8 *FAPAR,                   /* Output selected FPAR */
        int16 *brf_r,                   /* Output selected rect R */
        int16 *brf_n,                   /* Output selected rect NIR */

        uint8 *dev_FAPAR,               /* Output stdev FPAR */
        int16 *dev_brf_r,               /* Output stdev rect R */
        int16 *dev_brf_n,               /* Output stdev rect NIR */

        uint8 *p_NbValue,               /* Output nb of values used */
        uint8 *p_ValueNb)               /* Output file number selected */
 {
    float xval, xval1, xval2;

    int i, f;
    int elem;
    int num, num2;
    int ind, ind1, ind2;

    uint8 flag[NbInputMax];
    int indexMDthres[NbInputMax];
    int indexFPARnonzero[NbInputMax];
    int indexrectRNIRnonzero[NbInputMax];
    int selectedIndex, secondIndex;

    /* Local vectors */
    double fpar[NbInputMax], rred[NbInputMax], rnir[NbInputMax];
    double distance[NbInputMax];
    double v_dummy[NbInputMax];

    int Npoints, MinMDindex, OriginalIndex;

    double std_fpar = 0, std_rred = 0, std_rnir = 0, dummy = 0;
    BOOL ContinueWithOutliers = FALSE;
    BOOL InvalidFpar = FALSE;
    uint8 out_bad = badvalue;
    
    
    for(i=0; i<NbInputMax; ++i)
    {
        v_dummy[i] = 0;
    }

    /* Initialization */
    if (elem1 > 0) {
        for (elem = 0; elem < elem1; elem++) {

            (*FAPAR) = out_bad_ui8;
            (*brf_r) = out_bad_i16;
            (*brf_n) = out_bad_i16;
            
            (*dev_FAPAR) = out_bad_ui8;
            (*dev_brf_r) = out_bad_i16;
            (*dev_brf_n) = out_bad_i16;
            
            (*p_NbValue) = out_bad_ui8;
            (*p_ValueNb) = out_bad_ui8;

            FAPAR++;
            brf_r++;
            brf_n++;

            dev_FAPAR++;
            dev_brf_r++;
            dev_brf_n++;

            p_NbValue++;
            p_ValueNb++;
        }
        
        for (f = 0; f < NbInput; f++) {
            p_FAPAR[f] += elem1;
            p_brf_r[f] += elem1;
            p_brf_n[f] += elem1;
            p_flag[f] += elem1;
        }

    }

    /* ----------------------------------------------------------------- */
    /* loop over the number of elements */

    for (elem = elem1; elem < elem2; elem++) {

        /* Initialization */
        (*FAPAR) = out_bad_ui8;
        (*brf_r) = out_bad_i16;
        (*brf_n) = out_bad_i16;

        (*dev_FAPAR) = out_bad_ui8;
        (*dev_brf_r) = out_bad_i16;
        (*dev_brf_n) = out_bad_i16;
        
        (*p_NbValue) = 0;
        (*p_ValueNb) = out_bad_ui8;

        /* Fill in the local vectors and Check for inconsistent flags */
        for (f = 0; f < NbInput; f++) {
            flag[f] = *p_flag[f];
            
            fpar[f] = (double) *p_FAPAR[f];
            rred[f] = (double) *p_brf_r[f];
            rnir[f] = (double) *p_brf_n[f];
            //printf("RectR: %g, Rect NIR: %g, FPAR: %g\n", rectR[f], rectNIR[f], fpar[f]); 

            if (flag[f] < ITHRES_CLDICE && flag[f] > ITHRES_LAND) { /* flag values for valid FPAR */
                /* should be changed ng ?? 			*/
                if (flag[f] != IFLAG_NEGRECT && (fpar[f]) == badvalue) {
                    /* printf ("WARNING: ComputeFPAR - FPAR-flag inconsistent for"
                            " f=%d at line=%d, elem=%d; flag=%d\n",f,line,elem,flag[f]); */
                    flag[f] = out_bad_ui8;
                }
            }
        }

        /* =============================================== */
        /*       BEGINNING OF SELECTION ALGORITHM          */
        /* =============================================== */

        /* Count how many points (i.e. days) we have with a valid, non-zero FPAR */
        int NFparNonZero = 0;
        int NrectRNIRnotBad = 0;
        for (i = 0; i < NbInput; i++) {
            if ((fpar[i] != out_bad_ui8) && (fpar[i] > 0)) {
                indexFPARnonzero[NFparNonZero] = i;
                NFparNonZero++;
            }
            if ((rred[i] != out_bad_i16) && (rnir[i] != out_bad_i16)) {
                indexrectRNIRnonzero[NrectRNIRnotBad] = i;
                NrectRNIRnotBad++;
            }
        }

        /* Cases handled :
           1. Valid FAPAR for more than 3 points:
              Find distance for each point.
              Continue (later) to reject the most distant points.
           2. Valid FPAR for only 2 points: Select as representative
              the point with the maximum FPAR of the two.
           3. Valid FPAR for only 1 point: Select this point.
           4. No valid FPAR points, more than 3 valid rectR
              and rectNIR points: Find distance, and continue
              later to reject outliers
           5. No valid FPAR points, only two valid rectR and rectNIR
              points: Select the one with the maximum rectNIR
           6. No valid FPAR: Only one with valid rectR - rectNIR:
              Select this point. */

        if (NFparNonZero > 2) 
        {
            /* 1. If we have 3 points or more with a valid non-zero FPAR, 
               Use FPAR and Rect R and NIR and find the distance
               of each day data-point: Call with 3 vectors */

            Npoints = 0;
            for (i = 0; i < NbInput; i++) {
                if ((fpar[i] != out_bad_ui8) && (fpar[i] > 0)) {
                    fpar[Npoints] = fpar[i];
                    rred[Npoints] = rred[i];
                    rnir[Npoints] = rnir[i];
                    Npoints++;
                }
            }

            FindMinEuclDistance(fpar, rred, rnir, Npoints, 3, distance, &std_fpar, &std_rred, &std_rnir, &MinMDindex);
            
            OriginalIndex = indexFPARnonzero[MinMDindex];

            ContinueWithOutliers = TRUE;
            InvalidFpar = FALSE;
        }
        else if (NFparNonZero == 2) 
        {
            selectedIndex = indexFPARnonzero[0];
            secondIndex   = indexFPARnonzero[1];

            if (fpar[selectedIndex] < fpar[secondIndex]) {
                selectedIndex = indexFPARnonzero[1];
                secondIndex   = indexFPARnonzero[0];
            }

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *dev_FAPAR = (uint8) (0.707 * fabs(fpar[selectedIndex] - fpar[secondIndex]));
            *dev_brf_r = (int16) (0.707 * fabs(rred[selectedIndex] - rred[secondIndex]));
            *dev_brf_n = (int16) (0.707 * fabs(rnir[selectedIndex] - rnir[secondIndex]));

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NFparNonZero;

            ContinueWithOutliers = FALSE;
        } 
        else if (NFparNonZero == 1) 
        {
            selectedIndex = indexFPARnonzero[0];

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) 0;
            *dev_brf_n = (int16) 0;

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NFparNonZero;

            ContinueWithOutliers = FALSE;
        }
        else if (NrectRNIRnotBad > 2) 
        {
            /* 4. NO points with a non-zero FPAR: 
                use only the Rect R and NIR (entire) and find the distance
                of each data point: Call with 2 vectors - the third is 
                a dummy, not used */

            Npoints = 0;
            for (i = 0; i < NbInput; i++) {
                if ((rred[i] != out_bad_i16) && (rnir[i] != out_bad_i16)) {
                    rred[Npoints] = rred[i];
                    rnir[Npoints] = rnir[i];
                    Npoints++;
                }
            }

            FindMinEuclDistance(rred, rnir, v_dummy, Npoints, 2, distance, &std_rred, &std_rnir, &dummy, &MinMDindex);

            OriginalIndex = indexrectRNIRnonzero[MinMDindex];

            ContinueWithOutliers = TRUE;
            InvalidFpar = TRUE;
        }
        else if (NrectRNIRnotBad == 0) 
        {
            /* Case 5, 6, or no valid points at all */
            /* If no valid points at all, just take the first one */

            *FAPAR = (uint8) out_bad_ui8;
            *brf_r = (int16) out_bad_i16;
            *brf_n = (int16) out_bad_i16;

            *dev_FAPAR = (uint8) out_bad_ui8;
            *dev_brf_r = (int16) out_bad_i16;
            *dev_brf_n = (int16) out_bad_i16;

            *p_ValueNb = (uint8) out_bad_ui8;
            *p_NbValue = (uint8) 0;

            ContinueWithOutliers = FALSE;
        } 
        else if (NrectRNIRnotBad == 2) 
        {
            /* If 1 or 2 valid points, take maximum of rectNIR */
            /* Deviations: two points and one point */

            selectedIndex = indexrectRNIRnonzero[0];
            secondIndex   = indexrectRNIRnonzero[1];

            if (rnir[selectedIndex] < rnir[secondIndex]) {
                selectedIndex = indexrectRNIRnonzero[1];
                secondIndex   = indexrectRNIRnonzero[0];
            }

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) (0.707 * fabs(rred[selectedIndex] - rred[secondIndex]));
            *dev_brf_n = (int16) (0.707 * fabs(rnir[selectedIndex] - rnir[secondIndex]));

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NrectRNIRnotBad;

            ContinueWithOutliers = FALSE;
        } 
        else // (NrectRNIRnotBad == 1) 
        {
            /* If 1 or 2 valid points, take maximum of rectNIR */
            /* Deviations: two points and one point */

            selectedIndex = indexrectRNIRnonzero[0];

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) 0;
            *dev_brf_n = (int16) 0;

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NrectRNIRnotBad;

            ContinueWithOutliers = FALSE;
        }

        /* If we've had more than 3 points (FPAR or not), reject outliers
           and rerun algorithm */
        if (ContinueWithOutliers) 
        {
            double threshold = (InvalidFpar) ? 2.30 : sq(3.53);
            
            /* In either case, count the points with a distance of more than 3.53 squared */
            int NPdistthres = 0;
            for (i = 0; i < Npoints; i++) {
                if (distance[i] < threshold) 
                    NPdistthres++;
            }

            /* If within this radius, there are less than 3 points, 
               use the one with the minimum distance found previously */
            if ((NPdistthres < 3) && (NFparNonZero > 2)) 
            {
                *FAPAR = (uint8) fpar[MinMDindex];
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];

                *dev_FAPAR = (uint8) std_fpar;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) OriginalIndex;
                *p_NbValue = (uint8) Npoints;
            } 
            else if (NPdistthres < 3)
            {
                *FAPAR = (uint8) 0;
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];

                *dev_FAPAR = (uint8) 0;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) OriginalIndex;
                *p_NbValue = (uint8) Npoints;
            }
            /* If after rejection we have more than 2 points, find
                again the distance of the new points and use the
                point with the minimum MD as final result */

            else if (NFparNonZero > 2) 
            {
                /* Use all 3 vectors, if used previously */
                /* Repeat the point count, for clarity. So sue me. */
                NPdistthres = 0;
                for (i = 0; i < Npoints; i++) {
                    if (distance[i] < threshold) {
                        /* Count the points, but also re-index the data */
                        fpar[NPdistthres] = fpar[i];
                        rred[NPdistthres] = rred[i];
                        rnir[NPdistthres] = rnir[i];
                        indexMDthres[NPdistthres] = i;
                        NPdistthres++;
                    }
                }

                FindMinEuclDistance(fpar, rred, rnir, NPdistthres, 3, distance, &std_fpar, &std_rred, &std_rnir, &MinMDindex);
                
                OriginalIndex = indexMDthres[MinMDindex];
                
                *FAPAR = (uint8) fpar[MinMDindex];
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];
                
                *dev_FAPAR = (uint8) std_fpar;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) indexFPARnonzero[OriginalIndex]; /* ORIGINAL */
                *p_NbValue = (uint8) NPdistthres;
            }                    /* Otherwise, use only two vectors. Assert 0 for FPAR and its deviation */
            else // (NFparNonZero < 3)
            {
                /* Repeat the point count, for clarity. So sue me. */
                NPdistthres = 0;
                for (i = 0; i < Npoints; i++) {
                    if (distance[i] < threshold) {
                        /* Count the points, but also re-index the data */
                        fpar[NPdistthres] = fpar[i];
                        rred[NPdistthres] = rred[i];
                        rnir[NPdistthres] = rnir[i];
                        indexMDthres[NPdistthres] = i;
                        NPdistthres++;
                    }
                }

                FindMinEuclDistance(rred, rnir, v_dummy, NPdistthres, 2, distance, &std_rred, &std_rnir, &dummy, &MinMDindex);
                
                OriginalIndex = indexMDthres[MinMDindex];
                
                *FAPAR = (uint8) 0;
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];
                
                *dev_FAPAR = (uint8) 0;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) indexrectRNIRnonzero[OriginalIndex];
                *p_NbValue = (uint8) NPdistthres;
            }
        } 
        /* End if continue with Outliers */

        //printf("\n\nSelection: NFparNonZero: %d FinalPoints: %d\n", NFparNonZero, *NbValue);
        /* =============================================== */
        /*       END OF SELECTION ALGORITHM                */
        /* =============================================== */

        /* Go forward to next element */
        for (f = 0; f < NbInput; f++) {
            p_FAPAR[f]++;
            p_brf_r[f]++;
            p_brf_n[f]++;
            p_flag[f]++;
        }

        FAPAR++;
        brf_r++;
        brf_n++;

        dev_FAPAR++;
        dev_brf_r++;
        dev_brf_n++;

        p_NbValue++;
        p_ValueNb++;

    }
    /* End of loop over elements */
    /* ----------------------------------------------------------------- */

    if (elem2 < nelem) {
        /* ----------------------------------------------------------------- */
        /* Part of the map skipped */
        for (elem = elem2; elem < nelem; elem++) {
            /* Initialization */
            (*FAPAR) = out_bad_ui8;
            (*brf_r) = out_bad_i16;
            (*brf_n) = out_bad_i16;
            
            (*dev_FAPAR) = out_bad_ui8;
            (*dev_brf_r) = out_bad_i16;
            (*dev_brf_n) = out_bad_i16;
            
            (*p_NbValue) = out_bad_ui8;
            (*p_ValueNb) = out_bad_ui8;

            /* Go forward to next element */
            p_NbValue++;
            FAPAR++;
            brf_r++;
            brf_n++;
            dev_FAPAR++;
            dev_brf_r++;
            dev_brf_n++;
            p_ValueNb++;
        }
        
        for (f = 0; f < NbInput; f++) {
            p_FAPAR[f] += nelem - elem2;
            p_brf_r[f] += nelem - elem2;
            p_brf_n[f] += nelem - elem2;
            p_flag[f] += nelem - elem2;
        }
    }

    /* ----------------------------------------------------------------- */
    /* Go back to start element of the array */
    /* for (f=0; f<NbInput; f++) { p_Value[f] -=nelem; p_flag[f] -=nelem; } */

    /* Flag is rewinded because it is used again in CompositeFlag */
    for (f = 0; f < NbInput; f++) {
        p_flag[f] -= nelem;
    }

    FAPAR -= nelem;
    brf_r -= nelem;
    brf_n -= nelem;

    dev_FAPAR -= nelem;
    dev_brf_r -= nelem;
    dev_brf_n -= nelem;

    p_NbValue -= nelem;
    p_ValueNb -= nelem;

    /* End of routine */
}



void ComputeFPARwRectUncert(int NbInput, 
        int line, int nelem, int elem1, int elem2,
        uint8 *p_FAPAR[NbInputMax],     /* Input FPAR */
        int16 *p_brf_r[NbInputMax],     /* Input Rect R */
        int16 *p_brf_n[NbInputMax],     /* Input Rect NIR */

        int16 *p_FAPAR_u[NbInputMax],   /* Input FPAR */
        int16 *p_brf_r_u[NbInputMax],   /* Input Rect R */
        int16 *p_brf_n_u[NbInputMax],   /* Input Rect NIR */

        uint8 badvalue,                 /* Input bad value */
        uint8 *p_flag[NbInputMax],      /* Input flag */

        uint8 *FAPAR,                   /* Output selected FPAR */
        int16 *brf_r,                   /* Output selected rect R */
        int16 *brf_n,                   /* Output selected rect NIR */

        int16 *FAPAR_u,                 /* Output selected FPAR */
        int16 *brf_r_u,                 /* Output selected rect R */
        int16 *brf_n_u,                 /* Output selected rect NIR */

        uint8 *dev_FAPAR,               /* Output stdev FPAR */
        int16 *dev_brf_r,               /* Output stdev rect R */
        int16 *dev_brf_n,               /* Output stdev rect NIR */

        uint8 *p_NbValue,               /* Output nb of values used */
        uint8 *p_ValueNb)               /* Output file number selected */
 {
    float xval, xval1, xval2;

    int i, f;
    int elem;
    int num, num2;
    int ind, ind1, ind2;

    uint8 flag[NbInputMax];
    int indexMDthres[NbInputMax];
    int indexFPARnonzero[NbInputMax];
    int indexrectRNIRnonzero[NbInputMax];
    int selectedIndex, secondIndex;

    /* Local vectors */
    double fpar[NbInputMax], rred[NbInputMax], rnir[NbInputMax];
    double fpar_u[NbInputMax], rred_u[NbInputMax], rnir_u[NbInputMax];
    double distance[NbInputMax];
    double v_dummy[NbInputMax];

    int Npoints, MinMDindex, OriginalIndex;

    double std_fpar = 0, std_rred = 0, std_rnir = 0, dummy = 0;
    BOOL ContinueWithOutliers = FALSE;
    BOOL InvalidFpar = FALSE;
    uint8 out_bad = badvalue;
    
    /* Initialization */
    if (elem1 > 0) {
        for (elem = 0; elem < elem1; elem++) {

            (*FAPAR) = out_bad_ui8;
            (*brf_r) = out_bad_i16;
            (*brf_n) = out_bad_i16;

            (*FAPAR_u) = out_bad_i16;
            (*brf_r_u) = out_bad_i16;
            (*brf_n_u) = out_bad_i16;
            
            (*dev_FAPAR) = out_bad_ui8;
            (*dev_brf_r) = out_bad_i16;
            (*dev_brf_n) = out_bad_i16;
            
            (*p_NbValue) = out_bad_ui8;
            (*p_ValueNb) = out_bad_ui8;

            FAPAR++;
            brf_r++;
            brf_n++;

            FAPAR_u++;
            brf_r_u++;
            brf_n_u++;

            dev_FAPAR++;
            dev_brf_r++;
            dev_brf_n++;

            p_NbValue++;
            p_ValueNb++;
        }
        
        for (f = 0; f < NbInput; f++) {
            
            p_FAPAR[f] += elem1;
            p_brf_r[f] += elem1;
            p_brf_n[f] += elem1;

            p_FAPAR_u[f] += elem1;
            p_brf_r_u[f] += elem1;
            p_brf_n_u[f] += elem1;
            
            p_flag[f]  += elem1;
        }

    }

    /* ----------------------------------------------------------------- */
    /* loop over the number of elements */

    for (elem = elem1; elem < elem2; elem++) {

        /* Initialization */
        (*FAPAR) = out_bad_ui8;
        (*brf_r) = out_bad_i16;
        (*brf_n) = out_bad_i16;

        (*FAPAR_u) = out_bad_i16;
        (*brf_r_u) = out_bad_i16;
        (*brf_n_u) = out_bad_i16;

        (*dev_FAPAR) = out_bad_ui8;
        (*dev_brf_r) = out_bad_i16;
        (*dev_brf_n) = out_bad_i16;
        
        (*p_NbValue) = 0;
        (*p_ValueNb) = out_bad_ui8;

        /* Fill in the local vectors and Check for inconsistent flags */
        for (f = 0; f < NbInput; f++) {
            flag[f] = *p_flag[f];
            
            fpar[f] = (double) *p_FAPAR[f];
            rred[f] = (double) *p_brf_r[f];
            rnir[f] = (double) *p_brf_n[f];

            fpar_u[f] = (double) *p_FAPAR_u[f];
            rred_u[f] = (double) *p_brf_r_u[f];
            rnir_u[f] = (double) *p_brf_n_u[f];
            
            //printf("RectR: %g, Rect NIR: %g, FPAR: %g\n", rectR[f], rectNIR[f], fpar[f]); 

            if (flag[f] < ITHRES_CLDICE && flag[f] > ITHRES_LAND) { /* flag values for valid FPAR */
                /* should be changed ng ?? 			*/
                if (flag[f] != IFLAG_NEGRECT && (fpar[f]) == badvalue) {
                    /* printf ("WARNING: ComputeFPAR - FPAR-flag inconsistent for"
                            " f=%d at line=%d, elem=%d; flag=%d\n",f,line,elem,flag[f]); */
                    flag[f] = out_bad_ui8;
                }
            }
        }

        /* =============================================== */
        /*       BEGINNING OF SELECTION ALGORITHM          */
        /* =============================================== */

        /* Count how many points (i.e. days) we have with a valid, non-zero FPAR */
        int NFparNonZero = 0;
        int NrectRNIRnotBad = 0;
        for (i = 0; i < NbInput; i++) {
            if ((fpar[i] != out_bad_ui8) && (fpar[i] > 0) && (flag[i] == IFLAG_LAND)) {
                indexFPARnonzero[NFparNonZero] = i;
                NFparNonZero++;
            }
            if ((rred[i] != out_bad_i16) && (rnir[i] != out_bad_i16)) {
                indexrectRNIRnonzero[NrectRNIRnotBad] = i;
                NrectRNIRnotBad++;
            }
        }
        /*
         * Note: if FPAR is valid ALSO RED and NIR are valid!!!!
         */

        /* Cases handled :
           1. Valid FAPAR for more than 3 points:
              Find distance for each point.
              Continue (later) to reject the most distant points.
           2. Valid FPAR for only 2 points: Select as representative
              the point with the maximum FPAR of the two.
           3. Valid FPAR for only 1 point: Select this point.
           4. No valid FPAR points, more than 3 valid rectR
              and rectNIR points: Find distance, and continue
              later to reject outliers
           5. No valid FPAR points, only two valid rectR and rectNIR
              points: Select the one with the maximum rectNIR
           6. No valid FPAR: Only one with valid rectR - rectNIR:
              Select this point. */

        if (NFparNonZero > 2) 
        {
            /* 1. If we have 3 points or more with a valid non-zero FPAR, 
               Use FPAR and Rect R and NIR and find the distance
               of each day data-point: Call with 3 vectors */

            Npoints = 0;
            for (i = 0; i < NbInput; i++) {
                if ((fpar[i] != out_bad_ui8) && (fpar[i] > 0)) {
                    fpar[Npoints] = fpar[i];
                    rred[Npoints] = rred[i];
                    rnir[Npoints] = rnir[i];

                    fpar_u[Npoints] = fpar_u[i];
                    rred_u[Npoints] = rred_u[i];
                    rnir_u[Npoints] = rnir_u[i];

                    Npoints++;
                }
            }

            FindMinEuclDistanceUncert(
                    fpar,   rred,   rnir, 
                    fpar_u, rred_u, rnir_u, 
                    Npoints, 3, 
                    distance, 
                    &std_fpar, &std_rred, &std_rnir, 
                    &MinMDindex);
            
            OriginalIndex = indexFPARnonzero[MinMDindex];

            ContinueWithOutliers = TRUE;
            InvalidFpar = FALSE;
        }
        else if (NFparNonZero == 2) 
        {
            selectedIndex = indexFPARnonzero[0];
            secondIndex   = indexFPARnonzero[1];

            if (fpar[selectedIndex] < fpar[secondIndex]) {
                selectedIndex = indexFPARnonzero[1];
                secondIndex   = indexFPARnonzero[0];
            }

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *FAPAR_u = (int16) fpar_u[selectedIndex];
            *brf_r_u = (int16) rred_u[selectedIndex];
            *brf_n_u = (int16) rnir_u[selectedIndex];

            *dev_FAPAR = (uint8) (0.707 * fabs(fpar[selectedIndex] - fpar[secondIndex]));
            *dev_brf_r = (int16) (0.707 * fabs(rred[selectedIndex] - rred[secondIndex]));
            *dev_brf_n = (int16) (0.707 * fabs(rnir[selectedIndex] - rnir[secondIndex]));

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NFparNonZero;

            ContinueWithOutliers = FALSE;
        } 
        else if (NFparNonZero == 1) 
        {
            selectedIndex = indexFPARnonzero[0];

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *FAPAR_u = (int16) fpar_u[selectedIndex];
            *brf_r_u = (int16) rred_u[selectedIndex];
            *brf_n_u = (int16) rnir_u[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) 0;
            *dev_brf_n = (int16) 0;

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NFparNonZero;

            ContinueWithOutliers = FALSE;
        }
        else if (NrectRNIRnotBad > 2) 
        {
            /* 4. NO points with a non-zero FPAR: 
                use only the Rect R and NIR (entire) and find the distance
                of each data point: Call with 2 vectors - the third is 
                a dummy, not used */

            Npoints = 0;
            for (i = 0; i < NbInput; i++) {
                if ((rred[i] != out_bad_i16) && (rnir[i] != out_bad_i16)) {
                    fpar[Npoints] = fpar[i];
                    rred[Npoints] = rred[i];
                    rnir[Npoints] = rnir[i];

                    fpar_u[Npoints] = fpar_u[i];
                    rred_u[Npoints] = rred_u[i];
                    rnir_u[Npoints] = rnir_u[i];
                    Npoints++;
                }
            }

            FindMinEuclDistanceUncert(
                    v_dummy, rred,   rnir, 
                    v_dummy, rred_u, rnir_u, 
                    Npoints, 2, 
                    distance, &dummy, &std_rred, &std_rnir, 
                    &MinMDindex);

            OriginalIndex = indexrectRNIRnonzero[MinMDindex];

            ContinueWithOutliers = TRUE;
            InvalidFpar = TRUE;
        }
        else if (NrectRNIRnotBad == 0) 
        {
            /* Case 5, 6, or no valid points at all */
            /* If no valid points at all, just take the first one */

            *FAPAR = (uint8) out_bad_ui8;
            *brf_r = (int16) out_bad_i16;
            *brf_n = (int16) out_bad_i16;

            *FAPAR_u = (int16) out_bad_i16;
            *brf_r_u = (int16) out_bad_i16;
            *brf_n_u = (int16) out_bad_i16;

            *dev_FAPAR = (uint8) out_bad_ui8;
            *dev_brf_r = (int16) out_bad_i16;
            *dev_brf_n = (int16) out_bad_i16;

            *p_ValueNb = (uint8) out_bad_ui8;
            *p_NbValue = (uint8) 0;

            ContinueWithOutliers = FALSE;
        } 
        else if (NrectRNIRnotBad == 2) 
        {
            /* If 1 or 2 valid points, take maximum of rectNIR */
            /* Deviations: two points and one point */

            selectedIndex = indexrectRNIRnonzero[0];
            secondIndex   = indexrectRNIRnonzero[1];

            if (rnir[selectedIndex] < rnir[secondIndex]) {
                selectedIndex = indexrectRNIRnonzero[1];
                secondIndex   = indexrectRNIRnonzero[0];
            }

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *FAPAR_u = (int16) fpar_u[selectedIndex];
            *brf_r_u = (int16) rred_u[selectedIndex];
            *brf_n_u = (int16) rnir_u[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) (0.707 * fabs(rred[selectedIndex] - rred[secondIndex]));
            *dev_brf_n = (int16) (0.707 * fabs(rnir[selectedIndex] - rnir[secondIndex]));

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NrectRNIRnotBad;

            ContinueWithOutliers = FALSE;
        } 
        else // (NrectRNIRnotBad == 1) 
        {
            /* If 1 or 2 valid points, take maximum of rectNIR */
            /* Deviations: two points and one point */

            selectedIndex = indexrectRNIRnonzero[0];

            *FAPAR = (uint8) fpar[selectedIndex];
            *brf_r = (int16) rred[selectedIndex];
            *brf_n = (int16) rnir[selectedIndex];

            *FAPAR_u = (int16) fpar_u[selectedIndex];
            *brf_r_u = (int16) rred_u[selectedIndex];
            *brf_n_u = (int16) rnir_u[selectedIndex];

            *dev_FAPAR = (uint8) 0;
            *dev_brf_r = (int16) 0;
            *dev_brf_n = (int16) 0;

            *p_ValueNb = (uint8) selectedIndex;
            *p_NbValue = (uint8) NrectRNIRnotBad;

            ContinueWithOutliers = FALSE;
        }

        /* If we've had more than 3 points (FPAR or not), reject outliers
           and rerun algorithm */
        if (ContinueWithOutliers) 
        {
            double threshold = (InvalidFpar) ? 2.30 : sq(3.53);
            
            /* In either case, count the points with a distance of more than 3.53 squared */
            int NPdistthres = 0;
            for (i = 0; i < Npoints; i++) {
                if (distance[i] < threshold) 
                    NPdistthres++;
            }

            /* If within this radius, there are less than 3 points, 
               use the one with the minimum distance found previously */
            if ((NPdistthres < 3) && (NFparNonZero > 2)) 
            {
                *FAPAR = (uint8) fpar[MinMDindex];
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];

                *FAPAR_u = (int16) fpar_u[MinMDindex];
                *brf_r_u = (int16) rred_u[MinMDindex];
                *brf_n_u = (int16) rnir_u[MinMDindex];

                *dev_FAPAR = (uint8) std_fpar;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) OriginalIndex;
                *p_NbValue = (uint8) Npoints;
            } 
            else if (NPdistthres < 3)
            {
                *FAPAR = (uint8) 0;
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];

                *FAPAR_u = (int16) fpar_u[MinMDindex];
                *brf_r_u = (int16) rred_u[MinMDindex];
                *brf_n_u = (int16) rnir_u[MinMDindex];

                *dev_FAPAR = (uint8) 0;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) OriginalIndex;
                *p_NbValue = (uint8) Npoints;
            }
            /* If after rejection we have more than 2 points, find
                again the distance of the new points and use the
                point with the minimum MD as final result */

            else if (NFparNonZero > 2) 
            {
                /* Use all 3 vectors, if used previously */
                /* Repeat the point count, for clarity. So sue me. */
                NPdistthres = 0;
                for (i = 0; i < Npoints; i++) {
                    if (distance[i] < threshold) {
                        /* Count the points, but also re-index the data */
                        fpar[NPdistthres] = fpar[i];
                        rred[NPdistthres] = rred[i];
                        rnir[NPdistthres] = rnir[i];
                        
                        fpar_u[NPdistthres] = fpar_u[i];
                        rred_u[NPdistthres] = rred_u[i];
                        rnir_u[NPdistthres] = rnir_u[i];

                        indexMDthres[NPdistthres] = i;
                        NPdistthres++;
                    }
                }

                FindMinEuclDistanceUncert(
                        fpar,   rred,   rnir, 
                        fpar_u, rred_u, rnir_u, 
                        NPdistthres, 3, 
                        distance, &std_fpar, &std_rred, &std_rnir, 
                        &MinMDindex);
                
                OriginalIndex = indexMDthres[MinMDindex];
                
                *FAPAR = (uint8) fpar[MinMDindex];
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];
                
                *FAPAR_u = (int16) fpar_u[MinMDindex];
                *brf_r_u = (int16) rred_u[MinMDindex];
                *brf_n_u = (int16) rnir_u[MinMDindex];

                *dev_FAPAR = (uint8) std_fpar;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) indexFPARnonzero[OriginalIndex]; /* ORIGINAL */
                *p_NbValue = (uint8) NPdistthres;
            }                    /* Otherwise, use only two vectors. Assert 0 for FPAR and its deviation */
            else // (NFparNonZero < 3)
            {
                /* Repeat the point count, for clarity. So sue me. */
                NPdistthres = 0;
                for (i = 0; i < Npoints; i++) {
                    if (distance[i] < threshold) {
                        /* Count the points, but also re-index the data */
                        fpar[NPdistthres] = fpar[i];
                        rred[NPdistthres] = rred[i];
                        rnir[NPdistthres] = rnir[i];

                        fpar_u[NPdistthres] = fpar_u[i];
                        rred_u[NPdistthres] = rred_u[i];
                        rnir_u[NPdistthres] = rnir_u[i];

                        indexMDthres[NPdistthres] = i;
                        NPdistthres++;
                    }
                }

                FindMinEuclDistanceUncert(
                        v_dummy, rred, rnir, 
                        v_dummy, rred_u, rnir_u, 
                        NPdistthres, 2, 
                        distance, &dummy, &std_rred, &std_rnir, 
                        &MinMDindex);
                
                OriginalIndex = indexMDthres[MinMDindex];
                
                *FAPAR = (uint8) 0;
                *brf_r = (int16) rred[MinMDindex];
                *brf_n = (int16) rnir[MinMDindex];
                
                *FAPAR_u = (int16) fpar_u[MinMDindex];
                *brf_r_u = (int16) rred_u[MinMDindex];
                *brf_n_u = (int16) rnir_u[MinMDindex];

                *dev_FAPAR = (uint8) 0;
                *dev_brf_r = (int16) std_rred;
                *dev_brf_n = (int16) std_rnir;

                *p_ValueNb = (uint8) indexrectRNIRnonzero[OriginalIndex];
                *p_NbValue = (uint8) NPdistthres;
            }
        } 
        /* End if continue with Outliers */

        //printf("\n\nSelection: NFparNonZero: %d FinalPoints: %d\n", NFparNonZero, *NbValue);
        /* =============================================== */
        /*       END OF SELECTION ALGORITHM                */
        /* =============================================== */

        /* Go forward to next element */
        for (f = 0; f < NbInput; f++) {
            p_FAPAR[f]++;
            p_brf_r[f]++;
            p_brf_n[f]++;

            p_FAPAR_u[f]++;
            p_brf_r_u[f]++;
            p_brf_n_u[f]++;

            p_flag[f]++;
        }

        FAPAR++;
        brf_r++;
        brf_n++;

        FAPAR_u++;
        brf_r_u++;
        brf_n_u++;

        dev_FAPAR++;
        dev_brf_r++;
        dev_brf_n++;

        p_NbValue++;
        p_ValueNb++;

    }
    /* End of loop over elements */
    /* ----------------------------------------------------------------- */

    if (elem2 < nelem) {
        /* ----------------------------------------------------------------- */
        /* Part of the map skipped */
        for (elem = elem2; elem < nelem; elem++) {
            /* Initialization */
            (*FAPAR) = out_bad_ui8;
            (*brf_r) = out_bad_i16;
            (*brf_n) = out_bad_i16;

            (*FAPAR_u) = out_bad_i16;
            (*brf_r_u) = out_bad_i16;
            (*brf_n_u) = out_bad_i16;
            
            (*dev_FAPAR) = out_bad_ui8;
            (*dev_brf_r) = out_bad_i16;
            (*dev_brf_n) = out_bad_i16;
            
            (*p_NbValue) = out_bad_ui8;
            (*p_ValueNb) = out_bad_ui8;

            /* Go forward to next element */
            FAPAR++;
            brf_r++;
            brf_n++;

            FAPAR_u++;
            brf_r_u++;
            brf_n_u++;
            
            dev_FAPAR++;
            dev_brf_r++;
            dev_brf_n++;

            p_NbValue++;
            p_ValueNb++;
        }
        
        for (f = 0; f < NbInput; f++) {
            p_FAPAR[f] += nelem - elem2;
            p_brf_r[f] += nelem - elem2;
            p_brf_n[f] += nelem - elem2;

            p_FAPAR_u[f] += nelem - elem2;
            p_brf_r_u[f] += nelem - elem2;
            p_brf_n_u[f] += nelem - elem2;
            
            p_flag[f]  += nelem - elem2;
        }
    }

    /* ----------------------------------------------------------------- */
    /* Go back to start element of the array */
    /* for (f=0; f<NbInput; f++) { p_Value[f] -=nelem; p_flag[f] -=nelem; } */

    /* Flag is rewinded because it is used again in CompositeFlag */
    for (f = 0; f < NbInput; f++) {
        p_flag[f] -= nelem;
    }

    FAPAR -= nelem;
    brf_r -= nelem;
    brf_n -= nelem;

    FAPAR_u -= nelem;
    brf_r_u -= nelem;
    brf_n_u -= nelem;

    dev_FAPAR -= nelem;
    dev_brf_r -= nelem;
    dev_brf_n -= nelem;

    p_NbValue -= nelem;
    p_ValueNb -= nelem;

    /* End of routine */
}



/* --------------------------------------------------------------------*/
/*
 Select_BRF_TOA        : selection of BRF_TOA according to the number 
                         of the input file selected for FPAR.

 NbInput            I           number of files (==days).
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 ValueNb            I           file number selected .
 badvalue           I           bad value associated to ValueNb.
 p_brf_443          I           BRF TOA (443nm) for each file.
 p_brf_555          I           BRF TOA (555nm) for each file.
 p_brf_670          I           BRF TOA (670nm) for each file.
 p_brf_865          I           BRF TOA (865nm) for each file.
 brf_443            O           BRF TOA (443nm) selected.
 brf_555            O           BRF TOA (555nm) selected.
 brf_670            O           BRF TOA (670nm) selected. 
 brf_865            O           BRF TOA (865nm) selected.

 F. Melin - JRC-ME, 07/2001
 */

/* --------------------------------------------------------------------*/

void Select_BRF_TOA(int NbInput, int nelem, int elem1, int elem2, uint8 *ValueNb, uint8 badvalue,
        int16 *p_brf_443[NbInputMax], int16 *p_brf_555[NbInputMax],
        int16 *p_brf_670[NbInputMax], int16 *p_brf_865[NbInputMax],
        int16 *brf_443, int16 *brf_555,
        int16 *brf_670, int16 *brf_865) {
    int f;
    int elem;
    int num;
    int16 sval;
    int status;
    int ind;

    if (elem1 > 0) {
        /* ----------------------------------------------------------------- */
        for (elem = 0; elem < elem1; elem++) {
            /* Initialization */
            brf_443[elem] = out_bad_i16;
            brf_555[elem] = out_bad_i16;
            brf_670[elem] = out_bad_i16;
            brf_865[elem] = out_bad_i16;
        }
        for (f = 0; f < NbInput; f++) {
            p_brf_443[f] += elem1;
            p_brf_555[f] += elem1;
            p_brf_670[f] += elem1;
            p_brf_865[f] += elem1;
        }
        /* ----------------------------------------------------------------- */
    }

    /* ----------------------------------------------------------------- */
    for (elem = elem1; elem < elem2; elem++) {

        if ((ValueNb[elem]) != badvalue) {
            ind = (int) (ValueNb[elem]);
            brf_443[elem] = *p_brf_443[ind];
            brf_555[elem] = *p_brf_555[ind];
            brf_670[elem] = *p_brf_670[ind];
            brf_865[elem] = *p_brf_865[ind];
        } else {

            brf_443[elem] = out_bad_i16;
            brf_555[elem] = out_bad_i16;
            brf_670[elem] = out_bad_i16;
            brf_865[elem] = out_bad_i16;

        }
        /* Go one step forward */

        for (f = 0; f < NbInput; f++) {
            p_brf_443[f]++;
            p_brf_555[f]++;
            p_brf_670[f]++;
            p_brf_865[f]++;
        }

    }

    if (elem2 < nelem) {
        /* ----------------------------------------------------------------- */
        for (elem = elem2; elem < nelem; elem++) {
            /* Initialization */
            brf_443[elem] = out_bad_i16;
            brf_555[elem] = out_bad_i16;
            brf_670[elem] = out_bad_i16;
            brf_865[elem] = out_bad_i16;

        }
        for (f = 0; f < NbInput; f++) {
            p_brf_443[f] += nelem - elem2;
            p_brf_555[f] += nelem - elem2;
            p_brf_670[f] += nelem - elem2;
            p_brf_865[f] += nelem - elem2;
        }
        /* ----------------------------------------------------------------- */
    }

}
/* End of routine */


/* --------------------------------------------------------------------*/
/*
 Select_BRF_TOA2        : selection of BRF_TOA according to the number 
                          of the input file selected for FPAR.

 NbInput            I           number of files (==days).
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 ValueNb            I           file number selected .
 badvalue           I           bad value associated to ValueNb.
 p_brf_412          I           BRF TOA (412nm) for each file.
 p_brf_443          I           BRF TOA (443nm) for each file.
 p_brf_490          I           BRF TOA (490nm) for each file.
 p_brf_510          I           BRF TOA (510nm) for each file.
 p_brf_555          I           BRF TOA (555nm) for each file.
 p_brf_670          I           BRF TOA (670nm) for each file.
 p_brf_765          I           BRF TOA (765nm) for each file.
 p_brf_865          I           BRF TOA (865nm) for each file.
 brf_412            O           BRF TOA (412nm) selected.
 brf_443            O           BRF TOA (443nm) selected.
 brf_490            O           BRF TOA (490nm) selected.
 brf_510            O           BRF TOA (510nm) selected.
 brf_555            O           BRF TOA (555nm) selected.
 brf_670            O           BRF TOA (670nm) selected. 
 brf_765            O           BRF TOA (765nm) selected.
 brf_865            O           BRF TOA (865nm) selected.

 F. Melin - JRC-ME, 07/2001
 */

/* --------------------------------------------------------------------*/

//void Select_BRF_TOA2(int NbInput, int nelem, int elem1, int elem2, uint8 *ValueNb, uint8 badvalue,
//        int16 *p_brf_412[NbInputMax], int16 *p_brf_443[NbInputMax],
//        int16 *p_brf_490[NbInputMax], int16 *p_brf_510[NbInputMax],
//        int16 *p_brf_555[NbInputMax], int16 *p_brf_670[NbInputMax],
//        int16 *p_brf_765[NbInputMax], int16 *p_brf_865[NbInputMax],
//        int16 *brf_412, int16 *brf_443, int16 *brf_490, int16 *brf_510,
//        int16 *brf_555, int16 *brf_670, int16 *brf_765, int16 *brf_865) {
//    int f;
//    int elem;
//    int num;
//    int16 sval;
//    int status;
//    int ind;
//
//    if (elem1 > 0) {
//        /* ----------------------------------------------------------------- */
//        for (elem = 0; elem < elem1; elem++) {
//            /* Initialization */
//            brf_412[elem] = out_bad_i16;
//            brf_443[elem] = out_bad_i16;
//            brf_490[elem] = out_bad_i16;
//            brf_510[elem] = out_bad_i16;
//            brf_555[elem] = out_bad_i16;
//            brf_670[elem] = out_bad_i16;
//            brf_765[elem] = out_bad_i16;
//            brf_865[elem] = out_bad_i16;
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_brf_412[f] += elem1;
//            p_brf_443[f] += elem1;
//            p_brf_490[f] += elem1;
//            p_brf_510[f] += elem1;
//            p_brf_555[f] += elem1;
//            p_brf_670[f] += elem1;
//            p_brf_765[f] += elem1;
//            p_brf_865[f] += elem1;
//        }
//        /* ----------------------------------------------------------------- */
//    }
//
//    /* ----------------------------------------------------------------- */
//    for (elem = elem1; elem < elem2; elem++) {
//
//        if ((ValueNb[elem]) != badvalue) {
//            ind = (int) (ValueNb[elem]);
//            brf_412[elem] = *p_brf_412[ind];
//            brf_443[elem] = *p_brf_443[ind];
//            brf_490[elem] = *p_brf_490[ind];
//            brf_510[elem] = *p_brf_510[ind];
//            brf_555[elem] = *p_brf_555[ind];
//            brf_670[elem] = *p_brf_670[ind];
//            brf_765[elem] = *p_brf_765[ind];
//            brf_865[elem] = *p_brf_865[ind];
//        } else {
//
//            brf_412[elem] = out_bad_i16;
//            brf_443[elem] = out_bad_i16;
//            brf_490[elem] = out_bad_i16;
//            brf_510[elem] = out_bad_i16;
//            brf_555[elem] = out_bad_i16;
//            brf_670[elem] = out_bad_i16;
//            brf_765[elem] = out_bad_i16;
//            brf_865[elem] = out_bad_i16;
//
//        }
//        /* Go one step forward */
//
//        for (f = 0; f < NbInput; f++) {
//            p_brf_412[f]++;
//            p_brf_443[f]++;
//            p_brf_490[f]++;
//            p_brf_510[f]++;
//            p_brf_555[f]++;
//            p_brf_670[f]++;
//            p_brf_765[f]++;
//            p_brf_865[f]++;
//        }
//
//    }
//
//    if (elem2 < nelem) {
//        /* ----------------------------------------------------------------- */
//        for (elem = elem2; elem < nelem; elem++) {
//            /* Initialization */
//            brf_412[elem] = out_bad_i16;
//            brf_443[elem] = out_bad_i16;
//            brf_490[elem] = out_bad_i16;
//            brf_510[elem] = out_bad_i16;
//            brf_555[elem] = out_bad_i16;
//            brf_670[elem] = out_bad_i16;
//            brf_765[elem] = out_bad_i16;
//            brf_865[elem] = out_bad_i16;
//
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_brf_412[f] += nelem - elem2;
//            p_brf_443[f] += nelem - elem2;
//            p_brf_490[f] += nelem - elem2;
//            p_brf_510[f] += nelem - elem2;
//            p_brf_555[f] += nelem - elem2;
//            p_brf_670[f] += nelem - elem2;
//            p_brf_765[f] += nelem - elem2;
//            p_brf_865[f] += nelem - elem2;
//        }
//        /* ----------------------------------------------------------------- */
//    }
//
//}
/* End of routine */

/* --------------------------------------------------------------------*/
/*
 Select_BRF_Rec        : selection of BRF_Rec according to the number 
                         of the input file selected for FPAR.

 NbInput            I           number of files (==days).
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 ValueNb            I           file number selected for the vegetation index.
 badvalue           I           bad value associated to ValueNb.
 p_flag             I           flag values for each file.
 p_brf_r            I           BRF Rectified Red for each file.
 p_brf_n            I           BRF Rectified NIR for each file. 
 brf_r              O           BRF Rectified Red selected.
 brf_n              O           BRF Rectified NIR selected. 
 
 F. Melin - JRC-ME, 07/2001
 */

/* --------------------------------------------------------------------*/

//void Select_BRF_Rec(int NbInput, int nelem, int elem1, int elem2, uint8 *ValueNb, uint8 badvalue,
//        int16 *p_brf_r[NbInputMax], int16 *p_brf_n[NbInputMax],
//        int16 *brf_r, int16 *brf_n)
// {
//
//    int f;
//    int elem;
//    int num;
//    int16 sval;
//    int status;
//
//    if (elem1 > 0) {
//        /* ----------------------------------------------------------------- */
//        for (elem = 0; elem < elem1; elem++) {
//            /* Initialization */
//            brf_r[elem] = out_bad_i16;
//            brf_n[elem] = out_bad_i16;
//
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_brf_r[f] += elem1;
//            p_brf_n[f] += elem1;
//        }
//        /* ----------------------------------------------------------------- */
//    }
//
//    /* ----------------------------------------------------------------- */
//    for (elem = elem1; elem < elem2; elem++) {
//
//        if ((ValueNb[elem]) != badvalue) {
//            brf_r[elem] = *p_brf_r[ValueNb[elem]];
//            brf_n[elem] = *p_brf_n[ValueNb[elem]];
//        } else {
//            brf_r[elem] = out_bad_i16;
//            brf_n[elem] = out_bad_i16;
//        }
//
//        /* Go one step forward */
//
//        for (f = 0; f < NbInput; f++) {
//            p_brf_r[f]++;
//            p_brf_n[f]++;
//        }
//
//    }
//
//    if (elem2 < nelem) {
//        /* ----------------------------------------------------------------- */
//        for (elem = elem2; elem < nelem; elem++) {
//            /* Initialization */
//            brf_r[elem] = out_bad_i16;
//            brf_n[elem] = out_bad_i16;
//
//        }
//        for (f = 0; f < NbInput; f++) {
//            p_brf_r[f] += nelem - elem2;
//            p_brf_n[f] += nelem - elem2;
//        }
//        /* ----------------------------------------------------------------- */
//    }
//
//    /* ----------------------------------------------------------------- */
//    /* Rewind pointers */
//    /*
//    for ( f=0; f<NbInput; f++ ) {
//       p_brf_r[f] -=nelem; p_brf_n[f] -=nelem;
//    }
//     */
//
//    /* ----------------------------------------------------------------- */
//
//}
/* End of routine */

/* --------------------------------------------------------------------*/
/*
 Select_Angles         : select the angle values according to the number 
                         of the input file selected for FPAR.

 NbInput            I           number of files (==days).
 nelem              I           number of elements per lines.
 elem1              I           start element of the line.
 elem2              I           end element of the line.
 ValueNb            I           file number selected .
 badvalue           I           bad value associated to ValueNb.
 p_sat_zenith       I           Satellite zenith angle for each file.
 p_sun_zenith       I           Sun zenith angle for each file.
 p_rel_azimuth      I           Relative azimuth angle for each file.
 sat_zenith         O           Satellite zenith angle selected.
 sun_zenith         O           Sun zenith angle selected.
 rel_azimuth        O           Relative azimuth angle selected.
 
 F. Melin - JRC-ME, 07/2001
 */

/* --------------------------------------------------------------------*/

void Select_Angles(int NbInput, int nelem, int elem1, int elem2, uint8 *ValueNb, uint8 badvalue,
        int16 *p_sat_zenith[NbInputMax], int16 *p_sun_zenith[NbInputMax], int16 *p_rel_azimuth[NbInputMax],
        int16 *sat_zenith, int16 *sun_zenith, int16 *rel_azimuth) {
    int f;

    int elem;

    if (elem1 > 0) {
        /* ----------------------------------------------------------------- */
        for (elem = 0; elem < elem1; elem++) {
            sat_zenith[elem] = out_bad_i16;
            sun_zenith[elem] = out_bad_i16;
            rel_azimuth[elem] = out_bad_i16;
        }

        for (f = 0; f < NbInput; f++) {
            p_sat_zenith[f] += elem1;
            p_sun_zenith[f] += elem1;
            p_rel_azimuth[f] += elem1;
        }
        /* ----------------------------------------------------------------- */
    }

    /* ----------------------------------------------------------------- */
    for (elem = elem1; elem < elem2; elem++) {

        /* Select the file according to nb */

        if ((ValueNb[elem]) != badvalue) {
            sat_zenith[elem] = *p_sat_zenith[ValueNb[elem]];
            sun_zenith[elem] = *p_sun_zenith[ValueNb[elem]];
            rel_azimuth[elem] = *p_rel_azimuth[ValueNb[elem]];
        } else {
            sat_zenith[elem] = out_bad_i16;
            sun_zenith[elem] = out_bad_i16;
            rel_azimuth[elem] = out_bad_i16;
        }
        for (f = 0; f < NbInput; f++) {
            p_sat_zenith[f]++;
            p_sun_zenith[f]++;
            p_rel_azimuth[f]++;
        }

    }

    if (elem2 < nelem) {
        /* ----------------------------------------------------------------- */
        for (elem = elem2; elem < nelem; elem++) {
            sat_zenith[elem] = out_bad_i16;
            sun_zenith[elem] = out_bad_i16;
            rel_azimuth[elem] = out_bad_i16;
        }

        for (f = 0; f < NbInput; f++) {
            p_sat_zenith[f] += nelem - elem2;
            p_sun_zenith[f] += nelem - elem2;
            p_rel_azimuth[f] += nelem - elem2;
        }
        /* ----------------------------------------------------------------- */
    }

    /* ----------------------------------------------------------------- */
    /*
    for ( f=0; f<NbInput; f++ ) {
       p_sat_zenith[f] -=nelem;
       p_sun_zenith[f] -=nelem;
       p_rel_azimuth[f] -=nelem;
    }
     */
    /* ----------------------------------------------------------------- */

}
/* End of routine */


/**************************************************************************************/
/*
 SUMMARY: Function that finds the point with the minimum Mahalanobis distance from 
          2 or 3 one-dimensional arrays of data (vectors). 

 METHOD: It calculates the covariance matrix of the vectors (a 3x3 or 2x2 array),
         inverts it, and then calculates the Mahalanobis distance for each data point.
         By data point we mean the triplet (or couple) of the vector values that has 
         the same index, i.e. point #1 := (a[1],b[1],c[1]). The distance is given by:
            D^2 = (x - <x>)^T (times) Covar^(-1) (times) (x - <x>)
         where 
         -  x is the vector of the three values (the "point" mentioned above), 
         -  <x> is the mean value of x (over all data points of each data vector)
         -  ^T is the transposition operator
         -  Covar^(-1) is the inverse of the covariance matrix of the 2 or 3 data vectors
         This is a product of a line vector, times a square symmetric array, times a 
         column vector and the outcome is just a numerical value.

 LIBRARY: uses the GNU Scientific Library functions. MUST install it and compile
          with -lgsl

 RETURN: Zero for success, -1 if an error happened in the inversion functions
 */

/**************************************************************************************/

//int FindMinMahDistance
//(const double a[], /* IN: 1st data vector */
//        const double b[], /* IN: 2nd data vector */
//        const double c[], /* IN: 3rd data vector (used or not) */
//        const size_t dim, /* IN: Size of the data vectors */
//        const size_t nv, /* IN: Use the first 2 or all 3 vectors (MUST be 2 or 3) */
//        double *dist, /* OUT: Vector to return the Mah distances */
//        double *stdev_a, /* OUT: Stdev of vector a w/r/t the point of min Mah distance */
//        double *stdev_b, /* OUT: Stdev of vector a w/r/t the point of min Mah distance */
//        double *stdev_c, /* OUT: Stdev of vector a w/r/t the point of min Mah distance */
//        int *indexmin) /* OUT: the index of the data point with the min Mah distance */ {
//    int i, s;
//    double a_mean, b_mean, c_mean;
//    double md;
//
//    if ((nv < (size_t) 2) || (nv > (size_t) 3)) {
//        printf("ERROR in FindMahDistance function: wrong no of vectors in input.\n");
//        return -1;
//    }
//
//    /* Find the mean values */
//    a_mean = gsl_stats_mean(a, 1, dim);
//    b_mean = gsl_stats_mean(b, 1, dim);
//    c_mean = gsl_stats_mean(c, 1, dim);
//
//    /* Find covariances */
//
//    /* Allocate the necessary gsl_matrix type arrays to do the inversion */
//    gsl_matrix *covmatrix = gsl_matrix_alloc(nv, nv);
//    gsl_matrix *invcovmat = gsl_matrix_alloc(nv, nv);
//    gsl_permutation * p = gsl_permutation_alloc(nv);
//
//    /* RAZ the matrices */
//    gsl_matrix_set_zero(covmatrix);
//    gsl_matrix_set_zero(invcovmat);
//
//    /* Fill in the covariance matrix */
//    gsl_matrix_set(covmatrix, 0, 0, gsl_stats_covariance_m(a, 1, a, 1, dim, a_mean, a_mean));
//    gsl_matrix_set(covmatrix, 1, 1, gsl_stats_covariance_m(b, 1, b, 1, dim, b_mean, b_mean));
//    gsl_matrix_set(covmatrix, 0, 1, gsl_stats_covariance_m(a, 1, b, 1, dim, a_mean, b_mean));
//    /* Symmetric element */
//    gsl_matrix_set(covmatrix, 1, 0, gsl_matrix_get(covmatrix, 0, 1));
//
//    /* If we use all 3 data vectors, fill in the rest of the covariance matrix */
//    if (nv > 2) {
//        gsl_matrix_set(covmatrix, 2, 2, gsl_stats_covariance_m(c, 1, c, 1, dim, c_mean, c_mean));
//        gsl_matrix_set(covmatrix, 0, 2, gsl_stats_covariance_m(a, 1, c, 1, dim, a_mean, c_mean));
//        gsl_matrix_set(covmatrix, 1, 2, gsl_stats_covariance_m(b, 1, c, 1, dim, b_mean, c_mean));
//        /* Rest of elements are symmetric */
//        gsl_matrix_set(covmatrix, 2, 1, gsl_matrix_get(covmatrix, 1, 2));
//        gsl_matrix_set(covmatrix, 2, 0, gsl_matrix_get(covmatrix, 0, 2));
//    }
//
//    /* Print the covariance matrix (for debugging only) */
//    /*
//    int j;
//    for (i=0; i<nv; i++)
//    {
//      for (j=0; j<nv; j++)
//      {
//        printf("%g\t", gsl_matrix_get(covmatrix, i, j));
//      }
//      printf("\n");
//    }
//     */
//
//    /* Perform the LU decomposition. If function returns non-zero, abort */
//    if (gsl_linalg_LU_decomp(covmatrix, p, &s)) {
//        printf("ERROR in FindMahDistance function: Could not LU decompose cov. matrix.\n");
//        return -1;
//    }
//
//    /* Invert */
//    if (gsl_linalg_LU_invert(covmatrix, p, invcovmat)) {
//        printf("ERROR in FindMahDistance function: Could not invert cov. matrix.\n");
//        return -1;
//    }
//
//    /* Set up and RAZ the (x-mean) vector */
//    gsl_vector *x = gsl_vector_alloc(nv);
//    gsl_vector_set_zero(x);
//    /* Set up and RAZ an intermediate vector to hold the 1st product */
//    gsl_vector *interm = gsl_vector_alloc(nv);
//    gsl_vector_set_zero(interm);
//    /* Set up and RAZ the Mah distance vector */
//    gsl_vector *MahDist = gsl_vector_alloc(dim);
//    gsl_vector_set_zero(MahDist);
//
//
//    for (i = 0; i < dim; i++) {
//        /* Fill x in */
//        gsl_vector_set(x, 0, a[i] - a_mean);
//        gsl_vector_set(x, 1, b[i] - b_mean);
//        if (nv > 2) {
//            gsl_vector_set(x, 2, c[i] - c_mean);
//        }
//
//        /* Left-multiply with inverse covariance matrix */
//        gsl_blas_dgemv(CblasNoTrans, 1, invcovmat, x, 0, interm);
//
//        /* Left-multiply product of previous operation by x */
//        gsl_blas_ddot(x, interm, &md);
//
//        /* Put result in MahDist vector */
//        gsl_vector_set(MahDist, i, md);
//    }
//
//    /* Copy the gsl local vector with the distances to the output variable */
//    memcpy(dist, MahDist->data, dim * sizeof (double));
//
//    /* Find the index of the minimum Mah Distance */
//    *indexmin = gsl_vector_min_index(MahDist);
//
//    /* Find the stdev of each vector wrt the min Mah dist element */
//    *stdev_a = gsl_stats_sd_m(a, 1, dim, a[*indexmin]);
//    *stdev_b = gsl_stats_sd_m(b, 1, dim, b[*indexmin]);
//    if (nv > 2) {
//        *stdev_c = gsl_stats_sd_m(c, 1, dim, c[*indexmin]);
//    }
//
//    /* Free allocated entities */
//    gsl_permutation_free(p);
//    gsl_matrix_free(covmatrix);
//    gsl_matrix_free(invcovmat);
//    gsl_vector_free(x);
//    gsl_vector_free(interm);
//    gsl_vector_free(MahDist);
//
//
//    return 0;
//}

/**************************************************************************************/
/*
 SUMMARY: Function that finds the point with the minimum (Euclidean) distance from 
          2 or 3 one-dimensional arrays of data (vectors). 

 METHOD: By data point we mean the triplet (or couple) of the vector values that has 
         the same index, i.e. point #1 := (a[1],b[1],c[1]). The distance is given by:
            D^2 = (x - <x>)^T (times) (x - <x>)
         where 
         -  x is the vector of the three values (the "point" mentioned above), 
         -  <x> is the mean value of x (over all data points of each data vector)
         -  ^T is the transposition operator

 LIBRARY: uses the GNU Scientific Library functions. MUST install it and compile
          with -lgsl

 RETURN: Zero for success, -1 if an error happened 
 */

/**************************************************************************************/

int FindMinEuclDistance(
        const double fpar[],
        const double rred[],
        const double rnir[],
        int n,
        int nv,
        double* dist,
        double* fpar_s_p,
        double* rred_s_p,
        double* rnir_s_p,
        int* imin_p)
{
    int i, imin;
    double d, dmin = 1e300;
    double fpar_m = mean(n, fpar);
    double rred_m = mean(n, rred);
    double rnir_m = mean(n, rnir);
    double fpar_s = stdv(n, fpar, fpar_m);
    double rred_s = stdv(n, rred, rred_m);
    double rnir_s = stdv(n, rnir, rnir_m);
    
    for (i=0; i<n; ++i)
    {
        d = ((fpar_s > 0) ? sq((fpar[i] - fpar_m)/fpar_s) : 0)
          + ((rred_s > 0) ? sq((rred[i] - rred_m)/rred_s) : 0)
          + ((rnir_s > 0) ? sq((rnir[i] - rnir_m)/rnir_s) : 0);
        
        // Note: dist[i] is the SQUARED distance!
        dist[i] = d;
        if (d < dmin)
        {
            dmin = d;
            imin = i;
        }
    }
    
    *fpar_s_p = stdv(n, fpar, fpar[imin]);
    *rred_s_p = stdv(n, rred, rred[imin]);
    *rnir_s_p = stdv(n, rnir, rnir[imin]);
    
    *imin_p = imin;
    return imin;
}


int FindMinEuclDistanceUncert(
        const double fpar[],
        const double rred[],
        const double rnir[],
        const double fpar_u[],
        const double rred_u[],
        const double rnir_u[],
        int n,
        int nv,
        double* dist,
        double* fpar_s_p,
        double* rred_s_p,
        double* rnir_s_p,
        int* imin_p)
{
    int i, imin;
    double d, dmin = 1e300;
    double fpar_m = mean(n, fpar);
    double rred_m = mean(n, rred);
    double rnir_m = mean(n, rnir);
    double fpar_s = stdv(n, fpar, fpar_m);
    double rred_s = stdv(n, rred, rred_m);
    double rnir_s = stdv(n, rnir, rnir_m);

    double fpar_u_m = mean(n, fpar_u);
    double rred_u_m = mean(n, rred_u);
    double rnir_u_m = mean(n, rnir_u);

    double fpar_w;
    double rred_w;
    double rnir_w;
    
    for (i=0; i<n; ++i)
    {
        fpar_w = (fpar_u[i] > 0) ? fpar_u_m/fpar_u[i] : 1;
        rred_w = (rred_u[i] > 0) ? rred_u_m/rred_u[i] : 1;
        rnir_w = (rnir_u[i] > 0) ? rnir_u_m/rnir_u[i] : 1;
                
        d = ((fpar_s > 0) ? sq((fpar[i] - fpar_m)*fpar_w/fpar_s) : 0)
          + ((rred_s > 0) ? sq((rred[i] - rred_m)*rred_w/rred_s) : 0)
          + ((rnir_s > 0) ? sq((rnir[i] - rnir_m)*rnir_w/rnir_s) : 0);
        
        // Note: dist[i] is the SQUARED distance!
        dist[i] = d;
        if (d < dmin)
        {
            dmin = d;
            imin = i;
        }
    }
    
    *fpar_s_p = stdv(n, fpar, fpar[imin]);
    *rred_s_p = stdv(n, rred, rred[imin]);
    *rnir_s_p = stdv(n, rnir, rnir[imin]);
    
    *imin_p = imin;
    return imin;
}



//int FindMinEuclDistance(
//    const double a[], /* IN: 1st data vector */
//    const double b[], /* IN: 2nd data vector */
//    const double c[], /* IN: 3rd data vector (used or not) */
//    int dim, /* IN: Size of the data vectors */
//    int nv, /* IN: Use the first 2 or all 3 vectors (MUST be 2 or 3) */
//    double *dist, /* OUT: Vector to return the distances */
//    double *stdev_a, /* OUT: Stdev of vector a w/r/t the point of min distance */
//    double *stdev_b, /* OUT: Stdev of vector a w/r/t the point of min distance */
//    double *stdev_c, /* OUT: Stdev of vector a w/r/t the point of min distance */
//    int *indexmin) /* OUT: the index of the data point with the min distance */ 
//{
//    int i, s;
//    double a_mean, b_mean, c_mean, stdev_a1, stdev_b1, stdev_c1;
//    double md;
//
//    if ((nv < (size_t) 2) || (nv > (size_t) 3)) 
//    {
//        printf("ERROR in FindEuclDistance function: wrong no of vectors in input.\n");
//        return -1;
//    }
//
//    /* Find the mean values */
//    a_mean = gsl_stats_mean(a, 1, dim);
//    b_mean = gsl_stats_mean(b, 1, dim);
//    c_mean = gsl_stats_mean(c, 1, dim);
//
//    /* Set up and RAZ the (x-mean) vector */
//    gsl_vector *x = gsl_vector_alloc(nv);
//    gsl_vector_set_zero(x);
//    /* Set up and RAZ the distance vector */
//    gsl_vector *d = gsl_vector_alloc(dim);
//    gsl_vector_set_zero(d);
//
//
//    /* guido ++  
//
//    /* Find the stdev of each vector  */
//    stdev_a1 = gsl_stats_sd(a, 1, dim);
//    stdev_b1 = gsl_stats_sd(b, 1, dim);
//    if (nv > 2) {
//        stdev_c1 = gsl_stats_sd(c, 1, dim);
//    }
//
//    for (i = 0; i < dim; i++) {
//        
//        /* Fill x in */
//        gsl_vector_set(x, 0, (a[i] - a_mean) / stdev_a1);
//        gsl_vector_set(x, 1, (b[i] - b_mean) / stdev_b1);
//        if (nv > 2) {
//            gsl_vector_set(x, 2, (c[i] - c_mean) / stdev_c1);
//        }
//
//        /* guido -- 
//
//            /* Multiply x by itself (square of distance) */
//        gsl_blas_ddot(x, x, &md);
//
//        /* Put result in Dist vector */
//        gsl_vector_set(d, i, md);
//    }
//
//    /* Copy the gsl local vector with the distances to the output variable */
//    memcpy(dist, d->data, dim * sizeof (double));
//
//    /* Find the index of the minimum Distance */
//    *indexmin = gsl_vector_min_index(d);
//
//    /* Find the stdev of each vector wrt the min dist element */
//    *stdev_a = gsl_stats_sd_m(a, 1, dim, a[*indexmin]);
//    *stdev_b = gsl_stats_sd_m(b, 1, dim, b[*indexmin]);
//    if (nv > 2) {
//        *stdev_c = gsl_stats_sd_m(c, 1, dim, c[*indexmin]);
//    }
//
//    /* Free allocated entities */
//    gsl_vector_free(x);
//    gsl_vector_free(d);
//
//
//    return 0;
//}

