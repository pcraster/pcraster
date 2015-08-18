#ifndef  PCRCALCD__H
# define PCRCALCD__H
/*
 * code to load a function dynamic
 */

#include "csftypes.h"

#ifndef  INCLUDED_PCRDLL
#include "pcrdll.h"
#define  INCLUDED_PCRDLL
#endif

#ifdef USE_IN_PCRCALC
/* USE_IN_PCRCALC is defined
 * only in the pcrcalc sources
 */

#ifndef INCLUDED_PCRDATATYPE
#include "pcrdatatype.h"
#define INCLUDED_PCRDATATYPE
#endif

typedef CSF_CR  PCR_CR;
typedef OP_ARGS PCR_EXTERNAL_ARGUMENT;


#else

 typedef enum PCR_VS { /* datatype aka valuescale */
  PCR_VS_B=1,       /* boolean */
  PCR_VS_N=2,       /* nominal */
  PCR_VS_O=4,       /* ordinal */
  PCR_VS_S=8,       /* scalar */
  PCR_VS_D=16,      /* direction */
  PCR_VS_L=32       /* ldd */
 } PCR_VS;
 typedef enum PCR_ST { /* symboltype, subset of what is defined
                        * with in pcrcalc::ST 
                        */
  PCR_ST_SPATIAL=1,     /* spatial      */
  PCR_ST_NONSPATIAL=2,  /* non-spatial  */
  PCR_ST_BOTH=3         /* both allowed */
 } PCR_ST;
 typedef enum PCR_CR { /* cell representation */
   PCR_CR_UINT1 = CR_UINT1,
   PCR_CR_INT4  = CR_INT4,
   PCR_CR_REAL8 = CR_REAL8
 } PCR_CR;

 typedef struct PCR_EXTERNAL_ARGUMENT {
  PCR_VS vs;
  PCR_ST st;
 } PCR_EXTERNAL_ARGUMENT;
#endif

/* total number of external functions
 * per library that can be loaded
 */
#define PCR_NR_EXT_FUNCTIONS 64


typedef int (*PCR_EXTERNAL_FUNCTION_RESULT_TYPES)
    (PCR_VS *resVs,PCR_ST *resSt, size_t nrArgs, 
    const PCR_EXTERNAL_ARGUMENT *argVsStPairs);
typedef int (*PCR_EXTERNAL_FUNCTION_ALGORITHM)
    (void **result, const void **args,int nrArgs);

typedef struct PCR_EXTERNAL_FUNCTION_SYNOPSIS {
  char                               name[128];
  int                                nrResults;
  int                                nrArgs;
  PCR_EXTERNAL_FUNCTION_RESULT_TYPES resultTypes;
  PCR_EXTERNAL_FUNCTION_ALGORITHM    algorithm;
  PCR_EXTERNAL_ARGUMENT              argTypes[32];
} PCR_EXTERNAL_FUNCTION_SYNOPSIS;

typedef struct PCR_EXTERNAL_FUNCTION_LIST {
  int apiVersionNr;
  int nrFunctions;
  PCR_EXTERNAL_FUNCTION_SYNOPSIS 
    functionSynopsisList[PCR_NR_EXT_FUNCTIONS];
} PCR_EXTERNAL_FUNCTION_LIST; 

typedef struct PCR_MAP_UINT1 {
        void   *reserved1;
        /* public: */
        PCR_ST st;
        int reserved2;
        PCR_CR cr;
  int  (*Get)( UINT1 *v, int r, int c, const struct PCR_MAP_UINT1 *m);
  void (*Put)(unsigned int v, int r, int c, struct PCR_MAP_UINT1 *m);
  void (*PutMV)(int r, int c, struct PCR_MAP_UINT1 *m);
  void (*PutAllMV)( struct PCR_MAP_UINT1 *m);
  int  (*NrRows)(const struct PCR_MAP_UINT1 *m);
  int  (*NrCols)(const struct PCR_MAP_UINT1 *m);
  REAL8(*CellLength)(const struct PCR_MAP_UINT1 *m);
} PCR_MAP_UINT1;

typedef struct PCR_MAP_INT4 {
        void   *reserved1;
        /* public: */
        PCR_ST st;
        int reserved2;
        PCR_CR cr;
  int  (*Get)( INT4 *v, int r, int c, const struct PCR_MAP_INT4 *m);
  void (*Put)( INT4 v, int r, int c, struct PCR_MAP_INT4 *m);
  void (*PutMV)(int r, int c, struct PCR_MAP_INT4 *m);
  void (*PutAllMV)( struct PCR_MAP_INT4 *m);
  int  (*NrRows)(const struct PCR_MAP_INT4 *m);
  int  (*NrCols)(const struct PCR_MAP_INT4 *m);
  REAL8(*CellLength)(const struct PCR_MAP_INT4 *m);
} PCR_MAP_INT4;

typedef struct PCR_MAP_REAL8 {
        void   *reserved1;
        /* public: */
        PCR_ST st;
        int reserved2;
        PCR_CR cr;
  int  (*Get)( REAL8 *v, int r, int c, const struct PCR_MAP_REAL8 *m);
  void (*Put)( REAL8 v, int r, int c, struct PCR_MAP_REAL8 *m);
  void (*PutMV)(int r, int c, struct PCR_MAP_REAL8 *m);
  void (*PutAllMV)( struct PCR_MAP_REAL8 *m);
  int  (*NrRows)(const struct PCR_MAP_REAL8 *m);
  int  (*NrCols)(const struct PCR_MAP_REAL8 *m);
  REAL8(*CellLength)(const struct PCR_MAP_REAL8 *m);
} PCR_MAP_REAL8;

#endif
