#line 1 "randef.hh"
/* DO NOT EDIT: CREATED FROM randef.tem and randef.hh */
#ifndef API__H
#define API__H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * other declarations to be included in api.h
 * copied verbatim INTO api.h
 */

#include "pcrcalcd.h"
#include "csftypes.h"

/* make smaller to ease debugging */
#define MAX_NR_FAST_LIST 256

/* pass UINT1_T as default word size
 */
typedef unsigned int UINT1_T;

typedef enum  GET_TEST {
  GET_NO_MV_TEST, /* return FALSE if co-ordinates are outside the map,
                   * TRUE otherwise
                   */
  GET_MV_TEST    /* return FALSE if co-ordinates are outside the map
                   * or the value is a MV,
                   * TRUE otherwise
                   * this is the default behaviour at initialisation
                   */
} GET_TEST;

typedef  void (*PUT_MV_FUNC)(void **spatialValue, int r, int c);
typedef  void (*PUT_VAL_FUNC)(void **spatialValue, const void *val, int r, int c);
typedef void (*GET_FUNC)(void *v, const void ** spatialValue, int r, int c);

/* mapdim.c */
extern REAL8 Area(void);
extern REAL8 Side(void);
extern REAL8 Diagonal(void);
extern REAL8 YProjectionFactor(void);

/* bootapi.c */
/* only used and in calc::calcapi.c
 */
void BootTestApi(/* size_t nrRows, size_t nrCols, */
     double cellSize, int doesYincT2B);


#line 1 "randef.tem"
/* vim: syntax=c
 */

typedef struct MAP_UINT1 {
  /* private */
  void   *ownPtr;
  /* public: */
  PCR_ST st;
  PCR_VS vs;
  PCR_CR cr;
  /* Get has actually a BOOL return type */
  int  (*Get)( UINT1 *v, int r, int c, const struct MAP_UINT1 *m);
  void (*Put)( UINT1_T v, int r, int c, struct MAP_UINT1 *m);
  void (*PutMV)(int r, int c, struct MAP_UINT1 *m);
  void (*PutAllMV)( struct MAP_UINT1 *m);
  int  (*NrRows)(const struct MAP_UINT1 *m);
  int  (*NrCols)(const struct MAP_UINT1 *m);
  REAL8(*CellLength)(const struct MAP_UINT1 *m);

  /* private to libcalc.a */
  /* Copy is not correct implemented! 
   * void (*Copy)(const struct MAP_UINT1 *src, struct MAP_UINT1 *m);
   */
  void (*SetGetTest)(GET_TEST t, const struct MAP_UINT1 *m);
  GET_TEST  (*GetGetTest)( const struct MAP_UINT1 *m);
  int   (*HintNrFastList)(const struct MAP_UINT1 *m);
  /* testapi only
     CSF_VS (*GetValueScale)(const struct MAP_UINT1 *m);
     CSF_CR (*GetInCellRepr)(const struct MAP_UINT1 *m);
   */

  /* private to libapi.a */
  int maxVal;
  GET_TEST  getType;
  BOOL      spatial;         /* does mapCont contains a spatial or nonSpatial.*/
  UINT1 nonSpatialValue; /* if not spatial */
  void  **spatialValue;      /* if spatial */
  int nrRows, nrCols;
  BOOL (*getMVtest)( UINT1 *v, int r, int c, const struct MAP_UINT1 *m);
  BOOL (*getNOtest)( UINT1 *v, int r, int c, const struct MAP_UINT1 *m);
  GET_FUNC     getPrivate;
  PUT_VAL_FUNC putPrivate;
  PUT_MV_FUNC  putMVPrivate;
  CSF_VS       valueScale; /* testapi only */
  CSF_CR       inCellRepr; /* testapi only */
} MAP_UINT1;

/* delete map object AND map contents: */
extern void DeleteMAP_UINT1(MAP_UINT1 *m);

/* delete map object BUT NOT THE map contents: */
extern void DeleteInternalMAP_UINT1(MAP_UINT1 *m);

extern MAP_UINT1 *InitMapUINT1( size_t nrRows,size_t nrCols,void *v,BOOL spatial, CSF_CR inCr);

extern MAP_UINT1 *ReadUINT1(const char *nameOrValue,size_t nrRows, size_t nrCols);
extern MAP_UINT1 *ReadUINT1Map(const char *name);
extern void WriteUINT1Map(const char *name, MAP_UINT1 *map, CSF_VS valueScale);
extern MAP_UINT1 *CreateSpatialUINT1(CSF_CR inCr,size_t nrRows,size_t nrCols);
#line 1 "randef.tem"
/* vim: syntax=c
 */

typedef struct MAP_INT4 {
  /* private */
  void   *ownPtr;
  /* public: */
  PCR_ST st;
  PCR_VS vs;
  PCR_CR cr;
  /* Get has actually a BOOL return type */
  int  (*Get)( INT4 *v, int r, int c, const struct MAP_INT4 *m);
  void (*Put)( INT4 v, int r, int c, struct MAP_INT4 *m);
  void (*PutMV)(int r, int c, struct MAP_INT4 *m);
  void (*PutAllMV)( struct MAP_INT4 *m);
  int  (*NrRows)(const struct MAP_INT4 *m);
  int  (*NrCols)(const struct MAP_INT4 *m);
  REAL8(*CellLength)(const struct MAP_INT4 *m);

  /* private to libcalc.a */
  /* Copy is not correct implemented! 
   * void (*Copy)(const struct MAP_INT4 *src, struct MAP_INT4 *m);
   */
  void (*SetGetTest)(GET_TEST t, const struct MAP_INT4 *m);
  GET_TEST  (*GetGetTest)( const struct MAP_INT4 *m);
  int   (*HintNrFastList)(const struct MAP_INT4 *m);
  /* testapi only
     CSF_VS (*GetValueScale)(const struct MAP_INT4 *m);
     CSF_CR (*GetInCellRepr)(const struct MAP_INT4 *m);
   */

  /* private to libapi.a */
  int maxVal;
  GET_TEST  getType;
  BOOL      spatial;         /* does mapCont contains a spatial or nonSpatial.*/
  INT4 nonSpatialValue; /* if not spatial */
  void  **spatialValue;      /* if spatial */
  int nrRows, nrCols;
  BOOL (*getMVtest)( INT4 *v, int r, int c, const struct MAP_INT4 *m);
  BOOL (*getNOtest)( INT4 *v, int r, int c, const struct MAP_INT4 *m);
  GET_FUNC     getPrivate;
  PUT_VAL_FUNC putPrivate;
  PUT_MV_FUNC  putMVPrivate;
  CSF_VS       valueScale; /* testapi only */
  CSF_CR       inCellRepr; /* testapi only */
} MAP_INT4;

/* delete map object AND map contents: */
extern void DeleteMAP_INT4(MAP_INT4 *m);

/* delete map object BUT NOT THE map contents: */
extern void DeleteInternalMAP_INT4(MAP_INT4 *m);

extern MAP_INT4 *InitMapINT4( size_t nrRows,size_t nrCols,void *v,BOOL spatial, CSF_CR inCr);

extern MAP_INT4 *ReadINT4(const char *nameOrValue,size_t nrRows, size_t nrCols);
extern MAP_INT4 *ReadINT4Map(const char *name);
extern void WriteINT4Map(const char *name, MAP_INT4 *map, CSF_VS valueScale);
extern MAP_INT4 *CreateSpatialINT4(CSF_CR inCr,size_t nrRows,size_t nrCols);
#line 1 "randef.tem"
/* vim: syntax=c
 */

typedef struct MAP_REAL8 {
  /* private */
  void   *ownPtr;
  /* public: */
  PCR_ST st;
  PCR_VS vs;
  PCR_CR cr;
  /* Get has actually a BOOL return type */
  int  (*Get)( REAL8 *v, int r, int c, const struct MAP_REAL8 *m);
  void (*Put)( REAL8 v, int r, int c, struct MAP_REAL8 *m);
  void (*PutMV)(int r, int c, struct MAP_REAL8 *m);
  void (*PutAllMV)( struct MAP_REAL8 *m);
  int  (*NrRows)(const struct MAP_REAL8 *m);
  int  (*NrCols)(const struct MAP_REAL8 *m);
  REAL8(*CellLength)(const struct MAP_REAL8 *m);

  /* private to libcalc.a */
  /* Copy is not correct implemented! 
   * void (*Copy)(const struct MAP_REAL8 *src, struct MAP_REAL8 *m);
   */
  void (*SetGetTest)(GET_TEST t, const struct MAP_REAL8 *m);
  GET_TEST  (*GetGetTest)( const struct MAP_REAL8 *m);
  int   (*HintNrFastList)(const struct MAP_REAL8 *m);
  /* testapi only
     CSF_VS (*GetValueScale)(const struct MAP_REAL8 *m);
     CSF_CR (*GetInCellRepr)(const struct MAP_REAL8 *m);
   */

  /* private to libapi.a */
  int maxVal;
  GET_TEST  getType;
  BOOL      spatial;         /* does mapCont contains a spatial or nonSpatial.*/
  REAL8 nonSpatialValue; /* if not spatial */
  void  **spatialValue;      /* if spatial */
  int nrRows, nrCols;
  BOOL (*getMVtest)( REAL8 *v, int r, int c, const struct MAP_REAL8 *m);
  BOOL (*getNOtest)( REAL8 *v, int r, int c, const struct MAP_REAL8 *m);
  GET_FUNC     getPrivate;
  PUT_VAL_FUNC putPrivate;
  PUT_MV_FUNC  putMVPrivate;
  CSF_VS       valueScale; /* testapi only */
  CSF_CR       inCellRepr; /* testapi only */
} MAP_REAL8;

/* delete map object AND map contents: */
extern void DeleteMAP_REAL8(MAP_REAL8 *m);

/* delete map object BUT NOT THE map contents: */
extern void DeleteInternalMAP_REAL8(MAP_REAL8 *m);

extern MAP_REAL8 *InitMapREAL8( size_t nrRows,size_t nrCols,void *v,BOOL spatial, CSF_CR inCr);

extern MAP_REAL8 *ReadREAL8(const char *nameOrValue,size_t nrRows, size_t nrCols);
extern MAP_REAL8 *ReadREAL8Map(const char *name);
extern void WriteREAL8Map(const char *name, MAP_REAL8 *map, CSF_VS valueScale);
extern MAP_REAL8 *CreateSpatialREAL8(CSF_CR inCr,size_t nrRows,size_t nrCols);
#ifdef __cplusplus
}
#endif
#endif /* API__H */
