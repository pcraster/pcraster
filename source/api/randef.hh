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


