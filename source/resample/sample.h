#ifndef  __SAMPLE_H__ 
#define  __SAMPLE_H__ 

#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "csf.h"
#include "misc.h"
#include "geometry.h"
#include <math.h>

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

typedef struct ROW_CACHE{
  	int rowNr;
  	int lastCount;
  	void *rowField;
}ROW_CACHE;

typedef struct CACHE{
  	size_t nrRows;
  	ROW_CACHE *cache;
}CACHE;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/


/* sample.c */
extern void FreeCache(size_t nrMaps);
extern CACHE *InitCache(const MAP *out, MAP **in, size_t nrMaps);
extern void *CacheGetRow(MAP **in, size_t mapNr, double rowNr);
extern int SampleClass(
	MAP *out,
	MAP **in,
	double percentage,
	size_t nrMaps,
	size_t nrRows,
	size_t nrCols,
	BOOL alligned,
	REAL8 angle);

extern int SampleCont(
	MAP *out,
	MAP **in,
	double percentage,
	size_t nrMaps,
	size_t nrRows,
	size_t nrCols,
	BOOL alligned,
	REAL8 angle);

#endif
