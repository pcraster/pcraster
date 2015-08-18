#ifndef  __POINT_H__ 
#define  __POINT_H__ 

#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "geometry.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/
typedef struct RASTER{
	unsigned char *field;	/* field with bits to be set */
	BOOL covered;		/* coverage satisfied percentage */
	size_t nrCoverCells;	/* number of cells to be 1 for nonMV */
	size_t count;		/* number of cells equal to 1 */
	size_t rasterSize;		/* size of raster */
}RASTER;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
/* point.c */
extern double DetNrCoverCells(const RASTER *raster);
extern RASTER *InitRaster(RASTER *raster);
extern RASTER *NewRaster(size_t nrCoverCells, size_t rasterSize);
extern void FreeRaster(RASTER *raster);
extern PTYPE MinPoint(PTYPE tl, PTYPE tr, PTYPE br, PTYPE bl);
extern PTYPE MaxPoint(PTYPE tl, PTYPE tr, PTYPE br, PTYPE bl);
extern POINT2D *PutInPol(
	PTYPE tlX,
	PTYPE tlY,
	PTYPE trX,
	PTYPE trY,
	PTYPE brX,
	PTYPE brY,
	PTYPE blX,
	PTYPE blY);

extern double CalcArea(
	const POINT2D *inputCell,
	const POINT2D *outputCell,
	BOOL aligned);

extern void ModRaster(
	RASTER *raster,
	const POINT2D *outputcell,
	const POINT2D *inputcell,
	REAL8 angle);

#endif
