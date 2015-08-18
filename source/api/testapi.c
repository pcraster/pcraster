#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <math.h>  /* sqrt() */

/* global header (opt.) and testapi's prototypes "" */
#include "misc.h" 
#include "csf.h"  /* map i/o */
#include "api.h" 
#include "api_p.h"

#include "app.h"

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
static const MAP *maps[200];
static int nrMaps = 0;

/******************/
/* IMPLEMENTATION */
/******************/

static CSF_CR DetermineCellRepr(
	CSF_VS valueScale)
{
	switch(valueScale) {
	 case  VS_LDD       : 
	 case  VS_BOOLEAN   : return CR_UINT1;
	 case  VS_NOMINAL   :
	 case  VS_ORDINAL   : return CR_INT4;
	 case  VS_SCALAR    :
	 case  VS_DIRECTION : return CR_REAL4;
	 default            : return CR_UINT2; /* ERROR */
       }
}

static MAP *NewMap(
	const char *fileName,
	CSF_CR   cellRepr,
	CSF_VS   valueScale)
{
	MAP *m; const MAP *c;
	if (nrMaps == 0)
	{
		Error(" (on creating %s) Can't create "
		"without input maps opened\n",fileName);
		exit(1);
	}
	c = maps[0];
	m = Rdup(fileName, c, cellRepr, valueScale);	
	if (m == NULL)
		MperrorExit(fileName, 1);
	return(m);
}

	
static void Register(
	const MAP *m)
{
	if (nrMaps == 0) {
		CSF_RASTER_LOCATION_ATTRIBUTES a;
		RgetLocationAttributes(&a,m);
		BootTestApi(/* a.nrRows,a.nrCols, */a.cellSize,a.projection == PT_YINCT2B);
	}
	else
		if ( (!Rcompare(maps[0], m)) )
		  Error("Map '%s' not equal to previous maps read\n", 
		    MgetFileName(m));
	maps[nrMaps++] = m;
}

static void *ReadMapContents(
	MAP *m,
	CSF_CR outCr,size_t nrRows, size_t nrCols)
{
	void *buf = Rmalloc(m, nrRows * nrCols);
	if (buf == NULL)
		return NULL;
        RgetSomeCells(m, (size_t)0, nrRows*nrCols, buf);
	buf = ChkRealloc(buf, 
	 (size_t)(nrRows * nrCols * CELLSIZE(outCr)));
	POSTCOND(buf != NULL); /* realloc always smalller */
	return buf;
}

/* TEMPLATE MANAGED */
#include "testapi.inc"
