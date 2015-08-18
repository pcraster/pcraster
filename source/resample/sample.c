#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "app.h"	

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "sample.h"	

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

static int counter;
static CACHE *cache;

/******************/
/* IMPLEMENTATION */
/******************/

/* Deletes and frees the cache for each input map.
 */
void FreeCache(size_t nrMaps)	/* number of input maps */
{
	size_t i, r;
	for(i = 0; i < nrMaps; i++)
	{
		for(r = 0; r < cache[i].nrRows; r++)
			Free(cache[i].cache[r].rowField);
		Free(cache[i].cache);
	}
	Free(cache);
}

/* Initializes the cache for each input map.
 * Returns NULL in case of an error, pointer to CACHE otherwise.
 */
CACHE *InitCache(
	const MAP *out,		/* output maps */
	MAP **in,		/* read-only list of input maps */
	size_t nrMaps)		/* number of input maps */
{
	size_t i;
	UINT2 valueScale = RgetValueScale(in[0]);

	counter = 0;
	cache = (CACHE*)(ChkMalloc(sizeof(CACHE) * nrMaps));
	if(cache == NULL)
		return NULL;
	for(i = 0; i < nrMaps; i++)
	{
		UINT4 nrCols = RgetNrCols(in[i]);
		REAL8 cellSizeIn = RgetCellSize(in[i]);
		REAL8 cellSizeOut = RgetCellSize(out);
		int r, n = ceil(cellSizeOut / cellSizeIn) + 1;	
		cache[i].nrRows = n;

		AppProgress("nr. of rows in cache %d for input map %d\n",
				n, i);

		if(((cache[i].cache = 
		   (ROW_CACHE*)(ChkMalloc(sizeof(ROW_CACHE) * n)))) == NULL)
			return NULL;

		/* initialize each row in cache */
		for(r = 0; r < n; r++)
		{
			if(AppIsClassified((CSF_VS)(valueScale)) &&
			((cache[i].cache[r].rowField = 
			 ChkMalloc(sizeof(INT4) * nrCols))) == NULL)
				return NULL;
			if(!AppIsClassified((CSF_VS)(valueScale)) &&
			((cache[i].cache[r].rowField = 
			 ChkMalloc(sizeof(REAL8) * nrCols))) == NULL)
				return NULL;
			cache[i].cache[r].lastCount = counter;

			/* initialize row nr with illegal row nr. */
			cache[i].cache[r].rowNr = -1;
		}
	}
	return cache;
}

/* Gets cell from cache of given map
 * Returns NULL in case of an error, row otherwise.
 */
void *CacheGetRow(
	MAP **in,		/* read-only list of input maps */
	size_t mapNr,		/* map from which cell is wanted */
	double rowInd)		/* row of map that is wanted */
{
	size_t r;
	int lowestCount, lru = -1;
	size_t rowNr = (size_t)floor(rowInd);

	counter++;

	for(r = 0; r < cache[mapNr].nrRows; r++)
	{
		if(cache[mapNr].cache[r].rowNr == (int)rowNr)
		{
			/* set lastCount to counter for LRU */
			cache[mapNr].cache[r].lastCount = counter;
			return cache[mapNr].cache[r].rowField;
		}
	}

	/* row not in cache -> delete least recently used. */
	lowestCount = cache[0].cache[0].lastCount;
	for(r = 0; r < cache[mapNr].nrRows; r++)
	{
		if(cache[mapNr].cache[r].lastCount <= lowestCount)
		{
			lru = r;
			lowestCount = cache[mapNr].cache[r].lastCount;
		}
	}
	if(lru == -1)
		lru = 0;
	cache[mapNr].cache[lru].lastCount = counter;
	cache[mapNr].cache[lru].rowNr = rowNr;

	PRECOND(cache[mapNr].cache[lru].rowField != NULL);
	RgetRow(in[mapNr],  rowNr, cache[mapNr].cache[lru].rowField);
	return cache[mapNr].cache[lru].rowField;
}
