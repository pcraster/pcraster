#include "stddefx.h" 

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h" /* Ran() */
#include "misc.h"
#include "calc.h"
#include "app.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/
#ifdef DEBUG
 extern BOOL  repairLddModifiedMap;
#endif

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

#define MAKE_TEMP_CODE(validCode)    ((UINT1)((validCode)|16))
#define IS_TEMP_CODE(tempCode)       ((tempCode)&16)
#define MAKE_VALID_CODE(tempCode)    ((UINT1)((tempCode)&(~16)))
/* IS_VALID_CODE: strip invalid bit and test against 0
 */
#define IS_VALID_CODE(code)          ((!IS_TEMP_CODE(code)) && code)

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* Scales the difference in elevation with the diagonal if necessary.
 * This is necessary in case of a corner neighbor. (If the neighbor
 * is not in the same row or column as the current cell, the neighbor
 * is a corner neighbor.)
 * Returns the scaled difference in elevation. 
 */
static REAL8 Scale(int ldddir)		/* ldd direction */
{
	return (Corner(ldddir) == FALSE) SCALE;
} 		  

/* Finds lowest neighbor from current cell.
 * The parameter height is the maximum value for level neighbor.
 * Returns list of one cell with lowest neighbor or cell itself.
 */
 static void Lowest(
	int *rTo,               /* write-only flows to this one */
	int *cTo,               /* write-only flows to this one */
	const MAP_REAL8 *dem,	/* read-only dem map */
 	int rowNr,		/* row of cell to be checked*/ 	
 	int colNr,		/* column of cell */
	REAL8 height)		/* height if (rowNr, colNr) */
 {
	int 	nrBestDirs=USED_UNINIT_ZERO, i, rNext, cNext;
	BOOL    aNBisMV=FALSE; /* a neighbour is missing value */
	UINT1   bestDirs[NR_LDD_DIR]; /* array of bestdrops */
	REAL8 	bestDrop = -1;	/* scaled vertical distance between
				 * current cell and lowest neighbor.
				 * start negative if init always >= 0
				 */
     /* WAAL_CW
      *	BOOL  print = (rowNr == 11 && colNr == 87) && FALSE;
      */
     	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);

	FOR_ALL_LDD_NBS(i)
	{
		REAL8	demVal;

		rNext = RNeighbor(rowNr, i);
		cNext = CNeighbor(colNr, i);

		if (dem->Get(&demVal, rNext, cNext, dem))
		{
		 if ( demVal <= height) /* possible flow or flat */
		 {
			REAL8 thisDrop = (height-demVal)/Scale(i);
		  /* WAAL_CW
		   *	if (print)
		   *	{
		   *		printf("X drop %d %d %g\n",rNext,cNext,thisDrop);
		   *		printf("X comp %g  > %g = %d\n",thisDrop, bestDrop, thisDrop>bestDrop);
		   *		printf("X comp %g  < %g = %d\n",thisDrop, bestDrop, thisDrop<bestDrop);
		   *		printf("X comp %g  = %g = %d\n",thisDrop, bestDrop, thisDrop==bestDrop);
		   *	}
		   */
			if (AppCastREAL4(thisDrop) > AppCastREAL4(bestDrop))
			{ /* better drop found */
			  nrBestDirs  = 1;
			  bestDirs[0] = i;
			  bestDrop = thisDrop;
			}
			else
			 if (! (AppCastREAL4(thisDrop) < AppCastREAL4(bestDrop)))
			   bestDirs[nrBestDirs++] = i;
		 }
		}
		else
			aNBisMV = TRUE;
	}
	/* WAAL_CW
	 * if (print)
	 *	printf("X nrdrops %d best %g first drop: %d\n",nrBestDirs,bestDrop,bestDirs[0]);
	 */
	if((bestDrop < 0) || /* all cells are higher */
	   (aNBisMV && bestDrop == 0) /* border of a flat */
	  )
	    /* assign a pit: return pixel itself */
	   {
	    *rTo = rowNr; *cTo = colNr;
	    return;
	   }
        PRECOND(nrBestDirs > 0);
	if (bestDrop == 0 || nrBestDirs == 1) /* a flat or no conflict */
		i = 0;
	else
	{
         PRECOND(nrBestDirs > 1);
         /* multiple outflow directions found */
	 /* Check on multiple candidates */
	 /* CW change this! find bestDrop recursily */
	 i = (int)floor(Ran()*nrBestDirs); /* CW make a ran [0.1> ! */
	 i = MIN(i, nrBestDirs-1);
	 /* WAAL_CW
	  * (void)printf("Random selection done out of %d rc %d %d picked %d\n", nrBestDirs,
	  * rowNr, colNr, i );
	  */
	}

	*rTo = RNeighbor(rowNr, bestDirs[i]);
	*cTo = CNeighbor(colNr, bestDirs[i]);
 }			

/* Calculates the ldd direction values for the UINT1 ldd map.
 * Inputs are the REAL8 dem map the UINT1 ldd map 
 * and the cell for which the output value should be calculated.
 * Returns integer 0 if exit is successful, 1 otherwise.
 */
static void Step1(
     MAP_UINT1 *ldd,		/* write-only output ldd map, 
                                 * 0 is written to cells that
                                 * must be solved in later steps
                                 */ 
     const MAP_REAL8 *dem, 	/* dem map	*/
     REAL8 demVal,		/* height of (r, c) */
     int r, 			/* row number of current cell */
     int c)			/* column number of current cell */

{
	int  rTo,cTo;

	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);

     	Lowest(&rTo, &cTo, dem, r, c, demVal);  /* lowest neighbor */
	/* Test whether the cell itself is returned or not */
	if((r != rTo || c != cTo)) 
 	{
	   	REAL8 toDem;
	   	PRECOND(dem->Get(&toDem,rTo,cTo,dem));

	   	(void)dem->Get(&toDem,rTo,cTo,dem);
	   	if(toDem == demVal)  /* a flat found */
			ldd->Put(0, r, c, ldd);
 		else
		{
			/* Simple case */
			ldd->Put(Ldddir(r, c, rTo, cTo), r,c,ldd);
 		}
 	}
	else
		ldd->Put(LDD_PIT, r, c, ldd); /* pit, neighbors higher */
}

/* Calculates the ldd directions on a flat of type 1.
 * Type 1 looks like this: \_
 *                            \
 * All cells in the flat should stream into the cell that has a 
 * elevation that is less than the elevation of the cells of the flat.
 * This lower cell is first cell in the list, although it is not a cell
 * of the flat, assigning a temporary ldd-code.
 * Returns if the cell is fixed Yes or No. 
 */
static BOOL Step2( 
	MAP_UINT1 *ldd,		/* read-write ldd map */
	const MAP_REAL8 *dem,	/* dem map */
	int r,		/* row current cell */
	int c)		/* column current cell */
{
	REAL8 	demValNB,demVal;
	UINT1 	outVal;
	int 	i, rNB, cNB;

	PRECOND(dem->Get(&demVal, r, c, dem));

	dem->Get(&demVal, r, c, dem);
	/* CW multiple candidate selection
	 * not yet implemented
	 */
	FOR_ALL_LDD_NBS(i)
	{
		rNB = RNeighbor(r, i);	/* row neighbor */
		cNB = CNeighbor(c, i);	/* column neighbor */

		if(dem->Get(&demValNB, rNB, cNB, dem)&&
		   (demVal >= demValNB) &&
		   (ldd->Get(&outVal, rNB, cNB, ldd)) &&
		    IS_VALID_CODE(outVal) &&
		    (!(RNeighbor(rNB,outVal)==r && CNeighbor(cNB,outVal)== c)) 
		  )
		{ /*
		   * NB is lower or equal and
		   *    has valid direction and does
		   *    not point to current cell
		   */
			UINT1 ldddir;
		        PRECOND(demVal == demValNB); 
			ldddir = Ldddir(r, c, rNB, cNB);	
			ldd->Put(MAKE_TEMP_CODE(ldddir), r, c, ldd);
			return TRUE;
		}
	}
	return FALSE;
}

/* Operates on a flat of type 2.
 * Type 2 looks like this: \_/. The pit should be as near to the center
 * of the depression as possible that is why the size is determined.
 * The rest of the elements of the flat should flow to this pit.
 * Returns 0 if successful, 1 otherwise.
 */
static BOOL Step3( 
		MAP_UINT1 *ldd,	/* read-write ldd.map */
		const MAP_REAL8 *dem,	/* dem.map */
		int r,		/* row current cell */
		int c)		/* column current cell */
{
	REAL8 demVal;
	int i,j;

     	PRECOND(dem->GetGetTest(dem) == GET_MV_TEST);
	PRECOND(dem->Get(&demVal, r, c, dem));
	dem->Get(&demVal, r, c, dem);

	FOR_ALL_LDD_NBS(i)
	{
	   int 	rNB = RNeighbor(r, i);
	   int 	cNB = CNeighbor(c, i);
	   UINT1 v;
	   if ( ldd->Get(&v,rNB,cNB,ldd) && IS_VALID_CODE(v)
	        && FlowsTo(v,rNB,cNB,r,c) /* NB flows in current */
	      )
		FOR_ALL_LDD_NBS(j)
		{
	   	  REAL8 demNB;
	   	  rNB = RNeighbor(r, j);
	   	  cNB = CNeighbor(c, j);
	          if ( ldd->Get(&v,rNB,cNB,ldd) && v == 0
	               && dem->Get(&demNB,rNB,cNB,dem) && demVal == demNB )
                  {
			UINT1 ldddir = Ldddir(r, c, rNB, cNB);	
			ldd->Put(MAKE_TEMP_CODE(ldddir), r, c, ldd);
			return TRUE;
		  }
		}
	}
	return FALSE;
}



/* Determines the UINT1 ldd map (ldd map) out of the dem map.
 * Assumes an REAL8 dem map present (Digital Elevation Model).
 * The function also needs to know the map size. 
 * Returns 0 if termination is successful, 1 otherwise.
 */
int Lddm(
     MAP_UINT1 *ldd,			/* Read-write output ldd map  */ 
     const MAP_REAL8 *dem) 		/* dem map */
{
	UINT1 	outVal;			/* value in ldd map */
	REAL8 	demVal;			/* value in dem map */
	int 	r, c, nrRows, nrCols;
	BOOL    cellsFixed;

     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	dem->SetGetTest(GET_MV_TEST, dem);
	ldd->SetGetTest(GET_MV_TEST, ldd);

	AppProgress("Simple case:\n");
	/* Do CALL for first phase
	 * and mv setting for MV in dem
	 */
	for(r = 0; r < nrRows; r++)
	{
	 	AppRowProgress(r);
	 	for(c = 0; c < nrCols; c++)
			if(dem->Get(&demVal, r, c, dem))
			    Step1(ldd, dem, demVal, r, c);
			else
			    ldd->PutMV(r, c, ldd);
	}
	/* Do CALL for second phase */
	AppProgress("\nFlats of type 1:\n");
	cellsFixed = TRUE;
	while(cellsFixed)	/* still cells fixed in flats of type 1 */
	{

		cellsFixed = FALSE;
		for(r = 0; r < nrRows; r++)
		 for(c = 0; c < nrCols; c++)
		    if(ldd->Get(&outVal, r, c, ldd) &&
		       (outVal == 0))/* to be solved */
			 cellsFixed |= Step2(ldd, dem, r, c);
		/* replace temp codes */
		if (cellsFixed) 
		 for(r = 0; r < nrRows; r++)
		 for(c = 0; c < nrCols; c++)
		    if(ldd->Get(&outVal, r, c, ldd) &&
		       IS_TEMP_CODE(outVal))
			 ldd->Put(MAKE_VALID_CODE(outVal),r,c,ldd);
	}

	/* Do CALL for third phase */
	AppProgress("\nFlats of type 2:\n");
	cellsFixed = TRUE;
	while(cellsFixed)	/* still cells fixed in flats of type 2 */
	{

		cellsFixed = FALSE;
		for(r = 0; r < nrRows; r++)
		 for(c = 0; c < nrCols; c++)
		    if(ldd->Get(&outVal, r, c, ldd) &&
		       (outVal == 0))/* to be solved */
			 cellsFixed |= Step3(ldd, dem, r, c);
		/* replace temp codes */
		if (cellsFixed) 
		 for(r = 0; r < nrRows; r++)
		 for(c = 0; c < nrCols; c++)
		    if(ldd->Get(&outVal, r, c, ldd) &&
		       IS_TEMP_CODE(outVal))
			 ldd->Put(MAKE_VALID_CODE(outVal),r,c,ldd);
	}

	/* remaining 0's are pits
	 */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	    if(ldd->Get(&outVal, r, c, ldd))
	    {
	       POSTCOND(!IS_TEMP_CODE(outVal));
	       if (outVal == 0)
		 ldd->Put(LDD_PIT,r,c,ldd);
	    }
 	/* check if we have a sound ldd
 	 */
 	POSTCOND( (!RepairLdd(ldd,ldd)) && (!repairLddModifiedMap));
	return 0;
}
