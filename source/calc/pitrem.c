#include "stddefx.h" 


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"
#include "mathx.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "p_calc_list.h" 

/***************/
/* EXTERNALS   */
/***************/

#ifdef DEBUG
 extern BOOL repairLddModifiedMap;
#endif

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/* CATCH (LIBRARY_INTERNAL
 */
typedef struct CATCH{
	int r,c;          /* pit of catchment it belongs to */
#ifdef DEBUG_DEVELOP
	INT4 orgCatchNr;  /* for easy debugging
	                   */
#endif
	INT4 partOfCatch; /* id of catchment it belongs to
	                   * equals own index initially 
	                   * updated each time a pit is removed
	                   */
	struct CATCH *updList;/* list of catchments
	                       * that must be updated if this catchment
	                       * is connected
	                       */
} CATCH;

typedef struct PIT {
	int r,c;       /* co-ordinate */
	REAL8 demVal;  /* value of initial dem, only for qsort */
} PIT;

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* set a cell to MV and it's upstream neighbors to pits
 */

static void PutMvUpsPits(
	MAP_UINT1 *ldd,   /* read-write modified ldd */
	int r,            /* row of cell to become MV */
	int c)            /* column of cell to become MV */
{
	int i;
	ldd->PutMV(r,c,ldd);
	FOR_ALL_LDD_NBS(i)
	{
		int rNB = RNeighbor(r,i); 
		int cNB = CNeighbor(c,i); 
		UINT1 dNB;
		ldd->Get(&dNB,rNB,cNB,ldd);
		if (FlowsTo(dNB, rNB, cNB, r, c))
		  ldd->Put(LDD_PIT,rNB,cNB,ldd);
	}
}

static void FindOutflowCell(
	UINT1  *outflowConCellCode, /* read-write dir from (outflowRow,outflowCol) to 
	                             * connection cell, 0 if none found
	                             */
	REAL8  *outflowLevel,       /* read-write best outflowLevel so far */
	int    *outflowRow,         /* write-only row of outflow cell */
	int     *outflowCol,        /* write-only col of outflow cell */
 	const MAP_INT4 *catch,		/* catchment map */
 	const MAP_REAL8 *dem,		/* catchment map */
 	const CATCH *catchList,		/* catchment table */
 	int r,			/* row of cell to be checked */
 	int c) 			/* column cell to be checked */
{	
	INT4 	catchId, catchIdNB;
	UINT1 	i,conCellCode=0; /* 0 no cell found */
	REAL8   conCellDem=USED_UNINIT_ZERO; /* guarded by conCellCode */
	REAL8   overflowLevel;

	PRECOND(catch->Get(&catchId, r, c, catch) && catchId > 0);
	PRECOND(dem->Get(&overflowLevel, r, c, dem));

	catch->Get(&catchId, r, c, catch);
	FOR_ALL_LDD_NBS(i)
	{
		int 	rNB = RNeighbor(r, i);
		int 	cNB = CNeighbor(c, i);
		if (catch->Get(&catchIdNB, rNB, cNB, catch))
		   if (catchList[catchId-1].partOfCatch  !=
		       catchList[catchIdNB-1].partOfCatch )
		    { 
		    	REAL8 v;
		    	dem->Get(&v,rNB,cNB, dem);
		    	if ( (!conCellCode) || v < conCellDem)
		    	{
		    		conCellCode = i;
		    		conCellDem  = v;
		    	}
		    }
	}
	if (!conCellCode)
		return;   /* no new potential overflow cell found */
	
	dem->Get(&overflowLevel,r,c, dem);
	overflowLevel = MAX(overflowLevel, conCellDem);
	if ( (!(*outflowConCellCode)) /* first overflow cell found */
	   || (overflowLevel < *outflowLevel)) /* better one found */
	{
		 *outflowLevel = overflowLevel; 
		 *outflowConCellCode = conCellCode;
		 *outflowRow = r;
		 *outflowCol = c;
	}
}

/* Finds the potential overflow cells and picks the lowest.
 * The entire catchment is searched, depth first, since
 * we do the whole catchement we can also compute the area of
 * the catchment
 * Returns the area (in pixels) if outflow is found, 0 if not found
 * or -1 in case of an memory error.
 */
 static REAL8 Outflow(  
        REAL8  *outflowLevel,  /* write only */
	int    *outflowRow,    /* write only */
	int    *outflowCol,    /* write only */
	int    *outflowConRow, /* write only */
	int    *outflowConCol, /* write only */
 	const MAP_UINT1 *ldd,	/* read-write ldd map */	
 	const MAP_INT4 *catch,	/* catchment cells  */
 	const CATCH *catchList,	/* catchment table */
 	const MAP_REAL8 *dem,	/* Digital Elevation map */
	int pitr,		/* row number of the pit */
	int pitc)		/* column number of the pit */
{
	UINT1  outflowConCellCode=0; /* dir from (outflowRow,outflowCol) to 
	                              * connection cell, 0 means no outflow
	                              * point found
	                              */
	NODE 	*list = LinkChkNd(NULL, pitr, pitc); /* current search tree */
	REAL8   conPitLevel, pitLevel, area=0;
	INT4 conCatchId; 
        int conPitRow, conPitCol;

	if(list == NULL)
		return -1; /* allocation failed */

	while(list != NULL)
	{   
		int r = list->rowNr;
		int c = list->colNr;
  		area++;
		FindOutflowCell(&outflowConCellCode, outflowLevel,
	           outflowRow, outflowCol, catch,dem,catchList, r,c);
		
		if (ReplaceFirstByUpsNbs(&list,ldd))
			return -1;
	}
	if (! outflowConCellCode) 
		return 0; /* stop, no outflowCell found, keep pit */

	dem->Get(&pitLevel, pitr,pitc, dem);

	*outflowConRow = RNeighbor(*outflowRow, outflowConCellCode);
	*outflowConCol = CNeighbor(*outflowCol, outflowConCellCode);

	catch->Get(&conCatchId, *outflowConRow, *outflowConCol, catch); 
	conCatchId = catchList[conCatchId-1].partOfCatch;
        conPitRow = catchList[conCatchId-1].r;
        conPitCol = catchList[conCatchId-1].c;
	dem->Get(&conPitLevel, conPitRow,conPitCol, dem);
	if (conPitLevel > pitLevel)
		return 0; /* stop, keep pit */
	return area;
}

static int ComputeCore(
	REAL8 *coreArea,      /* write-only */
	REAL8 *coreVolume,    /* write-only */
	REAL8  outflowLevel, 
	int    pitr,
	int    pitc, 
	const MAP_UINT1 *ldd, 
	const MAP_REAL8 *dem)
{
	NODE 	*list = LinkChkNd(NULL, pitr, pitc); /* current search tree */

	if(list == NULL)
		return 1; /* allocation failed */

	*coreArea = 0;
	*coreVolume = 0;
	while(list != NULL)
	{   
		int r = list->rowNr;
		int c = list->colNr;
  		REAL8 l;

  		dem->Get(&l,r,c,dem);
		if (l < outflowLevel)
		{
  		 (*coreArea)++;
  		 (*coreVolume) += (outflowLevel - l);
		  if (ReplaceFirstByUpsNbs(&list,ldd))
			return 1;
		}
	        else
	        	list = RemFromList(list);
	}
	return 0;
}

/* traverse from outflowCell to pit
 * reversing all direction
 */
static void ReversePath(
	MAP_UINT1 *ldd, 
	int outflowRow, 
	int outflowCol, 
	int outflowConRow, 
	int outflowConCol)
{
	int r = outflowRow;	
	int c = outflowCol;	
	int newDSr = outflowConRow; /* new down stream row */
	int newDSc = outflowConCol; /* new down stream row */
	UINT1 oldCode = 0;

#ifdef DEBUG
	int sr=r, sc=c; /* start reversion, to check on cycles */
#endif

	while (oldCode != LDD_PIT)
	{
		ldd->Get(&oldCode, r, c, ldd);
		POSTCOND(oldCode != MV_UINT1);
		ldd->Put(Ldddir(r, c, newDSr, newDSc), r, c, ldd);
		newDSr = r;
		newDSc = c;
		r = DownStrR(r, oldCode);
		c = DownStrC(c, oldCode);
                /* test if we are not in
                 * a cycle
                 */
                POSTCOND(oldCode == LDD_PIT || COORD_NE(r,c,sr,sc)); 
	}

#ifdef DEBUG_DEVELOP
       /* test if we did not INTRODUCE a cycle
        */
         /* start point, prev. (old) pit */
         sr = r = newDSr;
         sc = c = newDSc;
         oldCode = 0;

         while (  oldCode != LDD_PIT )
         {
		ldd->Get(&oldCode, r, c, ldd);
		r = DownStrR(r, oldCode);
		c = DownStrC(c, oldCode);
                POSTCOND(COORD_NE(r,c,sr,sc));
         }
#endif
}

static void CutDem(
	MAP_REAL8 *dem,
	int    r,
	int    c, 
	REAL8  cutLevel,
	const MAP_UINT1 *ldd) 
{
	REAL8 l = cutLevel;
	UINT1 d = 0;
	while (l >= cutLevel
	       && d != LDD_PIT) /* do not proceed if connection pit is processed */
	{
		dem->Put(cutLevel,r,c,dem);
		ldd->Get(&d,r,c,ldd);
		POSTCOND(d != MV_UINT1);
		r = DownStrR(r, d);
		c = DownStrC(c, d);
		dem->Get(&l,r,c,dem);
	}
}

static int FillDem(
	MAP_REAL8 *dem,
	int    pitr,
	int    pitc, 
	REAL8  outflowLevel,
	const MAP_UINT1 *ldd) 
{
	NODE 	*list = LinkChkNd(NULL, pitr, pitc); /* current search tree */

	if(list == NULL)
		return 1; /* allocation failed */

	while(list != NULL)
	{   
		int r = list->rowNr;
		int c = list->colNr;
  		REAL8 l;

  		dem->Get(&l,r,c,dem);
		if (l < outflowLevel)
		{
  		  dem->Put(outflowLevel,r,c,dem);
		  if (ReplaceFirstByUpsNbs(&list,ldd))
			return 1;
		}
	        else
	        	list = RemFromList(list);
	}
	return 0;
}

static BOOL ThresholdFailed(
	REAL8  val,
	int    r, int c,
	const MAP_REAL8 *thmap)
{
	REAL8 th;
	if (thmap->Get(&th, r,c, thmap))
		return val > th;
	else
		return TRUE;
}


/* Removes one pit given by the input (pitr, pitc).
 * First the overflow cell and connection point are determined. After 
 * which the overflow level can be calculated. If the thresholds are
 * satisfied, the path to the overflow cell is inverted. After that the
 * dem.map should be adjusted if necessary.
 * Returns 1 in case of an error, 0 otherwise.
 */
 static int RemPit(
 	MAP_UINT1 *ldd,		/* read-write ldd map */	
 	MAP_REAL8 *dem,		/* write-only modified dem */
 	const MAP_INT4 *catch,		/* catchment map */
 	CATCH *catchList,		/* catchment table */
 	const MAP_REAL8 *depth,		/* depth threshold */
 	const MAP_REAL8 *volume,	/* volume threshold */
 	const MAP_REAL8 *area,		/* area threshold */
     	const MAP_REAL8 *mminput,	/* mminput threshold map */
 	int pitr,			/* row of pit to remove */
 	int pitc)			/* column of pit to remove */
{
	REAL8  pitLevel, outflowLevel, catchArea;
	REAL8  coreArea, coreVolume;
	int    outflowRow, outflowCol;
	int    outflowConRow, outflowConCol;
	INT4 pitCatchID,newCatchID;
	CATCH  *cf,*pf;

#ifdef DEBUG
        /* check if prev. dem modifications
         * removed this pit erroneously
         */
	UINT1 c;
	ldd->Get(&c, pitr, pitc, ldd);
	PRECOND(c == LDD_PIT);
#endif

        catchArea = Outflow( &outflowLevel, &outflowRow, &outflowCol, 
                        &outflowConRow, &outflowConCol,
 	                ldd, catch, catchList, dem, pitr, pitc);


	if (catchArea <= 0)
		return catchArea < 0; /* mem failure if -1, keep pit if 0 */

	dem->Get(&pitLevel, pitr, pitc, dem);

	if (ThresholdFailed(outflowLevel-pitLevel, pitr,pitc, depth)) /* same units no adjustment */
		return 0; /* keep pit */

	if (ComputeCore(&coreArea, &coreVolume, outflowLevel, 
	                 pitr,pitc, ldd, dem))
		return 1; /* failure */

	catchArea  *= Area();
	coreArea   *= Area();
	coreVolume *= Area();

	if (ThresholdFailed(coreVolume, pitr,pitc, volume))
		return 0; /* keep pit */
	if (ThresholdFailed(coreArea, pitr,pitc, area))
		return 0; /* keep pit */
	if (ThresholdFailed( (coreVolume/catchArea)*1000, pitr,pitc, mminput))
		return 0; /* keep pit */

        /* now pit can be removed
         */

	/* update (sub)catchment features 
	 * the connection cell identifies the catchement where the old pit
	 * now belongs to
	 */
	catch->Get(&newCatchID,outflowConRow,outflowConCol,catch);
	newCatchID = catchList[newCatchID-1].partOfCatch;
	catch->Get(&pitCatchID,pitr,pitc,catch);
	pitCatchID = catchList[pitCatchID-1].partOfCatch;
#ifdef DEBUG
       /* this catchment and connecting catchment are different
        */
	PRECOND(pitCatchID != newCatchID);
	ldd->Get(&c, catchList[newCatchID-1].r,
	             catchList[newCatchID-1].c, ldd);
       /* connecting catchment is still a valid one (with a pit)
        */
	PRECOND(c == LDD_PIT);
#endif
	cf = catchList+(pitCatchID-1);
	do {
		cf->r = catchList[newCatchID-1].r;
		cf->c = catchList[newCatchID-1].c;
		cf->partOfCatch = catchList[newCatchID-1].partOfCatch;
		pf = cf;
		cf = cf->updList;
	} while (cf != catchList+(pitCatchID-1));
	pf->updList = catchList[newCatchID-1].updList;
	catchList[newCatchID-1].updList = catchList+(pitCatchID-1);


	 /* do filling before pit removal
	  */
	 if(appLddDemModifier == APP_LDDDEMFILL)
		if (FillDem(dem, pitr, pitc, outflowLevel, ldd))
			return 1; /* failure */

	 ReversePath(ldd, outflowRow, outflowCol, outflowConRow, outflowConCol);

	 /* do cutting after pit removal
	  * removed pit is now a path trough the connection cell
	  * into another catchment
	  */
	 if(appLddDemModifier == APP_LDDDEMCUT)
		CutDem(dem, pitr, pitc, pitLevel, ldd);

 	return 0; /* pit succesfully removed */
}

/* sort with high first in array
 * make sort-order unique by also taking
 * r and c in account
 */
static int CmpPit(
		const PIT *in1,
	 	const PIT *in2)
{
	int res = - CmpDouble( &(in1->demVal), &(in2->demVal));
	if ( res)
		return res;
	if (in1->r != in2->r)
		return in1->r - in2->r;
	
	PRECOND( in1->c != in2->c);
	return in1->c - in2->c;
}

/* decide if a pit must be removed
 * due to external setting
 */
static BOOL GlobOptionPermitRemoval(
     const MAP_UINT1 *ldd,
	int r,
	int c)
{
   if (appPitOnBorder)
   {   /* lddout is active
        */
	int i;
	FOR_ALL_LDD_NBS(i)
	{ /* if one of the NBs is MV or outside
	   * then keep pit
	   */
	        UINT1 d;
		int rNB = RNeighbor(r,i);
		int cNB = CNeighbor(c,i);
		if (!ldd->Get(&d,rNB,cNB,ldd))
			return FALSE;
	}
   }
  return TRUE;
}

/* Modifies LDD (ldd map) and makes a modified DEM (outDem map).
 * Modifies the ldd map by removing pits and determines outDem map.
 * Assumes a dem map (Digital Elevation Model), a depth map, a 
 * volume map, an area map, an input map present being all REAL8 maps.
 * The function also needs to know the map size.
 * Returns 0 if termination is successful, 1 otherwise.
 */
int PitRem(
     MAP_UINT1 *ldd,			/* Read-write output map  */ 
     MAP_REAL8 *dem,			/* Read-write output map  */ 
     MAP_INT4 *catch,			/* Read-write catchment map */
     const MAP_REAL8 *inDem, 		/* input dem map */
     const MAP_REAL8 *depth, 		/* depth threshold map	*/
     const MAP_REAL8 *volume, 		/* volume threshold map	*/
     const MAP_REAL8 *area, 		/* area threshold map	*/
     const MAP_REAL8 *mminput) 		/* mminput threshold map	*/
{
	INT4 	nPits = 0;		/* number of pits to be done */ 
	PIT 	*pits = NULL;	/* list of pits to remove */
	CATCH 	*catchList = NULL;	/* catchment table  */
	REAL8 	demVal;
	int 	i,r, c, nrRows, nrCols;
	UINT1   lddVal;

	dem->SetGetTest(GET_MV_TEST, dem);
	depth->SetGetTest(GET_MV_TEST, depth);
	volume->SetGetTest(GET_MV_TEST, volume);
	mminput->SetGetTest(GET_MV_TEST, mminput);
	area->SetGetTest(GET_MV_TEST, area);
	ldd->SetGetTest(GET_MV_TEST, ldd);
	inDem->SetGetTest(GET_MV_TEST, inDem);
	catch->SetGetTest(GET_MV_TEST, catch);

	nrRows = dem->NrRows(dem);
	nrCols = dem->NrCols(dem);

	/* copy input dem to result dem
	 * and put MV's if one of (ldd,dem) maps is MV 
	 */
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
		if ( ldd->Get(&lddVal, r,c, ldd))
		{
		  if(inDem->Get(&demVal, r, c, inDem))
			dem->Put(demVal, r, c, dem); /* both defined */
		  else
		  { /* lld defined but dem not */
			dem->PutMV(r,c, dem);	
			PutMvUpsPits(ldd, r, c);
		  }
		}
		else /* dem defined but ldd not */
			dem->PutMV(r,c, dem);	

	/* Search for pits in the map, 
	 * create catchment map
	 */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	if (ldd->Get(&lddVal, r, c, ldd))
	{
	 if (lddVal == LDD_PIT)
	 {
		void *cp;
		nPits++;
		/* allocate and increase lists */
		if ((cp = ChkRealloc(catchList, (sizeof(CATCH)) * nPits))==NULL)
			goto error;
		catchList = (CATCH *)cp;
		if ((cp = ChkRealloc(pits, (sizeof(PIT)) * nPits))==NULL)
			goto error;
		pits = (PIT *)cp;
		pits[nPits-1].r = catchList[nPits-1].r = r;
		pits[nPits-1].c = catchList[nPits-1].c = c;

	        PRECOND(dem->Get(&demVal, r, c, dem));
	        (void)dem->Get(&demVal, r, c, dem);
		pits[nPits-1].demVal = demVal;

		/* initial part of own catchment */
		catchList[nPits-1].partOfCatch = nPits; 
#ifdef DEBUG_DEVELOP
		catchList[nPits-1].orgCatchNr = nPits; 
#endif 

		catch->Put(nPits, r, c, catch);
	 }
	 else
	      catch->Put(0,r,c,catch); /* find catch with Catchment func */
       }
       else
	      catch->PutMV(r,c,catch);

	/* checkif we have a sound ldd
	 */ 
 	POSTCOND( (!RepairLdd(ldd,ldd)) && (!repairLddModifiedMap));

	/* identify catchments */
	if (Catch(catch, ldd, catch))
		goto error;

	if (nPits == 1)
		goto done;

	/* init updList, ptr-based
	 * can't be done in the realloc loop
	 */
	for(i=0; i < nPits; i++) 
		catchList[i].updList = catchList+i;

	qsort(pits, (size_t)nPits, sizeof(PIT), (QSORT_CMP)CmpPit);

      /*
       * for(i=0; i < nPits; i++) 
       *  printf("PIT %d %d %g\n",pits[i].r,pits[i].c,pits[i].demVal);
       */

       /* not the last one !
        * although it must work, since 
        * for the last one we are not able to find
        * an outflow point, so that pit is kept
        */
#ifdef DEBUG_DEVELOP
	for(i=0; i < nPits; i++) 
#else
	for(i=0; i < (nPits-1); i++) 
#endif
	{
	 AppProgress("removing pit nr. %5d out of %5d\r", i+1, nPits);
	 if (GlobOptionPermitRemoval(ldd,pits[i].r,pits[i].c))
	      {
		if(RemPit(ldd, dem, catch, catchList, depth, volume,
			  area, mminput, pits[i].r, pits[i].c))
				  	goto error;
#ifdef HEAVY_DEBUG
	        /* check if we have a sound ldd
	         * after each pit removal
	         */ 
 	        POSTCOND( (!RepairLdd(ldd,ldd)) && (!repairLddModifiedMap));
#endif
 	      }
	}

done:
 	POSTCOND( (!RepairLdd(ldd,ldd)) && (!repairLddModifiedMap));
	Free(catchList);
	Free(pits);
	return 0;
error:
	Free(catchList);
	Free(pits);
	return 1;
}
