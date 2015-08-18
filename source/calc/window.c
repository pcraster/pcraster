#include "stddefx.h" 

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "app.h"	/* appUnitTrue, appOutput */
#include <string.h>	/* memmove */
#include "mathx.h"	/* modf */
#include "table.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/* DATA (LIBRARY_INTERNAL)
 */
typedef struct DATA {	
	REAL8 count;		/* counted value */
	INT4 value;		/* class value */
}DATA;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* computes the number of pixelss surrounding
 * the current pixel that must 
 * be analyzed 
 */
static int DetWindow(
	REAL8 *bw, /* write-only, border weight, 0 if border pixel are
	            * compeltely covered by window
	            */
	REAL8 winSize) /* window size */
{
	double Floor;
	PRECOND(winSize > 0);
	winSize /= Side();
	if (winSize <= 1)
	{
		*bw = winSize;
		return 0;
	}
	winSize *= 0.5;
	winSize -= 0.5;
	*bw = modf(winSize, &Floor);
	if (*bw == 1)
		*bw = 0;
	return (int)ceil(winSize);
}

static double Weight(
	int pw,    /* half pixel window size */
	int r,     /* row delta index */
	int c,     /* column delta index */
	double bw) /* border weight */
{
	REAL8 w = 1;
	if (bw > 0)
	{ /* determine border weigths */
		if (ABS(r) == pw)
			w *= bw;
		if (ABS(c) == pw)
			w *= bw;
	}
	return w;
}

/* Determines the minimum value of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowMin(
     MAP_REAL8 *min,		/* write-only output minimum map  */ 
     const MAP_REAL8 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
	 	{
			REAL8 	winSize;
			if (winsize->Get(&winSize, r, c, winsize) && (0 < winSize) )
			{
			        /* Determine window */
				REAL8 value,winMin;
				int   rWin, cWin, pw; 
				REAL8 bw; /* border weigth */

				pw =  DetWindow(&bw, winSize);

				SET_MV_REAL8(&winMin);
				/* Calculate in window */
				for(rWin = -pw; rWin <= pw; rWin++)
				 for(cWin= -pw; cWin <= pw; cWin++)
					if ( val->Get(&value, rWin+r, cWin+c, val))
					 if (IS_MV_REAL8(&winMin))
					 	winMin = value;
					 else
					 	winMin = MIN(winMin,value);
				if (IS_MV_REAL8(&winMin))
				 min->PutMV(r, c, min);
				else
				 min->Put(winMin, r, c, min);
			}
			else
				/* MV or winsize <= 0 */
				min->PutMV(r, c, min); 
		}	
	}
	AppEndRowProgress();
	return 0;
}

/* Determines the maximum value of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowMax(
     MAP_REAL8 *max,		/* write-only output max map  */ 
     const MAP_REAL8 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
	 	{
			REAL8 	value, winSize;
			if(winsize->Get(&winSize, r, c, winsize) && (0 < winSize) )
			{
				/* Determine window */
				REAL8 winMax;
				int   rWin, cWin, pw; 
				REAL8 bw; /* border weigth */

				pw =  DetWindow(&bw, winSize);

				SET_MV_REAL8(&winMax);
				/* Calculate in window */
				for(rWin = -pw; rWin <= pw; rWin++)
				 for(cWin= -pw; cWin <= pw; cWin++)
					if ( val->Get(&value, rWin+r, cWin+c, val))
				 	 if (IS_MV_REAL8(&winMax))
				 	  winMax = value;
				 	 else
				 	  winMax = MAX(winMax,value);
				if (IS_MV_REAL8(&winMax))
				 max->PutMV(r, c, max);
				else
				 max->Put(winMax, r, c, max);
			}
			else
				 max->PutMV(r, c, max);
		 }	
	}
	AppEndRowProgress();
	return 0;
}

/* Determines the average value of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowAverage(
     MAP_REAL8 *average,	/* write-only output average map  */ 
     const MAP_REAL8 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
		{
			REAL8 	value, winSize;
			if(winsize->Get(&winSize, r, c, winsize) && (0 < winSize) )
			{
				REAL8 count = 0, winTotal = 0;
				int   rWin, cWin, pw; 
				REAL8 bw; /* border weigth */

				pw =  DetWindow(&bw, winSize);

				/* Calculate in window */
				for(rWin = -pw; rWin <= pw; rWin++)
				 for(cWin= -pw; cWin <= pw; cWin++)
				 {
					if(val->Get(&value, rWin+r, cWin+c, val))
					{
						REAL8 w = Weight(pw,rWin,cWin,bw);
						winTotal += value*w;
						count    += w;
					}
				 }
				if(count > 0)
				 average->Put(winTotal/count, r, c, average);
				else
				 average->PutMV(r, c, average);
			}
			else
				/* MV or winSize <= 0 */
				average->PutMV(r, c, average);
		}
	}
	AppEndRowProgress();
	return 0;
}

/* Determines the total value of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowTotal(
     MAP_REAL8 *total,		/* write-only output total map  */ 
     const MAP_REAL8 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
		{
			REAL8 	value,winSize;
			if (winsize->Get(&winSize, r, c, winsize) && (0 < winSize) )
			{	
				REAL8 winTotal = 0;
				int   rWin, cWin, pw; 
				REAL8 bw; /* border weigth */
				BOOL  valSet=FALSE;

				pw =  DetWindow(&bw, winSize);

				/* Calculate in window */
				for(rWin = -pw; rWin <= pw; rWin++)
				 for(cWin= -pw; cWin <= pw; cWin++)
				 {
					if(val->Get(&value, rWin+r, cWin+c, val))
					{
					 valSet=TRUE;
					 winTotal += value * Weight(pw, rWin, cWin, bw);
					}
				 }
				if (valSet)
				 total->Put(winTotal, r, c, total);
				else
				 total->PutMV(r, c, total);
			}
			else
				/* MV or winSize <= 0 */
				total->PutMV(r, c, total);
		}
	}
	AppEndRowProgress();
	return 0;
}

static DATA const *foundRec = NULL;
static BOOL foundConflict = FALSE;
static REAL8 maxCount = 0;

static void ForAllMaj(
	const DATA *e)
{
	if (e->count == maxCount)
	{
		if (foundRec == NULL)
			foundRec = e;
		else
		{
		 foundConflict = TRUE;
		 if (e->value > foundRec->value)
		 	foundRec = e;
		}
	}
}

static int ReturnId(const DATA *e)
{
	return e->value;
}

static void InitRec(DATA *e, int i)
{
	e->value = i;
	e->count = 0;
}

static int CmpRec(
	const DATA *e1,
	const DATA *e2)
{
	return e1->value - e2->value;
}

/* Determines the majority of values of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowMajority(
     MAP_INT4 *majority,	/* write-only output majority map  */ 
     const MAP_INT4 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;
	INT4 	value;			/* value at r, c in value map */
	REAL8 	winSize;		/* window Size */

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);
	majority->SetGetTest(GET_MV_TEST, majority);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
	    AppRowProgress(r);
	    for (c = 0; c < nrCols; c++)
	    {
		if(winsize->Get(&winSize, r, c, winsize) && (0 < winSize))
		{
		    REAL8 prevMaxCount=-1;
		    REAL8 bw; /* border weigth */
		    INT4 outputValue;
		    int pw =  DetWindow(&bw, winSize);
		    maxCount = 0;

		    while( prevMaxCount != maxCount)
		    {
			SEARCH_TABLE *table;
			int   rWin, cWin; 

			prevMaxCount = maxCount;
			maxCount = 0;

			table = STnew((size_t)val->HintNrFastList(val), 
			         sizeof(DATA), (RETURN_ID)ReturnId,
			         (INIT_REC)InitRec, (QSORT_CMP) CmpRec); 
			if(table == NULL)
				return 1;

			/* Calculate in window */
			for(rWin = -pw; rWin <= pw; rWin++)
			 for(cWin= -pw; cWin <= pw; cWin++)
			 {
				if ( val->Get(&value, rWin+r, cWin+c, val))
				{
					DATA key,*rec;
					key.value = value;
					rec = STfindOrInsert(table,&key);
					if (rec == NULL)
					{
						STfree(table);
						return 1;
					}
					rec->count += Weight(pw,rWin,cWin,bw);
					maxCount = MAX(maxCount, rec->count);
				}
			 }
			if (maxCount == 0) 
			{ /* all mv in window */
				prevMaxCount = maxCount; /* stop iter */
				outputValue = MV_INT4;
			}
			else 
			{
				foundRec = NULL;
				foundConflict = FALSE;
				STforAll(table, (ACTION_REC)ForAllMaj);
				POSTCOND(foundRec != NULL);
				if (foundConflict)
					pw += 1; /* extend window */
				else
					prevMaxCount = maxCount;
				outputValue = foundRec->value;
			}
			STfree(table);
		  }
		  if (outputValue == MV_INT4)
		    majority->PutMV(r, c, majority);
		  else
		    majority->Put(outputValue, r, c, majority);
		}	
		else
		{
			/* MV or winSize <= 0 */
			majority->PutMV(r, c, majority);
		}
	    }
	}
	AppEndRowProgress();
	return 0;
}

/* Determines the diversity of values of a window, for each cell.
 * Returns 0 if termination is successful, 1 otherwise. 
 */
int WindowDiversity(
     MAP_REAL8 *divM,	/* write-only output diversity map  */ 
     const MAP_INT4 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{
	int 	r, c, nrRows, nrCols;
	INT4 	value;			/* value at r, c in value map */
	REAL8 	winSize;		/* window Size */

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);
	divM->SetGetTest(GET_MV_TEST, divM);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
	    AppRowProgress(r);
	    for (c = 0; c < nrCols; c++)
	    {
		if( winsize->Get(&winSize, r, c, winsize) && (0 < winSize) )
		{
		 REAL8 bw;
		 int pw =  DetWindow(&bw, winSize);
		 REAL8 divVal = 0;
		 SEARCH_TABLE *table;
		 int   rWin, cWin; 

		 table = STnew((size_t)val->HintNrFastList(val), 
			 sizeof(DATA), (RETURN_ID)ReturnId,
			 (INIT_REC)InitRec, (QSORT_CMP) CmpRec); 
		 if(table == NULL)
			return 1;

		/* Calculate in window */
		for(rWin = -pw; rWin <= pw; rWin++)
		 for(cWin= -pw; cWin <= pw; cWin++)
		 {
			if ( val->Get(&value, rWin+r, cWin+c, val))
			{
				DATA key,*rec;
				key.value = value;
				rec = STfindOrInsert(table,&key);
				if (rec == NULL)
				{
					STfree(table);
					return 1;
				}
				if (rec->count == 0) /* first time encountered */
					divVal++;
				rec->count = 1;
			}
		}
		STfree(table);
		if (divVal > 0)
		  divM->Put(divVal, r, c, divM);
		else
		  divM->PutMV(r, c, divM);
		}	
		else
		{
			/* MV or winSize <= 0 */
			divM->PutMV(r, c, divM);
		}
	    }
	}
	AppEndRowProgress();
	return 0;
}

/* Performs high-pass filter function.
 * Returns 0.
 */
int WindowHighpass( 
     MAP_REAL8 *h,	/* write-only output h map  */ 
     const MAP_REAL8 *val, 	/* input value map */
     const MAP_REAL8 *winsize) 	/* input window size map */
{ 
	int 	r, c, nrRows, nrCols;

	val->SetGetTest(GET_MV_TEST, val);
	winsize->SetGetTest(GET_MV_TEST, winsize);

	nrRows = val->NrRows(val);
	nrCols = val->NrCols(val);

	for (r = 0; r < nrRows; r++)
	{
		AppRowProgress(r);
		for (c = 0; c < nrCols; c++)
		{
			REAL8 	centralValue, winSize;
			if(winsize->Get(&winSize, r, c, winsize) &&
			     (0 < winSize)                       &&
			      val->Get(&centralValue, r, c, val) )
			{
				REAL8 bw;
				int rWin, cWin, pw =  DetWindow(&bw, winSize);
				/* make negative so scanning will adjust for
				 * the central pixel
				 */
				REAL8 value, areaSurr, totalSurr;

				areaSurr  = -Weight(pw, 0,0, bw);
				totalSurr = centralValue * areaSurr;

				/* Calculate in window */
				for(rWin = -pw; rWin <= pw; rWin++)
				 for(cWin= -pw; cWin <= pw; cWin++)
				   if ( val->Get(&value, rWin+r, cWin+c, val))
				   {
				 	REAL8 a    = Weight(pw,rWin,cWin,bw);
				 	areaSurr  += a;
				 	totalSurr += (a * value);
				   }
				if (areaSurr == 0)
				 h->PutMV(r, c, h); /* all missing value */
				else
				 h->Put( (2 * areaSurr * centralValue) - totalSurr, r, c, h);
			}
			else
				h->PutMV(r, c, h);
		 }
	}
	AppEndRowProgress();
	return 0;
}
