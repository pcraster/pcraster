#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"
#include "csf.h"
#include "misc.h"
#include "app.h"   
#include "table.h" /* used in minority and majority  */

/* apps. called */
#include "col2map.h"

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
/* Can't we reuse POS_X or POS_Y for POS_ID ?
 */
#define ID_NOT_IN_MAP  -1

#define POS_COUNT 0 /* reuse POS_X */
#define POS_ID 3
#define REC_SIZE 4

typedef struct DATA{
	double val;		/* value is search-key */
	long int index;		/* index of first record found with that
	                         * value
	                         */
}DATA;

typedef int (*CALC_CELL)(
 	REAL8 *val,   /* write only */
 	REAL8 **recs, /* some sort the recs, others don't change it */
 	size_t   nrRecs);

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/
/* local copy of ptrs to records
 */
static REAL8 **recList=NULL;
static size_t     nrRecords=0; /* number of records actually filled */
/* used in majority, minority and directional
 */  
static size_t     nrCountConflict=0;

/******************/
/* IMPLEMENTATION */
/******************/

/* Compares two records according to the values.
 * Returns integer according to the result of the comparison.
 */
 static int CmpVal(
 	const DATA *e1,		/* pointer to DATA record 1 */
 	const DATA *e2)		/* pointer to DATA record 2 */
 {
	double diff = e1->val - e2->val;
	if(diff < 0)
		return -1;
	return (diff > 0);
 }

/* Compares two records according to the ID's.
*  It's safe here to cmp double since they are actually int's
 * Returns integer according to the result of the comparison.
 */
 static int CmpId(
 	const REAL8 **e1,	/* pointer to record 1 */
 	const REAL8 **e2)	/* pointer to record 2 */
 {
  // OLS: the following line does not work
  // with cellIDs larger than MAX_INT (bug/sf661)
  // return((*e1)[POS_ID] - (*e2)[POS_ID]);
  if( ((*e1)[POS_ID] - (*e2)[POS_ID]) < 0){
    return -1;
  }
  else if( ((*e1)[POS_ID] - (*e2)[POS_ID]) == 0){
    return 0;
  }
  else{
    return 1;
  }
 }


/* Calculates the average direction.
 * Radians are always between 0 and 2pi or -1 for no direction.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 */
 static int CalcDirection(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
	double *valList = ChkMalloc(sizeof(double)*nrRecs);
	size_t i,n=0; /* number not -1 */

	if (valList == NULL)
		return 1;

	for(i = 0; i < nrRecs; i++)
		if (recs[i][POS_V] != -1)
			valList[n++] = recs[i][POS_V];

	if (n == 0) 
		*val = -1;
	else 
	{
		*val = DirectionalMean(valList, n);
		if (n != nrRecs)
			nrCountConflict++;
	}
	Free(valList);
	return 0;
}

/* Compares two records according to the count and value.
 *  It's safe here to cmp double since they are actually int's
 *   both the count and the value (always classified if minority or majority 
 *   is used 
 * Returns integer according to the result of the comparison.
 */
 static int CmpCountVal(
 	const REAL8 **e1,	/* pointer to record 1 */
 	const REAL8 **e2)	/* pointer to record 2 */
 {
	int n =  ((*e1)[POS_COUNT] - (*e2)[POS_COUNT]);
	if (!n)
	  n =  ((*e1)[POS_V] - (*e2)[POS_V]);
 	return n;
 }

 static long int ReturnId(const DATA *e)
 {
 	return (long int)(e->val);
 }

 static void InitFastList(DATA *e, int i)
 {
 	e->val = i;
 	e->index = -1;
 }

 static int CalcSortTable(
 	REAL8 **recs,
 	size_t  nrRecs,
 	size_t   idNotUsed)
 {
	size_t i;
	SEARCH_TABLE *table = STnew(MIN(40, nrRecs), sizeof(DATA), 
	                     (RETURN_ID)ReturnId, (INIT_REC)InitFastList, (QSORT_CMP) CmpVal);
	if (table == NULL)
		return 1;

	/* Scan records for values and occurrences */
	for(i = 0; i < nrRecs; i++)
	{
		DATA key, *new;
		key.val = recs[i][POS_V];
		new = STfind(table, &key);
		if(new == NULL || new->index == -1) /* new or fastlist */
		{
			if (new == NULL)
			 if( (new = STinsert(table, &key)) == NULL )
			 { STfree(table);
			   return 1;
			 }
			new->index = i;
			new->val = key.val;
		        recs[i][POS_COUNT] = 0; /* init the one used */
		}
		else
		{
		        /* mark the one not used 
		         * in such a way it will end up at 
		         * the beginning or the end
		         */
		        recs[i][POS_COUNT] = idNotUsed; 
		}
		recs[new->index][POS_COUNT]++;
	}
	STfree(table);
 	qsort(recs, (size_t) nrRecs, sizeof(REAL8*), (QSORT_CMP)CmpCountVal);
 	return 0;
}

/* Calculates the most occurring value in each cell.
 * The value that occurs the most will be the output value.
 * Returns 1 in case of failed memory allocation 0 otherwise.
 */
 static int CalcMajority(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
 	PRECOND(nrRecs > 1);
 	if (CalcSortTable(recs, nrRecs, (size_t)0))
 		return 1;
 	/* now the majority with highest value 
 	 * has ended up in the last one 
 	 */
 	POSTCOND(recs[nrRecs-1][POS_COUNT] > 0);
 	*val = recs[nrRecs-1][POS_V];
 	if (recs[nrRecs-1][POS_COUNT] == recs[nrRecs-2][POS_COUNT])
 		nrCountConflict++;
 	return 0;
}

/* Calculates the least occurring value in each cell.
 * The value that occurs the most will be the output value.
 * Returns 1 in case of failed memory allocation 0 otherwise.
 */
 static int CalcMinority(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
 	PRECOND(nrRecs > 1);
 	if (CalcSortTable(recs, nrRecs, nrRecs+1))
 		return 1;
 	/* now the minority with lowest value 
 	 * has ended up in the first one 
 	 */
 	POSTCOND(recs[0][POS_COUNT] > 0);
 	*val = recs[0][POS_V];
 	if (recs[0][POS_COUNT] == recs[1][POS_COUNT])
 		nrCountConflict++;
 	return 0;
}

/* Calculates the average value of the record values that fall in cell.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 */
 static int CalcAverage(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
	size_t 	i;
	double totalVal = 0;
	PRECOND(nrRecs > 0);
	for(i = 0; i < nrRecs; i++)
	{
		PRECOND(recs[0][POS_ID] == recs[i][POS_ID]);
		totalVal += recs[i][POS_V];
		*val = totalVal / nrRecs;
	}
	return 0;
}

/* Calculates the total (sum) of the record values that fall in cell.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 */
 static int CalcTotal(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
	size_t 	i;
	double totalVal = 0;
	PRECOND(nrRecs > 0);
	for(i = 0; i < nrRecs; i++)
	{
		PRECOND(recs[0][POS_ID] == recs[i][POS_ID]);
		totalVal += recs[i][POS_V];
		*val = totalVal;
	}
	return 0;
}

/* Calculates the minumum value of the record values that fall in cell.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 */
 static int CalcMin(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
	size_t 	i;
	*val = recs[0][POS_V];
	for(i = 1; i < nrRecs; i++)
	{
		PRECOND(recs[0][POS_ID] == recs[i][POS_ID]);
		if ( *val >  recs[i][POS_V])
		     *val = recs[i][POS_V];
	}
	return 0;
}

 /* Calculates the maximum value of the record values that fall in cell.
 * Returns 1 in case of failed memory allocation, 0 otherwise.
 */
 static int CalcMax(
 	REAL8 *val,  /* write only */
 	REAL8 **recs,
 	size_t   nrRecs)
 {
	size_t 	i;
	PRECOND(nrRecs > 0);
	*val = recs[0][POS_V];
	for(i = 1; i < nrRecs; i++)
	{
		PRECOND(recs[0][POS_ID] == recs[i][POS_ID]);
		if ( *val < recs[i][POS_V])
		     *val = recs[i][POS_V];
	}
	return 0;
}

static void CalcRecordId(
	const MAP *out) /* file to use for co-ordinate
                           * calculation
                           */
{
  size_t 	i = 0;
  int   row,col,nrCols = (int)RgetNrCols(out);

  for(i = 0; i < nrRecords; i++)
  {
    if (AppRgetRowCol(out, recList[i][POS_X], recList[i][POS_Y], &row, &col)) {
      /* coordinate is in the map, give linear ID */
      long int cell_id = (long int)row * (long int)nrCols + (long int)col;
      recList[i][POS_ID] = cell_id;
    }
    else {
      recList[i][POS_ID] = ID_NOT_IN_MAP;
    }
  }
}

static CALC_CELL Method(COMP_CELL compCell)
{
	switch (compCell) {
		case	AVERAGE:	return CalcAverage;
		case	DIR_AVERAGE:	return CalcDirection;
		case	HIGHEST:	return CalcMax;
		case	MAJORITY:	return CalcMajority;
		case	LOWEST:		return CalcMin;
		case 	MINORITY:	return CalcMinority;
		case 	TOTAL   :	return CalcTotal;
		default      : POSTCOND(FALSE);
				return NULL;
	}
}

/* Converts a column input file to a csf file.
 * Converts a column input file to a csf file.
 * Returns 1 in case of an error, 0 otherwise
 */
int Col2Map(
	      MAP *out,		/* output file */
	const char *inputFile,	/* input file */
	COMP_CELL  compCell,  /* calculation methode */
	const char *mv,       /* missing value */
	const size_t  *colNr,     /* the column nrs */
	int       sepChar)   /* separator character */
{
	size_t r,c,nrRows = RgetNrRows(out);
	size_t nrCols = RgetNrCols(out);
	size_t  ri; /* record index */
	const char *conflictType;
	REAL8 *buf = NULL;
	size_t nrMvPixels = 0, nrMultPixels = 0;
	size_t nrRecordsRead, nrMVvalueColumn,  nrMVcoordColumn;
	BOOL geoeas;           /* Geo-eas  Y/N */
	CALC_CELL calcMultPixel = Method(compCell);
	REAL8 **orgRecList;

	nrCountConflict=0;

	if(
	 AppReadColumnFile(&orgRecList, &nrRecords, &nrRecordsRead, &nrMVvalueColumn, &nrMVcoordColumn,
	          &geoeas, inputFile, mv,
		  RgetValueScale(out),
		  RgetCellRepr(out),
	          colNr, sepChar, TRUE)) 
	{
		Error("while reading '%s'",inputFile);
		goto error1;
	}

	if (LimitedVersionCheck((int)nrRows,(int)nrCols,-1,-1,(int)nrRecordsRead,-1))
		goto error1;
	if ((recList = (double **)CHK_MALLOC_TYPE(double *, nrRecords)) == NULL)
		goto error1;
	for(ri = 0; ri < nrRecords; ri++)
		recList[ri] = orgRecList[ri];

	AppVerbose("nr. of records read: %u\n", nrRecordsRead);
	AppVerbose("nr. of records with mv value: %u\n", nrMVvalueColumn);
	AppVerbose("nr. of records with mv (x,y): %u\n", nrMVcoordColumn);

	CalcRecordId(out); 

	/* Sort records on cell ID, ID outside map = -1 */
 	qsort(recList, (size_t) nrRecords, sizeof(REAL8*), (QSORT_CMP) CmpId);

	/* skip over records outside map */
	ri = 0;
	while (ri < nrRecords && recList[ri][POS_ID] == ID_NOT_IN_MAP)
		ri++;
	AppVerbose("nr. of records outside map: %u\n", ri);

	if ( (buf = (REAL8 *)Rmalloc(out, nrCols)) == NULL)
		goto error2;

	for (r = 0; r < nrRows; r++)
	{
	 for (c = 0; c < nrCols; c++)
	 {
	        double l = (r * nrCols) + c;
		size_t    n=0;
		while ( (ri+n) < nrRecords && recList[ri+n][POS_ID] == l) 
		     n++;
		switch(n) {
		 case 0: SET_MV_REAL8(buf+c);
			 nrMvPixels++;
		         break;
		 case 1: buf[c] = recList[ri][POS_V];
		         break;
		 default:
		         if (calcMultPixel(buf+c, recList+ri, n))
		           goto error2;
		         nrMultPixels += n - 1;
	 	}
	 	ri += n;
	 }
	 RputRow(out, r, buf);
	}
	POSTCOND(ri == nrRecords); /* all processed */

	AppVerbose("nr. of cells with mv: %u\n", nrMvPixels);
	AppVerbose("nr. of cells with more than one record: %u\n", 
			nrMultPixels);

	switch(compCell) {
		case MAJORITY: conflictType = "majority"; break;
		case MINORITY: conflictType = "minority"; break;
		case DIR_AVERAGE: conflictType = "direction"; break;
		default: conflictType = NULL;
	}
	if (conflictType != NULL)
	 AppVerbose("nr. of cells with %s conflict: %u\n", 
	 		conflictType, nrCountConflict);

	AppFreeColumnData(orgRecList, nrRecords);

	Free(buf);
	Free(recList);
	nrRecords = 0;
	
	return 0;

error2:
	AppFreeColumnData(orgRecList, nrRecords);
error1:
	Free(buf);
	Free(recList);
	nrRecords = 0;
	return 1;
}
