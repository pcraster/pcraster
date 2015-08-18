#include "stddefx.h" 


/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "misc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "calc.h"

/* Adapted from Sedgewick, Algorithms */

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/
/* Determines row number according to index i and nr. of columns in map.
 * Returns the row number.
 */
 static int DetRow(	int nrCols,		/* number of columns */
 			int i)			/* index in map */
 {
	int row = (int) (i / nrCols);
	return row;
 }

/* Determines column number according to index i and nr. of rows in map.
 * Returns the column number.
 */
 static int DetCol(	int nrCols,		/* number of columns */
 			INT4 i)			/* index in map */
 {
	int column = (int) (i % nrCols);
	return column;
 }

/* Gets value of REAL8 map at location i.
 * Returns the REAL8 value.
 */
 static REAL8 GetREAL8(	const MAP_REAL8 *map,	/* map  to read */
 			INT4 i)			/* index in map */
 {
 	REAL8 	value;				/* value to read */
 	int	nrCols = map->NrCols(map);
 	int	r = DetRow(nrCols, i);
 	int	c = DetCol(nrCols, i);
# ifdef DEBUG
 	int	nrRows = map->NrRows(map);
 	PRECOND(0 <= r && r < nrRows);
 	PRECOND(0 <= c && c < nrCols);
# endif

 	map->Get(&value, r, c, map);
 	PRECOND(! IS_MV_REAL4(&value));
 	return value;
 }

/* Gets value of INT4 map at location i.
 * returns the INT4 value.
 */
 static INT4 GetINT4(	const MAP_INT4 *map,	/* map to read */ 
 			INT4 i)			/* location */
 {
 	INT4 	value;				/* value to read */
 	int	nrCols = map->NrCols(map);
 	int	r = DetRow(nrCols, i);
 	int	c = DetCol(nrCols, i);
# ifdef DEBUG
 	int	nrRows = map->NrRows(map);
 	PRECOND(0 <= r && r < nrRows);
 	PRECOND(0 <= c && c < nrCols);
# endif

 	map->Get(&value, r, c, map);
 	return value;
 }

static REAL8 Gett(
	const MAP_INT4  *tmp,
	const MAP_REAL8 *in, 
	INT4 j)
{
	INT4 i = GetINT4(tmp,j);
	PRECOND(i >= 0 && i < (tmp->NrRows(tmp) * tmp->NrCols(tmp)));
	return GetREAL8(in, i);
}

static void 	PutINT4(
	INT4 val,
	INT4 linPos,
	MAP_INT4 *map)
{
 	int	nrCols = map->NrCols(map);
 	int	r = DetRow(nrCols, (int) linPos);
 	int	c = DetCol(nrCols, (int) linPos);
# ifdef DEBUG
 	int	nrRows = map->NrRows(map);
 	PRECOND(0 <= r && r < nrRows);
 	PRECOND(0 <= c && c < nrCols);
# endif

 	map->Put(val, r, c, map);
}

static void Exchange(
	MAP_INT4  *tmp,
	INT4 i,
	INT4 j)
{
	INT4 iv = GetINT4(tmp,i);
	INT4 jv = GetINT4(tmp,j);
	PRECOND(i >= 0  && i < (tmp->NrRows(tmp) * tmp->NrCols(tmp)));
	PRECOND(j >= 0  && j < (tmp->NrRows(tmp) * tmp->NrCols(tmp)));
	PRECOND(iv >= 0 && iv < (tmp->NrRows(tmp) * tmp->NrCols(tmp)));
	PRECOND(jv >= 0 && jv < (tmp->NrRows(tmp) * tmp->NrCols(tmp)));
	PutINT4(iv,j,tmp);
	PutINT4(jv,i,tmp);
}

static INT4 Partition(
		MAP_INT4 *tmp,		/* read-write index map */
		const MAP_REAL8 *in,	/* input map */
		INT4 left,		/* first element */
		INT4 right)		/* last element */
{		
	INT4 	i = left;
	INT4    j = right;
	REAL8   pe;

	/* choose middle element as partition element 
	 */
	Exchange(tmp, (i+j)/2, j);

	pe = Gett(tmp, in, j); /* move pivot to right end */

	while( i < j)
	{
		while( i < j && Gett(tmp,in,i) <= pe) i++;
		while( i < j && Gett(tmp,in,j) >= pe) j--;
		if ( i < j ) Exchange(tmp, i, j);
	}
	if ( i != right )
		Exchange(tmp, i, right); /* pe to partition location */

	return i;	/* return index of partition element */
}

static int QuickSort(
     INT4 partToSort,
     INT4 nrCells,
     const MAP_REAL8 *in, 		/* Read-write input map	*/
     MAP_INT4 *tmp)		/* read-write index temporary map */
{
	int left = partToSort;
	int right = nrCells -1;
	int i, pointer=2;
	int *stack=NULL;	/*  contains partitions to do */
	/* Perform quicksort on the tmp map and to modify the output map
	 * later. 
	 */
	do {
		if(right > left)
		{
			i = Partition(tmp, in, left, right);
			if (ChkReallocFree((void **)&stack, 
					(pointer+2)*sizeof(INT4)))
				return 1;


			if(i - left > right - i)
			{
				stack[pointer] = left;
				stack[pointer + 1] = i - 1;
				left = i + 1;
			}
			else
			{
				stack[pointer] = i + 1;
				stack[pointer + 1] = right;
				right = i - 1;
			}
			pointer += 2;
		}
		else
		{
			pointer -= 2;		/* other partition */
			POSTCOND(pointer >= 0);
			
			left = stack[pointer];
			right = stack[pointer + 1];
		}
	} while (pointer != 0);
	PRECOND(stack != NULL);
	Free(stack);
	return 0;
}


/* Gives unique numbers to values in the input map in ascending order.
 * The quicksort algorithm is used to determine the order of the
 * numbers in the output map.
 * quicksort algoritm copied from Paul S. Wang's An Introduction to
 * ANSI C on Unix, partitioning is written by Matthuschka adapted
 * from Sedgewick.
 * Returns 0 if termination is successful, 1 otherwise.
 */
int Order(
     MAP_REAL8 *out,		/* Read-write output map  */ 
     const MAP_REAL8 *in, 		/* Read-write input map	*/
     MAP_INT4 *tmp)		/* read-write index temporary map */
{
	REAL8 	inputVal, prevVal;
	int 	i, r, c, nrRows, nrCols, nrCells;
	INT4 	partToSort,id;
	int nrRuns = 1;


	nrRows = in->NrRows(in);
	nrCols = in->NrCols(in);
	nrCells = nrRows * nrCols;

	/* algorithm wants in->Get() to return FALSE if a value is a 
	 * missing value and for the other maps just the same. 
	 */
	in->SetGetTest(GET_MV_TEST, in);
	out->SetGetTest(GET_MV_TEST, out);
	tmp->SetGetTest(GET_MV_TEST, tmp);

	/* Initialize the outBuf with ascending numbers.  
	 * Since we already need this first sweep to get
	 * indirection index map (tmp) we can also analyse
	 * in this sweep what sort algoritm is the best
	 */
	id = 0;		/* id is linearized index 
	                 * for maps
	                 */
	partToSort = nrCells;
	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	{
		PRECOND(id == (r*nrCols + c));

		if(in->Get(&inputVal, r, c, in))
		{
			if (partToSort != nrCells) {
			 /* above assures prevVal is
			  * already initialized
			  */
                         if (inputVal < prevVal)
                         	nrRuns++;
                        } 	
                        prevVal = inputVal;
			PutINT4(id, --partToSort, tmp);
		}
		else
			out->PutMV(r, c, out);
		id++;		/* Calculate next id */
	}

	if (partToSort == nrCells) 
	    /* ALL MV, which are already written in above loop: ready */
		return 0;

	if(nrRuns == 1) {
	  /* if nrRuns == 1 then everything is sorted, 
	   * it is one increasing run 
	   */
	   id = 1;
	   for (r = 0; r < nrRows; r++)
	    for (c = 0; c < nrCols; c++)
		if(in->Get(&inputVal, r, c, in))
			out->Put(id++,r, c, out);
	   return 0;		
	}
	
	if (QuickSort( partToSort, nrCells, in, tmp))
		return 1;

	for(i = partToSort; i < nrCells; i++)
	{	/* result on location out[tmp[i]]  = i */
		INT4 tmpIn;
		tmpIn = GetINT4(tmp, i);
		r = DetRow(nrCols, tmpIn);
		c = DetCol(nrCols, tmpIn);
		out->Put((REAL8)(i-partToSort+1), r, c, out);
#	 ifdef NEVER		
/* for some reason it simply does not hold!
 * AFTER adding the action when we have one run
 * I did not test is again, still need to write
 * a good test suite for this sucker
 */
#	 ifdef DEBUG_DEVELOP
		{       
		static REAL8 prevValDebug;
	        REAL8 testVal;
		PRECOND(in->Get(&testVal, r, c, in));
		if (i != partToSort)
			POSTCOND(prevValDebug <= testVal);
		prevValDebug = testVal;
		}
#	 endif
#	 endif
	}
	return 0;
}
