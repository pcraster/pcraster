#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
# include "p_calc_list.h"

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

static int IdentifyFromPit(
	MAP_INT4 *out, 
	int r, 
	int c, 
	const MAP_UINT1 *ldd, 
	const MAP_INT4  *points) 
{
	NODE 	*list = LinkChkNd(NULL, r, c); /* current search tree */
	INT4    val;
	if(list == NULL)
		return -1; /* allocation failed */

	/* init pit
	 * copy id or 0 to out map
	 * and set to 0 if points is MV
	 */
	if (!points->Get(&val, r,c, points))
		val = 0;
	out->Put(val, r,c, out);

	while(list != NULL)
	{   
		int rDS, cDS;
		UINT1 l;
		INT4  id, idDS;
		r = list->rowNr;
		c = list->colNr;
		ldd->Get(&l,r,c,ldd);
		POSTCOND(l != MV_UINT1);
		rDS = DownStrR(r,l);
		cDS = DownStrC(c,l);
		out->Get(&idDS, rDS, cDS, out);
		POSTCOND(idDS != MV_INT4);
		if (idDS != 0 )
		     out->Put(idDS,r,c,out);
		else {
		       if (points->Get(&id, r, c, points))
		     	out->Put(id,r,c,out);
		       else
		        out->Put(0,r,c,out);
		     }
		if (ReplaceFirstByUpsNbs(&list,ldd))
			return 1;
	}
	return 0;
}


/* implementation of catchment function
 * Works allright even if out and points are the same map, PitRem() depends
 * on this feature when calling Catch().
 * In case of an unsound ldd, some cells on out are not initialized
 * Returns 0 if termination is successful, non-zero otherwise 
 */
int Catch(
     MAP_INT4 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd, 		/* ldd map	*/
     const MAP_INT4 *points) 		/* points map	*/
{
     	UINT1 	lddVal;
     	int 	r, c , nrRows, nrCols;

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	PRECOND(nrRows == points->NrRows(points));
	PRECOND(nrCols == points->NrCols(points));

	/* algorithm wants ldd->Get() to return FALSE if value is MV */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	points->SetGetTest(GET_MV_TEST, points);
	out->SetGetTest(GET_MV_TEST, out);


	/* For every pit in the ldd map do the function  
	 * for every cell in the catchment
	 */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{
		if(ldd->Get(&lddVal, r, c, ldd))
		{
		 if (lddVal == LDD_PIT)	/* start from each pit */
			if ( IdentifyFromPit(out, r, c, ldd, points) )
				return 1;
	        }
	        else
	        	out->PutMV(r,c,out);
	}
	return 0;			/* successful terminated */ 
}

static int SubIdentifyFromPit(
	MAP_INT4 *out, 
	int r, 
	int c, 
	const MAP_UINT1 *ldd, 
	const MAP_INT4  *points) 
{
	NODE 	*list = LinkChkNd(NULL, r, c); /* current search tree */
	INT4    val;
	if(list == NULL)
		return -1; /* allocation failed */

	/* init pit
	 * copy id or 0 to out map
	 * and set to 0 if points is MV
	 */
	if (!points->Get(&val, r,c, points))
		val = 0;
	out->Put(val, r,c, out);

	while(list != NULL)
	{   
		int rDS, cDS;
		UINT1 l;
		INT4  id, idDS;
		r = list->rowNr;
		c = list->colNr;
		ldd->Get(&l,r,c,ldd);
		POSTCOND(l != MV_UINT1);
		rDS = DownStrR(r,l);
		cDS = DownStrC(c,l);
		points->Get(&id, r, c, points);
		if ( id != MV_INT4 && id != 0  )
		     out->Put(id,r,c,out);
		else 
		{
			/* propagate dowstream id */
			out->Get(&idDS, rDS, cDS, out);
			POSTCOND(idDS != MV_INT4);
			out->Put(idDS,r,c,out);
		}
		if (ReplaceFirstByUpsNbs(&list,ldd))
			return 1;
	}
	return 0;
}

/* implementation of subcatchment function
 * In case of an unsound ldd, some cells on out are not initialized
 * Returns 0 if termination is successful, non-zero otherwise 
 */
int SubCatchment(
     MAP_INT4 *out,			/* write-only output map  */ 
     const MAP_UINT1 *ldd, 		/* ldd map	*/
     const MAP_INT4 *points) 		/* points map	*/
{
     	UINT1 	lddVal;
     	int 	r, c , nrRows, nrCols;

     	nrRows = ldd->NrRows(ldd);
     	nrCols = ldd->NrCols(ldd);

	PRECOND(nrRows == points->NrRows(points));
	PRECOND(nrCols == points->NrCols(points));

	/* algorithm wants ldd->Get() to return FALSE if value is MV */
	ldd->SetGetTest(GET_MV_TEST, ldd);
	points->SetGetTest(GET_MV_TEST, points);
	out->SetGetTest(GET_MV_TEST, out);


	/* For every pit in the ldd map do the function  
	 * for every cell in the catchment
	 */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{
		if(ldd->Get(&lddVal, r, c, ldd))
		{
		 if (lddVal == LDD_PIT)	/* start from each pit */
			if ( SubIdentifyFromPit(out, r, c, ldd, points) )
				return 1;
	        }
	        else
	        	out->PutMV(r,c,out);
	}
	return 0;			/* successful terminated */ 
}
