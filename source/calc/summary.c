#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <ctype.h>
#include "misc.h"
#include "calc.h"
#include "app.h"	/* AppProgress, APP_PROGRESS, appOutput */
#include "mathx.h"	/* pow, sqrt */

/* global header (opt.) and test's prototypes "" */

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

/******************/
/* IMPLEMENTATION */
/******************/
/* Calculate the first two moments of a directional data sample
 * Input and result are in radians. The sample size must be larger
 * than one.
 */
static void DirectionalStatistics(
	long double *mean, 	/* write-only, mean */
	long double *sd,	/* write-only, standard deviation */
	const long double *samples, /* array of n samples, radians */
	int n)			/* sample size */
{
	long double tC, tD, tS;
	long double meanIn, D;
	long double R;
	int i;
	const long double *p;
	
	PRECOND(n > 1);

	tC = tS = tD = 0;

	for(i = 0, p = samples; i < n; p++, i++)
	{
		tC  += cos(*p);
		tS  += sin(*p);
	}
	tC /= n;
	tS /= n;
	R = sqrt(tC * tC + tS * tS);
	meanIn = atan2(tS / R, tC / R);

	for(i = 0, p = samples; i < n; p++, i++)
		tD += cos(*p - meanIn);

	D = 1- tD / n;
	*mean = meanIn;
	*sd = sqrt(-2 * log(1 - D));
}

/*! Determines the statistics of a classified map on given time step.
 * Returns 1 in case of failed allocation, 0 otherwise.
 * \todo TODO if it is classified there then expr should be MAP_INT4
 */
static int ClassSummary(
	TIME_TABLE *t,		/* read-write table to add to */
	const MAP_REAL8 *expr,	/* map of values to read */
	int currTimeStep,	/* row index in time table */
	int nrRows,		/* number of rows of expression map */
	int nrCols)		/* nr. of columns of expression map */
{
	REAL8 majority, min = REAL8_MAX, max = -REAL8_MAX;
	int i, r, c, n = 0, *score, maxScore = 0;

	PRECOND(expr->GetGetTest(expr) == GET_MV_TEST);

	/* scan map for minimum and maximum value */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
	 	REAL8 val;
	 	if(expr->Get(&val, r, c, expr))
	 	{
	 		n++;
			if(val < min)
				min = val;
			if(val > max)
				max = val;
		}
	}

	/* Consider special case */
	if(n == 0)
	{
		t->vals[currTimeStep][0] = n;
		SET_MV_REAL8(t->vals[currTimeStep] + 1);
		SET_MV_REAL8(t->vals[currTimeStep] + 2);
		SET_MV_REAL8(t->vals[currTimeStep] + 3);
		return 0;
	}

	/* determine the majority */
	if((score = ChkMalloc(sizeof(REAL8) * (int) ((max - min) + 1)))
	   == NULL)
		return 1;
	for(i = (int)min; i <= (int)max; i++)
		score[i - (int) min] = 0;

	/* Scan map for majority */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
	 	REAL8 val;
	 	if(expr->Get(&val, r, c, expr))
		{
			int index = (int) (val - min);
			score[index]++;
			if(score[index] > maxScore)
				maxScore = score[index];
		}
	}

	/* Search for minimum value with highest score */
	c = (int)min;
	while(score[c - (int) min] != maxScore)
		c++;
	majority = c;
			
	/* Write characteristics to table */
	t->vals[currTimeStep][0] = n;
	t->vals[currTimeStep][1] = majority;
	t->vals[currTimeStep][2] = min;
	t->vals[currTimeStep][3] = max;
	free(score);
	return 0;
}

/* Determines the statistics of a boolean map */
static void BooleanSummary(
	TIME_TABLE *t,		/* read-write table to add to */
	const MAP_REAL8 *expr,	/* map of values to read */
	int currTimeStep,	/* row index in time table */
	int nrRows,		/* number of rows of expression map */
	int nrCols)		/* nr. of columns of expression map */
{
	int	r, c, n = 0, nrFalse = 0, nrTrue = 0;
	REAL8 	val;

	PRECOND(expr->GetGetTest(expr) == GET_MV_TEST);

	/* Scan map for statistics */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	 {
	 	if(expr->Get(&val, r, c, expr))
	 	{
	 		n++;
	 		if(val == 0)
	 			nrFalse++;
	 		else
	 			nrTrue++;
	 	}
	 }
	t->vals[currTimeStep][0] = n;
	t->vals[currTimeStep][1] = nrFalse;
	t->vals[currTimeStep][2] = nrTrue;
	return;
}

/* Determines the statistics of a scalar map */
static void ScalarSummary(
	TIME_TABLE *t,		/* read-write table to add to */
	const MAP_REAL8 *expr,	/* map of values to read */
	int currTimeStep,	/* row index in time table */
	int nrRows,		/* number of rows of expression map */
	int nrCols)		/* nr. of columns of expression map */
{
	REAL8	sd, val, mean, min, max, sum;
	int	n, r, c;
	
	/* Initialize settings */
	min = REAL8_MAX;
	max = -REAL8_MAX;
	sum = 0;
	n = 0;

	PRECOND(expr->GetGetTest(expr) == GET_MV_TEST);

	/* Scan map for mean, min, max and n */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{	/* consider only non-MV pixels */
		if(expr->Get(&val, r, c, expr))
		{	
			n++;	
			if(val > max)
				max = val;
			if(val < min)
				min = val;
			sum += val;
		}
	}

	/* scan map for standard deviation */
	if(n == 1 || n == 0)
	{
		mean = sum;
		sd = 0;
	}
	else
	{
		mean = sum / (REAL8) n;
		sum = 0;
		for(r = 0; r < nrRows; r++)
		 for(c = 0; c < nrCols; c++)
		{
			if(expr->Get(&val, r, c, expr))
			    sum += pow((val - mean), (double) 2) / n;
			    			/*	(n - 1); */
		}
		sd = sqrt(sum);
	}

	/* Put characteristis in table */
	t->vals[currTimeStep][0] = n;

	if(n != 0)
	{
		t->vals[currTimeStep][1] = mean;
		t->vals[currTimeStep][2] = sd;
		t->vals[currTimeStep][3] = min;
		t->vals[currTimeStep][4] = max;
	}
	else
	{
		SET_MV_REAL8(t->vals[currTimeStep] + 1);
		SET_MV_REAL8(t->vals[currTimeStep] + 2);
		SET_MV_REAL8(t->vals[currTimeStep] + 3);
		SET_MV_REAL8(t->vals[currTimeStep] + 4);
	}
	return;
}

/* Determines the statistics of a directional map.
 * Returns 1 in case of an allocation error, 0 otherwise.
 */
static int DirectionSummary(
	TIME_TABLE *t,		/* read-write table to add to */
	const MAP_REAL8 *expr,	/* map of values to read */
	int currTimeStep,	/* row index in time table */
	int nrRows,		/* number of rows of expression map */
	int nrCols)		/* nr. of columns of expression map */
{
	int		i, n = 0, r, c;
	long double 	mean, sd, *vals;
	REAL8		val, min = REAL8_MAX, max = -REAL8_MAX;
	
	PRECOND(expr->GetGetTest(expr) == GET_MV_TEST);

	/* Scan map for min, max and n */
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{	/* consider only non-MV pixels */
		if(expr->Get(&val, r, c, expr))
		{	
			n++;	
			if(val > max)
				max = val;
			if(val < min)
				min = val;
		}
	}

	/* Consider special case of n = 0 */
	if(n == 0)	
	{
		t->vals[currTimeStep][0] = n;
		SET_MV_REAL8(t->vals[currTimeStep] + 1);
		SET_MV_REAL8(t->vals[currTimeStep] + 2);
		SET_MV_REAL8(t->vals[currTimeStep] + 3);
		SET_MV_REAL8(t->vals[currTimeStep] + 4);
		return 0;
	}

	if((vals = ChkMalloc(sizeof(long double) * n)) == NULL)
		return 1;

	/* Scan map for mean and standard deviation */
	i = 0;
	for(r = 0; r < nrRows; r++)
	 for(c = 0; c < nrCols; c++)
	{	/* consider only non-MV pixels */
		if(expr->Get(&val, r, c, expr))
		{	
			vals[i] = val;
			i++;
		}
		if(n == 1 || n == 0)
		{	/* consider special cases */
			sd = 0;
			mean = 0;
		}
		else
			DirectionalStatistics(&mean, &sd, vals, n);
	}

	/* Put characteristis in table */
	t->vals[currTimeStep][0] = n;
	t->vals[currTimeStep][1] = AppOutputDirection(mean);
	t->vals[currTimeStep][2] = AppOutputDirection(sd);
	t->vals[currTimeStep][3] = AppOutputDirection(min);
	t->vals[currTimeStep][4] = AppOutputDirection(max);
	free(vals);
	return 0;
}

/* Adds given time step to the given summary table.
 * Returns NULL in case of an error, pointer to table otherwise.
 */
TIME_TABLE *AddToSummaryTable(
	TIME_TABLE *t,		/* read-write table to add to */
	const MAP_REAL8 *expr,	/* map of values to read */
	int currTimeStep)	/* row index in time table */
{
    /* Initialize settings of expression map */
    int nrRows = expr->NrRows(expr);
    int nrCols = expr->NrCols(expr);
	
    expr->SetGetTest(GET_MV_TEST, expr);

    /* Determine which kind of summary should be made */
    switch(t->vs)
    {
	case VS_SCALAR:
	    ScalarSummary(t, expr, currTimeStep, nrRows, nrCols);
	    break;

	case VS_DIRECTION:
	    if(DirectionSummary(t, expr, currTimeStep, nrRows, nrCols))
	    {
		FreeTimeTable(t);
		return NULL;		/* allocation failed */
	    }
	    break;

	case VS_BOOLEAN:
	    BooleanSummary(t, expr, currTimeStep, nrRows, nrCols);
	    break;

	default:
	    if(ClassSummary(t, expr, currTimeStep, nrRows, nrCols))
	    {
		FreeTimeTable(t);
		return NULL;		/* allocation failed */
	    }
	    break;
    }
    return t;				/* return modified time table */
}

/* Creates a summary table.
 * Returns NULL when allocation fails, pointer to time table otherwise.
 */
TIME_TABLE *CreateSummaryTable(
	int nrTimeSteps,   /* number of time steps */
	CSF_VS vs)	   /* value scale of map in AddToSummaryTable */
{
	int		nrCols;
	TIME_TABLE 	*t;			/* new time table */

	/* determine the number of columns for time table */
	switch(vs)
	{
		case VS_DIRECTION:
		case VS_SCALAR:
			nrCols = 5;
			break;
		case VS_BOOLEAN:
			nrCols = 3;
			break;
		default:
			nrCols = 4;
			break;
	}

	/* allocate and initialize the table */
	if((t = ChkMalloc(sizeof(TIME_TABLE))) == NULL)
		return NULL;
	t->vs = vs;
	t->nrSteps = nrTimeSteps;
	t->nrCols = nrCols;
	if((t->vals = (REAL8 **)Malloc2d((size_t)t->nrSteps, 
	                     (size_t)t->nrCols, sizeof(REAL8))) == NULL)
	{
		free(t);
		return NULL;
	}
	return t;
}
