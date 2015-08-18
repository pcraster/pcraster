#include "stddefx.h" 

/*
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

#include "calctypes.h"
/* global header (opt.) and dunfunc's prototypes "" */
#include <string.h> /* memset */
#include "mathx.h" 
#include "misc.h"  /* random numbers */
#include "csftypes.h" 
#include "calc.h" 
#include "app.h"
#include "dunfunc.h"


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

#ifndef ZERO_UINT1
#define ZERO_UINT1 ((UINT1)0)
#endif

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

void Do_4_2_b(UINT1 *r, const INT4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_INT4)
	  r[i] = MV_UINT1;
	 else
	  r[i] = v[i] != 0;
}

/* the ?to? are only used if an expr has multiple
 * types, therefor they msut fall in the range
 */
void Do_4to1(UINT1 *r, const INT4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_INT4)
	  r[i] = MV_UINT1;
	 else
	 {
	  PRECOND(0 <= v[i] && v[i] <= (INT4)UINT1_MAX);
	  r[i] = (UINT1)v[i];
	 }
}

void Do_sto1(UINT1 *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (IS_MV_REAL4(v+i))
	  r[i] = MV_UINT1;
	 else
	 {
	  PRECOND(0 <= v[i] && v[i] <= UINT1_MAX);
	  r[i] = v[i];
	 }
}

void Do_sto4(INT4 *r, const REAL4 *v, size_t n)
{
#ifdef DEBUG
	size_t i;
	for(i=0;i<n; i++)
	 if (!IS_MV_REAL4(v+i))
	  PRECOND(INT4_MIN <= v[i] && v[i] <= INT4_MAX);
#endif
	Do_s_2_4(r, v, n);
}

void Do_s_2_b(UINT1 *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (IS_MV_REAL4(v+i))
	  r[i] = MV_UINT1;
	 else
	  r[i] = v[i] != 0;
}

void Do_1_2_4(INT4  *r, const UINT1 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_UINT1)
	  r[i] = MV_INT4;
	 else
	  r[i] = v[i];
}

void Do_s_2_4(INT4  *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (IS_MV_REAL4(v+i))
	  r[i] = MV_INT4;
	 else
	  r[i] = MAX( ((INT4)v[i]), INT4_MIN);
}

void Do_1_2_s(REAL4 *r, const UINT1 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_UINT1)
	  SET_MV_REAL4(r+i);
	 else
	  r[i] = v[i];
}

void Do_4_2_s(REAL4 *r, const INT4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_INT4)
	  SET_MV_REAL4(r+i);
	 else
	  r[i] = v[i];
}

void Do_l_2_d(REAL4 *r, const UINT1 *v, size_t n)
{
	const double ldd2Dir[10] = { 0, 0.625, 0.500, 0.375,
	                             0.750, -1   , 0.250,
	                             0.875, 0    , 0.125 };
	size_t i;
	for(i=0;i<n; i++)
	 switch(v[i]) {
	  case MV_UINT1: SET_MV_REAL4(r+i); break;
	  case LDD_PIT : r[i] = -1; break;
	  default      : PRECOND(v[i] > ZERO_UINT1 && v[i] < ((UINT1)10) );
	                 r[i] = ldd2Dir[v[i]] * M_2PI;
          }
}

void Do_1_2_d(REAL4 *r, const UINT1 *v, size_t n)
{
	size_t i;
	double (* f)(double x) = 
	 (appDirection == APP_RADIANS) ? ScaleRad : Deg2Rad;
	for(i=0;i < n; i++)
	 if (v[i] != MV_UINT1)
	  r[i] = (REAL4)f((double)v[i]); 
	 else
	  SET_MV_REAL4(r+i);
}

void Do_4_2_d(REAL4 *r, const INT4 *v, size_t n)
{
	size_t i;
	double (* f)(double x) = 
	 (appDirection == APP_RADIANS) ? ScaleRad : Deg2Rad;
	for(i=0;i < n; i++)
	 if (v[i] != MV_INT4)
	  r[i] = (REAL4)f((double)v[i]); 
	 else
	  SET_MV_REAL4(r+i);
}

void Do_4_2_l(UINT1 *r, const INT4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (v[i] == MV_INT4)
	  r[i] = MV_UINT1;
	 else
	 {
	  r[i] = (UINT1)(ABS(v[i]) % 10);
	  if (r[i] == 0)
	  	r[i] = MV_UINT1;
	 }
	 /* MISSING CODE: DO A REPAIR */
}

void Do_s_2_l(UINT1 *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0;i<n; i++)
	 if (IS_MV_REAL4(v+i))
	  r[i] = MV_UINT1;
	 else
	 {
	  r[i] = (UINT1)(ABS(((INT4)v[i])) % 10);
	  if (r[i] == 0)
	  	r[i] = MV_UINT1;
	 }
	 /* MISSING CODE: DO A REPAIR */
}

void Do_d_2_l(UINT1 *r, const REAL4 *v, size_t n)
{
        const UINT1 lookup[8] = { 8, 9, 6, 3, 2, 1, 4, 7 }; 
	double dum,shift = 1.0/16.0; /* ((pi/8)/2pi) = 1/16 */
	size_t i;
	for(i=0;i<n; i++)
	 if (IS_MV_REAL4(v+i))
	  r[i] = MV_UINT1;
	 else
	 {
	  double d = v[i];
	  if (d == -1)
	  	r[i] = LDD_PIT;
	  else
	 {
	  /* shift a halfdir - eps. (0.0624999) */
	   r[i] =  (UINT1)( modf( (d/M_2PI)+shift, &dum)*8);
	   POSTCOND(r[i] < ((UINT1)8) );
	   r[i] = lookup[r[i]];
	 }
	}
	 /* MISSING CODE: DO A REPAIR */
}


int Do_1_2_b(UINT1 *v, size_t n)
{
	size_t i;
	for(i=0;i < n; i++)
		if (v[i] != MV_UINT1)
			v[i] = (v[i] != 0);
	return 0;
}


int Do_d_2_s(REAL4 *v, size_t n)
{
	size_t i;
	if (appDirection == APP_DEGREES)
	{
	 for(i=0;i < n; i++)
	  if ( ! ( (IS_MV_REAL4(v+i)) || v[i] == -1) )
	   v[i] = (REAL4)Rad2Deg(v[i]); 
	}
	return 0;
}

void Do_downstreamdist(REAL4 *r, const UINT1 *v, size_t n)
{
	size_t i;
	double d[2];
	d[0] = Side();
	d[1] = d[0]*sqrt(2.0);
	for(i=0;i < n; i++)
	 switch(v[i]) {
		case MV_UINT1: SET_MV_REAL4(r+i); break;
		case LDD_PIT : r[i] = 0; break;
		default      : r[i] = d[((int)v[i])%2]; break;
	 }
}

void Do_pit(
	INT4 *r,
	const UINT1 *val,
	size_t n)
{
	size_t i;
	INT4   p=1;

	for(i=0; i < n; i++)
	 switch(val[i]) {
		case MV_UINT1: r[i] = MV_INT4; break;
		case LDD_PIT : r[i] = p++; break;
		default      : r[i] = 0; break;
	 }
}

void Do_maptotal(REAL4 *r, const REAL4 *v, size_t n)
{
	size_t i;
	double sum=(double)0;
	for(i=0; i< n; i++)
		if (! IS_MV_REAL4(v+i))
			sum += v[i];
	*r = (REAL4)sum;
}

void Do_nodirection(UINT1 *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0; i< n; i++)
		if (! IS_MV_REAL4(v+i))
		   r[i] = (v[i] == -1);
		else
		   r[i] = MV_UINT1;
}

void Do_mapminimum_4(INT4 *r, const INT4 *v, size_t n)
{
	size_t i;
	INT4 min = INT4_MAX;
	for(i=0; i< n; i++)
	 if (v[i] != MV_INT4)
	 	min = MIN(min,v[i]);
	*r = min;
}

void Do_mapminimum_s(REAL4 *r, const REAL4 *v, size_t n)
{
	size_t i;
	double min = REAL4_MAX;
	for(i=0; i< n; i++)
	 if (! IS_MV_REAL4(v+i))
		min = MIN(min,v[i]);
	*r = (REAL4)min;
}

void Do_mapmaximum_4(INT4 *r, const INT4 *v, size_t n)
{
	size_t i;
	INT4 max = INT4_MIN;
	for(i=0; i< n; i++)
	 if (v[i] != MV_INT4)
	 	max = MAX(max,v[i]);
	*r = max;
}

void Do_mapmaximum_s(REAL4 *r, const REAL4 *v, size_t n)
{
	size_t i;
	double max = -REAL4_MAX;
	for(i=0; i< n; i++)
	 if (! IS_MV_REAL4(v+i))
		max = MAX(max,v[i]);
	*r = (REAL4)max;
}

void Do_maparea_1(REAL4 *r, const UINT1 *v, size_t n)
{
	size_t i;
	double area = (double)0, p = Area();
	for(i=0; i< n; i++)
		if (v[i] != MV_UINT1)
			area += p;
	*r = (REAL4)area;
}

void Do_maparea_4(REAL4 *r, const INT4 *v, size_t n)
{
	size_t i;
	double area = (double)0, p = Area();
	for(i=0; i< n; i++)
		if (v[i] != MV_INT4)
			area += p;
	*r = (REAL4)area;
}

void Do_maparea_s(REAL4 *r, const REAL4 *v, size_t n)
{
	size_t i;
	double area = (double)0, p = Area();
	for(i=0; i< n; i++)
	 if (! IS_MV_REAL4(v+i))
		area += p;
	*r = (REAL4)area;
}

int Do_defined_1(UINT1 *v, size_t n)
{
	size_t i;
	for(i=0; i< n; i++)
		v[i] = v[i] != MV_UINT1;
	return 0;
}

void Do_defined_4(UINT1 *r, const INT4 *v, size_t n)
{
	size_t i;
	for(i=0; i< n; i++)
		r[i] = v[i] != MV_INT4;
}

void Do_defined_s(UINT1 *r, const REAL4 *v, size_t n)
{
	size_t i;
	for(i=0; i< n; i++)
		r[i] = !IS_MV_REAL4(v+i);
}


void Do_normal(REAL4 *values, const UINT1 *b, size_t n)
{
	size_t i;
	for(i=0; i < n ; i++)
	 if (b[i] == 1)
		values[i] = (REAL4)GasDev();
	 else
		SET_MV_REAL4(values+i);
}

void Do_uniform(REAL4 *values, const UINT1 *b, size_t n)
{
	size_t i;
	for(i=0; i < n ; i++)
	 if (b[i] == 1)
		values[i] = (REAL4)Ran();
	 else
		SET_MV_REAL4(values+i);
}


void Do_normal_1(REAL4 *values, const UINT1 *b, size_t n)
{
	size_t i;
	for(i=0; i < n ; i++)
	 if (b[0] == 1)
		values[i] = (REAL4)GasDev();
	 else
		SET_MV_REAL4(values+i);
}

void Do_uniform_1(REAL4 *values, const UINT1 *b, size_t n)
{
	size_t i;
	for(i=0; i < n ; i++)
	 if (b[0] == 1)
		values[i] = (REAL4)Ran();
	 else
		SET_MV_REAL4(values+i);
}

int Do_spatial_1(UINT1 *vL, UINT1 *vR, size_t n)
{
	memset(vL, *vR, n);
	return 0;
}

int Do_spatial_4(INT4 *vL, INT4 *vR, size_t n)
{
	size_t i;
	INT4 val = *vR;
	for(i=0; i< n; i++)
		vL[i] = val;
	return 0;
}

int Do_spatial_s(UINT4 *vL, UINT4 *vR, size_t n)
{
	size_t i;
	REAL4 val = *vR;
	for(i=0; i< n; i++)
		vL[i] = val;
	return 0;
}
