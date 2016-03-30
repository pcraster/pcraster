#include "stddefx.h" 


/********/
/* USES */
/********/

#include "calctypes.h"
/* libs ext. <>, our ""  */
#include "mathx.h" /* Ran(), GasDev() */
#include "calc.h"

/* global header (opt.) and genfunc's prototypes "" */
#include "genfunc.h" 


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/
extern int currentTime, endTime, startTime, timeStep; /* machine.cc */

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* ARGSUSED */
void Do_celllength(REAL4 *values, size_t n )
{ PRECOND(n==1); *values = (REAL4)Side(); 
  (void)n; // shut up compiler
}
/* ARGSUSED */
void Do_cellarea(REAL4 *values, size_t n)
{  PRECOND(n==1); *values = (REAL4)Side()*(REAL4)Side(); 
  (void)n; // shut up compiler
}
void Do_mapnormal(REAL4 *values, size_t n)
{
	PRECOND(n==1);
  (void)n; // shut up compiler
	*values = (REAL4)GasDev();
}
void Do_mapuniform(REAL4 *values, size_t n)
{
	PRECOND(n==1);
  (void)n; // shut up compiler
	*values = (REAL4)Ran();
}
