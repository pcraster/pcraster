#include "stddefx.h" 


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "mathx.h"	/* sqr */
#include "calc.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 
#include "delta.h"	/* Zeven* */

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

static void Stuff(
	REAL8 *D,
	REAL8 *E,
	REAL8 *F,
	REAL8 *G,
	REAL8 *H,
 const	REAL8 *Z)
 {
   REAL8 L = Side();

   *D = ((Z[4]+Z[6])/2-Z[5])/sqr(L);
   *E = ((Z[2]+Z[8])/2-Z[5])/sqr(L);
   *F = (-Z[1]+Z[3]+Z[7]-Z[9])/(4*sqr(L));
   *G = (-Z[4]+Z[6])/(2*L);
   *H = ( Z[2]-Z[8])/(2*L);
 }


/* Calculates profile curvature (See Zevenbergen)
 * Returns 0.
 */
int ProfileCurvature(
     	MAP_REAL8 *curv,       /* Read-write slope output map  */ 
     	const MAP_REAL8 *dem)	/* Digital Elevation Model */
{
	REAL8 	val;			/* value read in dem.map */
     	int 	nrRows, nrCols, r, c;
     	REAL8   Z[10],D,E,F,G,H;

     	dem->SetGetTest(GET_MV_TEST, dem);
     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	/* For every cell in the dem map calculate the curv. */
     	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	  if(dem->Get(&val, r, c, dem)) 
	  {
		ZevenbergenGrid(Z,dem,r,c);
		Stuff( &D, &E, &F, &G, &H, Z);
		if (G == 0 && H == 0)
		 val = 0;
		else
		 val = -2*( D*sqr(G) + E*sqr(H) + F*G*H)/(sqr(G)+sqr(H));
		curv->Put(val, r, c, curv);
	   }
	   else
		curv->PutMV(r, c, curv);
     	return 0;
}

/* Calculates planform curvature (See Zevenbergen)
 * Returns 0.
 */
int PlanformCurvature(
     	MAP_REAL8 *curv,       /* Read-write slope output map  */ 
     	const MAP_REAL8 *dem)	/* Digital Elevation Model */
{
	REAL8 	val;			/* value read in dem.map */
     	int 	nrRows, nrCols, r, c;
     	REAL8   Z[10],D,E,F,G,H;

     	dem->SetGetTest(GET_MV_TEST, dem);
     	nrRows = dem->NrRows(dem);
     	nrCols = dem->NrCols(dem);

	/* For every cell in the dem map calculate the curv. */
     	for (r = 0; r < nrRows; r++)
	 for (c = 0; c < nrCols; c++)
	  if(dem->Get(&val, r, c, dem)) 
	  {
		ZevenbergenGrid(Z,dem,r,c);
		Stuff( &D, &E, &F, &G, &H, Z);
		if (G == 0 && H == 0)
		 val = 0;
		else
		 val = 2*( D*sqr(H) + E*sqr(G) - F*G*H)/(sqr(G)+sqr(H));
		curv->Put(val, r, c, curv);
	   }
	   else
		curv->PutMV(r, c, curv);
     	return 0;
}
