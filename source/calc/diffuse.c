#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"
#include "calc.h"
#include "api.h"

#include "mathx.h"

static void AddToState(
     REAL8  addThis,
     int r,	
     int c,	
     MAP_REAL8 *stateOut)		/* Read-write state map */ 
{
    REAL8 addTo;
    if (stateOut->Get(&addTo,       r, c, stateOut))
	stateOut->Put(addTo+addThis,r, c, stateOut);
}

static void ReplaceOutFlux(
     REAL8  newVal,
     int r,	
     int c,	
     MAP_REAL8 *fluxOut)		/* Read-write state map */ 
{
    REAL8 dummy;
    if (fluxOut->Get(&dummy, r, c, fluxOut))
	fluxOut->Put(newVal, r, c, fluxOut);
}

/* fluxOut,stateOut = diffuseflux,diffusestate(in,dir,fluxIn) */
int  Diffuse1(
     MAP_REAL8 *stateOut,		/* Read-write state map */ 
     MAP_REAL8 *fluxOut,		/* Read-write fluxOut map */ 
     const MAP_REAL8 *dir, 		/* dir map */
     const MAP_REAL8 *in, 		/* in map */
     const MAP_REAL8 *fluxIn)		/* fluxIn map */
{
 int c,r,rows,cols;
 REAL8 inVal,fluxInVal,dirVal;

 cols = in->NrCols(in);
 rows = in->NrRows(in);

 fluxOut->PutAllMV(fluxOut);
 stateOut->PutAllMV(stateOut);
 for (c = 0; c < cols; c++)               
  for (r = 0; r < rows; r++)                  
     if (in->Get(&inVal,r,c,in)) {
        fluxOut->Put(inVal,r,c,fluxOut);
	/* state is initial equal to inVal */
        stateOut->Put(inVal,r,c,stateOut);
 }
                                             
 for (c = 0; c < cols; c++)                  
  for (r = 0; r < rows; r++)               
 {
     int dirValQuadr;
     double fc,fs,a;
     if (
	 (!dir->Get(&dirVal,r,c,dir)) ||
	 (dirVal == -1 ) || /* flat */
         (!fluxIn->Get(&fluxInVal,r,c,fluxIn))
        )  {
	fluxOut->PutMV(r,c,fluxOut);
        stateOut->PutMV(r,c,stateOut);
	continue; /* it is a missing value */
     }

     /* rounddown, [0,1> -> 0 [1,2> -> 1, etc.  */
     dirValQuadr = (int)floor((dirVal / M_PI) * 4 );

     switch(dirValQuadr) {
	case 0:
		 a = dirVal - 0;
		 fc = fluxInVal*cos(a);
		 fs = fluxInVal*sin(a);
		 ReplaceOutFlux(fc,r,c,fluxOut);

		 AddToState(fc-fs, r-1,c,  stateOut);
		 AddToState(fs,    r-1,c+1,stateOut);
		 AddToState(-fc,   r,  c,  stateOut);
		 break; 
	case 1:
		 a = (M_PI)/2 - dirVal;
		 fc = fluxInVal*cos(a);
		 fs = fluxInVal*sin(a);
		 ReplaceOutFlux(fc,r,c,fluxOut);

		 AddToState(fs,   r-1,c+1,stateOut);
		 AddToState(fc-fs,r,  c+1,stateOut);
		 AddToState(-fc,  r,  c,  stateOut);
		 break; 
        case 	2 :
	 a = dirVal - (M_PI)/2;
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc,r,c,fluxOut);

	 AddToState(fc-fs, r,  c+1,stateOut);
	 AddToState(fs,    r+1,c+1,stateOut);
	 AddToState(-fc,   r,  c,  stateOut);
	 break;                              
        case 	3 :
	 a = (M_PI) - dirVal;
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc,r,c,fluxOut);

	 AddToState(fs,   r+1,c+1,stateOut);
	 AddToState(fc-fs,r+1,c,  stateOut);
	 AddToState(-fc,  r,  c,  stateOut);
	 break;                              
        case 	4 :
	 a = dirVal - (M_PI);
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc,r,c,fluxOut);

	 AddToState(fc-fs,r+1,c,  stateOut);
	 AddToState(fs,   r+1,c-1,stateOut);
	 AddToState(-fc,  r,  c,  stateOut);
	 break;                              
        case 	5 :
	 a = (1.5*M_PI) - dirVal;
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc,r,c,fluxOut);

	 AddToState(fs,   r+1,c-1,stateOut);
	 AddToState(fc-fs,r,  c-1,stateOut);
	 AddToState(-fc,  r,  c,stateOut);
	 break;                              
        case 	6 :
	 a = dirVal - (1.5*M_PI);
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc, r,   c,  fluxOut);

	 AddToState(fc-fs, r,   c-1,stateOut);
	 AddToState(fs,    r-1, c-1,stateOut);
	 AddToState(-fc,   r,   c,  stateOut);
	 break;                              
        case 	7 :
        case 	8 : /* 8 should not happen but numerical rounding 
		     * may cause it	
                     */
	 a = 2*M_PI - dirVal;
	 fc = fluxInVal*cos(a);
	 fs = fluxInVal*sin(a);
	 ReplaceOutFlux(fc,r,c,fluxOut);

	 AddToState(fs,    r-1,c-1,stateOut);
	 AddToState(fc-fs, r-1,c,  stateOut);
	 AddToState(-fc,   r,  c,  stateOut);
	 break;                              
       default:
	 printf("value is %d dirVal is %g\n", dirValQuadr,dirVal);
	 PRECOND(FALSE);
   } /* switch */
 }
    return 0;
}
