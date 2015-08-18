#include "stddefx.h"
/*
 *
 */

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include "csf.h"
#include "app.h"

/* apps. called */

/*************/
/* EXTERNALS */
/*************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/**********************/
/* LOCAL DEFINITIONS  */
/**********************/

/******************/
/* IMPLEMENTATION */ 
/******************/

/* Compute output co-ordinate of a pixel on base of global options.
 * A row-column index is translate to a co-ordinate.
 * Which co-ordinate depends on the global options unit* and coor*.
 * The row, column co-ordinate don't have to be on the map. 
 * They are just relative to top-left position.
 * For example (row,col) = (-1,0) computes the (x,y) co-ordinate of
 * the pixel that is right above top-left pixel. 
 * returns:
 *  
 *  0 if the input co-ordinate (row,col) is outside the map,
 *  1 if inside,
 * -1 in case of an error
 *
 * Merrno
 * ILL_CELLSIZE
 */
int AppRgetCoords(
	const MAP *m,	/* map handle */
	int row,      /* Row number (relates to y position). */
	int col,      /* Column number (relates to x position). */
	double *x,      /* write-only. Returns x of  output co-ordinate */
	double *y)      /* write-only. Returns y of output co-ordinate */
{
	int result = (row >=0 && col >= 0 && row < (int)RgetNrRows(m) && col < (int)RgetNrRows(m));
	*x = col;
	*y = row;
	switch(appCoord) {
		case APP_C:  *y += 0.5;
		             *x += 0.5;
		             break;
	        case APP_LR: *y += 1;
	                     *x += 1;
	        IFDEBUG(case APP_UL:);
	}
	if(appUnitTrue)
		(void)RrowCol2Coords(m,*y,*x,x,y);
	return result;
}
