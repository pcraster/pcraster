#include "stddefx.h"
/*
 */

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include <math.h>
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

/* compute row, column number of an input co-ordinate.
 * The input-co-ordinate is converted based on the setting of
 * -unit* and coor*. The left and upper side of a pixel are viewed
 * as belonging to the pixel  and the right and lower not if coorul
 * or coorcentre are set. Vice versa if coorlr is set.
 * The return arguments are always set even if it's outside the map.
 * They are just relative from the top index.
 * returns:
 * 
 *  0  if the resulting (row,col) index is outside the map,
 *  1 if inside
 * 
 */
int AppRgetRowCol(
	const MAP *m,	/* map handle */
	double x,      	/* x of input co-ordinate */
	double y,      	/* y of input co-ordinate */
	int *row,    	/* write-only row number.  */
	int *col)    	/* write-only column number.  */
{
	double r,c;
	if(appUnitTrue)
		Rcoords2RowCol(m,x,y,&r,&c);
	else
		{ r = y; c = x; }

	(*row) = (int)floor(r);
	(*col) = (int)floor(c);
	if (appCoord == APP_LR && 
	   ((double)(*row)) == r &&
	   ((double)(*col)) == c )
	 { (*row)--; (*col)--;}

	return ((*col) >= 0 && (*row) >= 0 && 
	       (*col) < (int)RgetNrCols(m) && (*row) < (int)RgetNrRows(m));
}
