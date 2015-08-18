#include "stddefx.h" 

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h" /* CmpDouble */
#include "geometry.h"

/* global header (opt.) and rect's prototypes "" */


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
/* SORT (LIBRARY_INTERNAL)
 */
typedef	struct SORT { 
	double angle; /* angle with c */
	int ind;      /* index in p */
} SORT;

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/



static int CmpAngle(const SORT *e1, const SORT *e2)
{
	return CmpDouble(&(e1->angle), &(e2->angle));
}

/* intersection of 2 rectangles r1 and r2.
 * returns number of points in resulting polygon:
 * .br
 * 0 if rectangles do not intersect
 * .br
 * or a value between [3,8] if rectangles do intersect
 */
int IntersectRectangles(
	POINT2D *pol,      /* intersecting rectangle stored in polygon format */
	const POINT2D *r1, /* rectangle 1 */
	const POINT2D *r2) /* rectangle 2 */
{
	/*PROBLEM:
	* intersection body of 2 rectangle r1 and r2 in 2-D.
	*
	*REMARKS:
	* -If they intersect, then at least one side of the smallest
	*  square is part of the intersection.
	* -Degenerated case: some sides of q1 fall in the sides of q2.
	*   then we still have the problem if q1 is inside q2 or vice versa.
	* -Degenerate case: only 1 point falls On other rectangle
	* -the resulting polygon is convex and has a max of 8 points.
	*
	*ALGORITHM:
	* calculate every intersection point of each pair of sides.
	*  and select the corner point that are in the other rectangle
	*  also as intersection points.
	* if (no intersection points)
	* then there is no intersection
	* else
	*  calculate centre of intersection points or
	*    some other point that is IN resulting polygon 
	*    ( average x and y will do: convex polygon )
	*   sort intersection point on angle between some line and centre
	*    resulting in the intersecting body
	*/

	POINT2D p[16];
	SORT sort[16];
	const POINT2D *r[2];
	POINT2D c = { 0 , 0 };  /*  point in pol */
	int   i,j,n = 0;
	r[0] = r1; r[1] = r2;
#	define OTHER_RECT(x)	( ((x)+1)%2 )

        /* look if cornerpoint fall in (or on) other rectangle */
	for(j = 0; j < 2; j++)
	{
	  for (i=0; i < 4; i++)
		if (PointInPolygon(r[j]+i, r[OTHER_RECT(j)], 4))
			p[n++] = r[j][i]; /* point in other rect */
	}
	/* look if sides intersect */
	for (i=0; i < 4; i++)
	 for (j=0; j < 4; j++)
		if  (IntersectCords(p+n, 
		        r1+i, r1+(i+1), r2+j,r2+(j+1)) != NULL )
		      n++; /* cords intersect */

	if (n == 0)
		return 0;

	for (i = 0; i < n; i++)
	 { c.x += p[i].x; c.y += p[i].y; }
	 c.x /= n; c.y /= n;
	for (i = 0; i < n; i++)
	 { 
	    POINT2D t = p[i];
	    if ( (sort[i].angle = CWAngle(SubtrPoint(&t,&c))) == -1 )
	    	return 0; /* c falls on only point, degenerated case */
	    sort[i].ind = i;
	 }
	qsort(sort, (size_t)n, sizeof(SORT), (QSORT_CMP)CmpAngle);
	pol[0] = p[sort[0].ind];
	for (j = i = 1; i < n; i++)
	 {
	       pol[j] = p[sort[i].ind];
	       if (pol[j].x != pol[j-1].x || pol[j].y != pol[j-1].y)
		j++;    /* remove duplicate points */
         }
        POSTCOND(j <= 8);
        pol[j] = pol[0]; /* close polygon */
	/* degenerate cases if j <= 2 */
	return (j > 2) ? j : 0;
#	undef OTHER_RECT
}
