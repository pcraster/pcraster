#include "stddefx.h"
#include "geometry.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.) and point's prototypes "" */
#include "mathx.h"
#include "misc.h"
#include <float.h>

#ifndef INCLUDED_BOOST_VERSION
#include <boost/version.hpp>
#define INCLUDED_BOOST_VERSION
#endif

#if BOOST_VERSION > 105800
#include <boost/test/tools/floating_point_comparison.hpp>
#else
#include <boost/test/floating_point_comparison.hpp>
#endif

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/* tell PointInPolygon() if point ON polygon is IN
 * If geomPointOnPolygon is non-zero (default) then
 * PointInPolygon regards a point On polygon as IN
 * the polygon
 */
int geomPointOnPolygonIsIn = 1;
/* used in SmallestFittingRectangleCentre()
 * The smallest fitting rectangle is found by rotating the polygon
 * in steps of pi/geomFittingRectangleStep and computing the area.
 * geomFittingRectangleStep has default value 16.
 */
double geomFittingRectangleStep = 16;

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
#define LENGTH(p1,p2)		(fabs((p1)-(p2)))
#define IN_BETWEEN(p1, b, p2)	(LENGTH((p1),(b)) + LENGTH((p2),(b)) <= \
					LENGTH((p1),(p2)))
				/* NOTE the macro IN_BETWEEN compares
				   at <= instead of ( the mathematicall
				   justifiable) ==. This implementation will
				   hold better if the parameters are floats */

#define Double(x)	((double)x)
/* put long double here if neccessary */
#define LDouble(x)	((double)x)
#define LD(x)		LDouble(x)

#define ON_POLYGON (geomPointOnPolygonIsIn)


/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/*
 * FLT_EPSILON is to precise for testcase in
 * void geom::PointTest::testPerpOnCord()
 * TODO
 *  make all this a C++ template class
 *   with types and a certains EPS approiate
 *     in certains conditions
 *  Below is the one DT2D uses
 */
#define EPS	0.000004

static BOOL CompEps(
	double a, /* r- value to compare against b */
	double b) /* r- value to compare against a */
{
  /* void geom::PointTest::testPerpOnCord()
   * really streches this one
   * found this code on the NET
   */
  double d=MAX(1,MAX(fabs(a),fabs(b)));
  return (fabs(a-b)/d) < EPS;
/*
  double eps;
  printf("%g  %g %g %g\n",a,b,d,fabs(a-b)/d);
  if (fabs(a) < EPS  || fabs(b) < EPS)
    return (fabs(a-b) < EPS);
 *
 * Let Eps be a percentage of value a
  eps=fabs(EPS*a);
return a-eps <= b && b <= a+eps;
*/
}

/* create a line from 2 distinct points on that line
 * returns argument l
 */
LINE  *CalcLine(
	LINE *l,    /* write-only, line created */
	const POINT2D *a, /* first point on the line */
	const POINT2D *b) /* second point on the line */
{
	if(PointsEq(a,b))
  	PRECOND(! PointsEq(a,b));
	if (a->x == b->x)
	{
		l->parY  = TRUE;
		l->xInt  = Double(a->x);
		l->slope = 0.0;
	}
	else
	{
		l->parY  = FALSE;
		l->slope = Double(a->y - b->y) / Double(a->x - b->x);
		l->yInt  = Double(a->y) - ( l->slope * Double(a->x) );
	}
	return(l);
}

/* compute line perpendicular to line l going through point k
 * returns argument perp
 */
LINE *PerpLine(
	LINE *perp,    /* write-only. line created */
	const POINT2D *k,/* point element of perp */
	const LINE *l) /* line perpendicular to perp  */
{
	if (l->parY)  /* vertical */
	{
		/* perpendicular is horizontal */
		perp->slope = 0;
		perp->parY  = FALSE;
		perp->yInt  = k->y;
	}
	else
	{
	if (l->slope == 0) /* horizontal */
	     {
		/* perpendicular is vertical */
		perp->slope = 0;  /* actually undefined */
		perp->parY  = TRUE;
		perp->xInt  = k->x;
	     }
	 else
	    { /* not vertical or horizontal */
		perp->slope = -1 / l->slope;
		perp->parY  = FALSE;
		perp->yInt  = -(perp->slope * k->x) + k->y;
	        POSTCOND( CompEps( YgivenX(perp, (PTYPE)0) , perp->yInt));
	    }
	 }
   /* we could relax the EPS for the CONDITION below
    * for now disable, this made EPS bigger and bigger!
    * POSTCOND( PointOnLine(perp, k));
    */
	 return(perp);
}

/* compute intersection of 2 lines
 * returns argument p if lines intersect or
 * NULL if lines are parallel
 */
POINT2D *IntersectLines(
	POINT2D *p, /* write-only, point of intersection. Previous
	           *  value is untouched if lines are parallel
	           */
	const LINE *l1, /* line 1 */
	const LINE *l2) /* line 2 */
{
	if (l1->parY && l2->parY)
		return(NULL);

	if (l1->parY) /* l2 not */
	{
		p->x = (PTYPE)l1->xInt;
		p->y = (PTYPE)YgivenX(l2, p->x);
		return(p);
	}
	if (l2->parY) /* l1 not */
	{
		p->x = (PTYPE)l2->xInt;
		p->y = (PTYPE)YgivenX(l1, p->x);
		return(p);
	}
	if (l1->slope == l2->slope)
		return(NULL);
	p->x =  (PTYPE)((l2->yInt - l1->yInt) / (l1->slope - l2->slope));
	p->y =  (PTYPE)YgivenX(l1, p->x);
	return(p);
}

/* compute intersection on 2 cords
 * returns argument i if cords intersect or
 * NULL if not or if parallel
 */
POINT2D *IntersectCords(
	POINT2D *i, /* write-only, point of intersection.
	           * Undefined if cords do not intersect
	           */
	const POINT2D *l1p1,  /* first end of cord 1 */
	const POINT2D *l1p2,  /* second end of cord 1 */
	const POINT2D *l2p1,  /* first end of cord 2 */
	const POINT2D *l2p2)  /* second end of cord 2 */
{
	LINE l1, l2;

	(void)CalcLine(&l1, l1p1, l1p2);
	(void)CalcLine(&l2, l2p1, l2p2);

	if (! IntersectLines(i, &l1, &l2))
		return(NULL);
	/* else there is an intersection,
	 *  So point i is on both lines,
	 *  now test if that intersection falls on both cords:
	 */
	if (
	     PointOnLineAlsoOnCord(i, l1p1, l1p2) &&
	     PointOnLineAlsoOnCord(i, l2p1, l2p2) )
		return(i);
	return(NULL);
}

/* compute intersection on a cord and a line
 * returns argument i if cord and line intersect or
 * NULL if not
 */
POINT2D *IntersectLineCord(
	POINT2D *i, /* write-only, point of intersection. Previous
	           *  value is undefined if cords do not intersect
	           */
	const LINE  *l,     /* line  */
	const POINT2D *c1,    /* first end of cord */
	const POINT2D *c2)    /* second end of cord */
{
	LINE cordLine;

	(void)CalcLine(&cordLine, c1, c2);

	if (! IntersectLines(i, l, &cordLine))
		return(NULL);
	/* else there is an intersection,
	 *  So point i is on both lines,
	 *  now test if that intersection falls on the cord:
	 */
	if ( PointOnLineAlsoOnCord(i, c1, c2))
		return(i);
	return(NULL);
}

/* test if a point of a line through a cord is on the cord
 * Point p is assumed to be on the line through the cord.
 * This precondition is tested in DEBUG mode.
 * Behaviour is undefined if this precondition fails
 * returns 0 if point p is not on cord c1-c2
 * non-zero if point p is on cord c1-c2
 */
int PointOnLineAlsoOnCord(
	const POINT2D *p,     /* point that is on the line through
	                     the cord  c2,c2 */
	const POINT2D *c1,  /* first end of cord */
	const POINT2D *c2)  /* second end of cord  */
{
	PTYPE maxX ,minX, maxY, minY;
	IFDEBUG(LINE lineThroughCord);

	PRECOND(PointOnLine(CalcLine(&lineThroughCord,c1,c2), p));

	/* maxX, minX, maxY, minY defines the
	rectangle of the 2 points c1 and c2 */
	maxX = MAX(c1->x, c2->x);
	minX = MIN(c1->x, c2->x);
	maxY = MAX(c1->y, c2->y);
	minY = MIN(c1->y, c2->y);

	/* see if it falls on the cord */
	return(
	     (minX <= p->x) && (p->x <= maxX) &&
	     (minY <= p->y) && (p->y <= maxY) );
}

/* compute perpendicular line going through point p and perpendicular to cord c1-c2
 * returns argument cut if the perpendicular line cuts the cord.
 * NULL otherwise.
 */
POINT2D *PerpOnCord(
	POINT2D *cut,   /* write-only. Cutting point of perp and *line through*
	               * cord c1-c2. Even if the function returns NULL!
	               */
	LINE  *perp,  /* write-only. perpendicular line on *line through*
	               * cord c1-c2 going through point p. Even if the function
	               * returns NULL!
	               */
	const POINT2D *p,   /* point going through the perpendicular */
	const POINT2D *c1,  /* first end of cord */
	const POINT2D *c2)  /* second end of cord  */
{
	LINE lineThroughCord;

	(void)CalcLine(&lineThroughCord, c1, c2);
	(void)PerpLine(perp, p, &lineThroughCord);
	(void)IntersectLines(cut, &lineThroughCord, perp);

	if (PointOnLineAlsoOnCord(cut, c1, c2))
		return(cut);
	return(NULL);
}

void Projected(
    POINT2D *projected,
    const POINT2D *p,
    const LINE    *line)
{
  LINE perp;
 (void)PerpLine(&perp, p, line);
 // if they do not intersect then it was already on the line
 if (!IntersectLines(projected, line, &perp))
   *projected = *p;
}

double DistPointLine(
    const POINT2D *p,
    const LINE    *line)
{
  POINT2D projected;
  Projected(&projected,p,line);
  return Dist(&projected,p);
}

/* compute the middle point of 3 points on a line
 * Points p1, p2 and p3 are assumed to be on the line.
 * This precondition is tested in DEBUG mode.
 * returns
 *
 * 1 if p1 is between p2 and p3 on the line,
 *
 * 2 if p2 is between p1 and p3 on the line,
 *
 * 3 if p3 is between p1 and p2 on the line.
 */
int MiddleOnLine(
	const LINE *l,    /* line where all three points are part of */
	const POINT2D *p1,  /* point 1 */
	const POINT2D *p2,  /* point 2 */
	const POINT2D *p3)  /* point 3 */
{
	double d1,d2,d3;

	PRECOND(PointOnLine(l, p1));
	PRECOND(PointOnLine(l, p2));
	PRECOND(PointOnLine(l, p3));

	if (l->parY)
	{
	   /* test on Y-value */
	   d1 = p1->y;
	   d2 = p2->y;
	   d3 = p3->y;
	}
	else
	{
	   /* test on X-value */
	   d1 = p1->x;
	   d2 = p2->x;
	   d3 = p3->x;
	}
	if((d1 <= d2 && d2 <= d3) ||
	   (d1 >= d2 && d2 >= d3))
		return(2);
	else
	{
		if((d1 <= d3 && d3 <= d2) ||
		   (d1 >= d3 && d3 >= d2))
			return 3;
		else
		{
			POSTCOND((d2 <= d1 && d1 <= d3) ||
		   		(d2 >= d1 && d1 >= d3));
			/* if COND fails, loss of pre.  */
			return 1;
		}
	}
}

/* compute X co-ordinate of point on line l given Y co-ordinate
 * Line l is assumed
 * .B not
 * to be of type 'x = constantValue'.
 * This precondition is tested in DEBUG mode.
 * returns X co-ordinate belonging to Y co-ordinate on line l
 */
double	XgivenY(
	const LINE *l, /* the line  */
	double y)      /* value of Y co-ordinate */
{
	PRECOND( l->parY || l->slope != 0); /* ! parX */

	if (l->parY)
		return(l->xInt);
	else
		return( (y - l->yInt) / l->slope );
}

/* compute Y co-ordinate of point on line l given X co-ordinate
 * Line l is assumed
 * .B not
 * to be of type 'y = constantValue'.
 * This precondition is tested in DEBUG mode.
 * returns Y co-ordinate belonging to X co-ordinate on line l
 */
double	YgivenX(
	const LINE *l, /* the line  */
	double x)      /* value of X co-ordinate */
{
	PRECOND(! l->parY);

	return( l->yInt + (x*l->slope) );
}

/* test if a point p in on line l
 * returns
 *
 * 0 if point is not on line,
 *
 * non-zero if point is on line.
 */
int PointOnLine(
	const LINE *l,  /* line */
	const POINT2D *p) /* point */
{
	if (l->parY)
		return(l->xInt == p->x);
	if (l->slope == 0)
		return(l->yInt == p->y);
	return(CompEps(
		YgivenX(l, p->x)
		, p->y));
}

/* test if a point p is in polygon pol
 * whether a point on
 * the cords of a polygon
 * is regarded as in the polygon is defined by
 * the global variable geomPointOnPolygonIsIn.
 * If it is 0 then a point on a cord is not in polygon,
 * otherwise point on a cord is in polygon (default)
 *
 * returns
 *
 * 0 if point is not in polygon,
 * non-zero if point is in polygon
 *
 * Warning
 *
 * Optimization of this function in Microsoft 6.0
 * generated wrong code used #pragma optimize("e",off/on)
 * to prevent this.
 */
int  PointInPolygon(
 const POINT2D 	*p,   /* point  */
 const POINT2D *pol,     /* polygon */
 int	nr)           /* number of points defining the polygon  */
{
        /* algorithm used is plumb-line-algorithm described
         * in Monmonier's Computer Assisted Cartography
         * Revision 1.2  and earlier was wrong for edges that were
         * parallel to the plumb-line
         */
	int	i;
	int	nrInter=0;
	double  atY;
	PTYPE	minX, maxX, minY, maxY;
	LINE    l;

	PRECOND(nr > 2);
	PRECOND(pol[0].x == pol[nr].x);
	PRECOND(pol[0].y == pol[nr].y);

	for(i = 0; i < nr; i++)
	{
	  maxX = MAX(pol[i].x, pol[i+1].x);
          minX = MIN(pol[i].x, pol[i+1].x);
	  maxY = MAX(pol[i].y, pol[i+1].y);
          minY = MIN(pol[i].y, pol[i+1].y);

	  if (p->x == pol[i].x)
	  { /* do not test i+1 -> double counting otherwise */
	  	if (p->y == pol[i].y)
		  return(ON_POLYGON); /* p == pol[i] */
	  	if (pol[i+1].x != pol[i].x)
		{
#	ifdef NEVER
		   /* rev 1.2 and earlier */
			if (p->y > pol[i].y)
				nrInter++;
#	  endif
		}
		else /* line : x = a */
		{
			if (minY < p->y && p->y < maxY)
			  return(ON_POLYGON); /* p on cord pol[i],pol[i+1] */
			else
			{
#	   ifdef NEVER
		          /* rev 1.2 and earlier */
			   /* DEAD WRONG:
			     edge is parallel to plumb-line: NO INTERSECTION */
				if (  (maxY < p->y) &&
				      /* i-1 -> (i ? nr:i)-1   */
				    IN_BETWEEN(pol[(i?nr:i)-1].x , p->x ,
				      pol[(i+2)%nr].x ))
					nrInter++;
#	   endif
		        }
		}
	}  /* eoif (p->x == pol[i].x )) */
	else
	  if ( minX < p->x && p->x < maxX )
	  {
	     /* intersection is possible */
	     /* line: y = ax + b or y = a */
	     if (p->y == minY && minY == maxY)
		return(ON_POLYGON);  /* y = a, p on cord pol[i],pol[i+1] */
	     if (maxY < p->y)
			nrInter++;
	     else
		{
		  if (minY < p->y)
		  {
			atY = YgivenX(CalcLine(&l,
				&(pol[i]), &(pol[i+1])), Double(p->x));
				if (atY < Double(p->y))
					nrInter++;
				if (atY == Double(p->y))
					return(ON_POLYGON);  /* p on cord pol[i],pol[i+1] */
                  }
		}
	  }
      } /* eofor */
      return((nrInter%2)==1);
}

/* compute the centroid of polygon pol
 * In addition to computing the centroid, this function
 * test if the centroid is in the polygon by calling
 * PointInPolygon
 * returns 0 if centroid is not in polygon
 * non-zero if centroid is in polygon
 *
 * Warning: Optimization of this function in Microsoft 6.0
 * generated wrong code used #pragma optimize("e",off/on)
 * to prevent this
 */
int CentroidOfPolygon(
       POINT2D 	*c,   /* write-only. the centroid  */
 const POINT2D *pol,    /* polygon */
 int	nr)           /* number of points defining the polygon  */
{
	int	i;
	long double up,low;

	PRECOND(nr > 2);
	PRECOND(pol[0].x == pol[nr].x);
	PRECOND(pol[0].y == pol[nr].y);

	up = 0;
	low = 0;
	for(i = 0; i < nr; i++)
	{
		up  += LDouble((pol[i+1].x + pol[i].x)/2) * LDouble(pol[i+1].x - pol[i].x)
			* LDouble((pol[i+1].y + pol[i].y)/2);
		low += LDouble(pol[i+1].x - pol[i].x) * LDouble((pol[i+1].y + pol[i].y)/2);
	}
	c->x = (PTYPE)(up/low);

	up = 0;
	low = 0;
	for(i = 0; i < nr; i++) /* this loop is wrong when /Oe is enabled */
	{
		up  += LDouble((pol[i+1].y + pol[i].y)/2) * LDouble(pol[i+1].y - pol[i].y)
			* LDouble((pol[i+1].x + pol[i].x)/2);
		low += LDouble(pol[i+1].y - pol[i].y) * LDouble((pol[i+1].x + pol[i].x)/2);
	}
	c->y = (PTYPE)(up/low);

	return(PointInPolygon(c, pol, nr));
}

/* compute the centre of the smallest fitting rectangle enclosing a polygon
 * the smallest fitting rectangle is found by rotating the polygon
 * in steps of pi/geomFittingRectangleStep and computing the area.
 * the centre of the smallest fitting rectangle that has a centre in
 * the polygon is returned. geomFittingRectangleStep
 * (double) is a global variable with default value 16.
 *
 * returns
 *
 * 0 if no rectangle is found that has its centre in the polygon,
 * non-zero if such a rectangle is found.
 */
int  SmallestFittingRectangleCentre(
	POINT2D *c,   /* write-only. the centre of the smallest rectangle that
	             * surrounds polygon pol, lying in polygon pol.
	             * undefined if return value is 0.
	             */
	const POINT2D *pol, /* the polygon */
	int nr)           /* number of points in the polygon */
{
	PTYPE minX, maxX, minY, maxY;
	double angle, bestAngle;
	double area, smallArea;
	double step = geomFittingRectangleStep;
	int i;
	POINT2D *p, nc; /* p is work-copy of polygon */

	PRECOND(pol[0].x == pol[nr].x);
	PRECOND(pol[0].y == pol[nr].y);


	p = (POINT2D*)MemcpyChkMalloc(pol, (nr+1)*sizeof(POINT2D));

	smallArea = 0.0;
	angle = 0.0;
	while (angle < M_PI/2.0)
	{
		minX = maxX = p[0].x;
		minY = maxY = p[0].y;
		for (i=1;i < nr ; i++)
		{
			minX = MIN(minX, p[i].x);
			maxX = MAX(maxX, p[i].x);
			minY = MIN(minY, p[i].y);
			maxY = MAX(maxY, p[i].y);
		}

		area =  AreaRectangle(maxX, minX, maxY, minY);
		if (smallArea == 0.0 || smallArea > area)
		{
			nc.x = minX + ((maxX - minX)/2);
	        	nc.y = minY + ((maxY - minY)/2);
			if (PointInPolygon(&nc, p, nr))
			{
				smallArea = area;
				(void)CopyPoint(c, &nc);
				bestAngle = angle;
			}
		}

		for (i=0;i < nr+1; i++)
			(void)RotPoint(&(p[i]), M_PI/step);
		angle += M_PI/step;
	}
	Free(p);

	if (smallArea == 0.0)
		return(FALSE);

	(void)RotPoint(c, -bestAngle);
	POSTCOND(PointInPolygon(c, pol, nr));
	return(TRUE);
}

/* compute area of rectangle. DOES NOT WORK!
 * Flagged with a PRECOND(FALSE)
 */
double AreaRectangle(
double	maxX,
double  minX,
double  maxY,
double  minY)
{
	PRECOND(FALSE);
	return( (maxX - minX) * (maxY - minY));
}

/* compute area of polygon
 * returns the area of the polygon
 */
double	AreaOfPolygon(
	const POINT2D *p, /* the polygon */
	int	nr)     /* number of points defining the polygon */
{
	int	i;
	long double	a=0;

	PRECOND(p[0].x == p[nr].x);
	PRECOND(p[0].y == p[nr].y);

	for(i = 0; i < nr; i++)
		a += LDouble(p[i].x)*LDouble(p[i+1].y) -
			LDouble(p[i].y)*LDouble(p[i+1].x);
	a /= -2;
	return((double)ABS(a));
}



/* clockwise angle of a point with the x-axis
 * returns the clockwise angle between 0 (included) and 2 *pi* (not included)
 *    or -1 if the point (0,0) is given.
 *
 * Example: if point is (1,1), CWAngle returns 1.75 pi.
 */
double CWAngle(
	const POINT2D *p) /* the point */
{
	double angle;

	/********************************************************/
	/* Documentation note	CWAngle				*/
	/* What we want is a clockwise scale starting at 0 and  */
	/* ending at 2 PI where 0 == 2 PI			*/
	/* 2 PI is not used, only 0                             */
	/* Range = [0, 2 PI>				        */
	/* We use the function atan2 for this purpose           */
	/* this function is not defined for the angles 0, PI/2, */
	/* PI and 3/2 PI 					*/
	/* after that we have to transform the result to our    */
	/* clockwise scale					*/
	/********************************************************/
	/*	atan2( 3.000000, 1.000000) = 1.249046		*/
	/*	atan2( -3.000000, 1.000000) = -1.249046		*/
	/*	atan2( -3.000000, -1.000000) = -1.892547	*/
	/*	atan2( 3.000000, -1.000000) = 1.892547		*/
	/*							*/
	/********************************************************/

	if (p->x == 0 && p->y == 0)
		return -1;

	if (p->x == 0)
	{
		if (p->y > 0)
			return(0);
		return(M_PI);
	}
	if (p->y == 0)
	{
		if (p->x > 0)
			return(M_PI/2);
		return(M_PI + M_PI/2);
	}

	angle = atan2(p->x, p->y);
	if (angle < 0) /* to clock wise scale */
		angle += M_2PI;

	POSTCOND(0.0 <= angle && angle < M_2PI);

	return(angle);
}

/* distance between two points
 * returns the distance between the two points
 */
double Dist(
	const POINT2D *p1,
	const POINT2D *p2)
{
	return((double)
#ifdef _MSC_VER
   _hypot
#else
   hypot
#endif
    ((p1->x)-(p2->x), (p1->y)-(p2->y)));
}

/* compare two points
 * returns 0 if they are not equal
 *         non-zero they are equal
 */
int PointsEq(
	const POINT2D *p1, /* point 1 */
	const POINT2D *p2) /* point 2 */
{
	return(p1->x == p2->x && p1->y == p2->y);
}

/* compare two points in qsort fashion
 * CmpPoints is for qsort-type comparison
 *
 * returns
 *
 * 0 if they are equal,
 *
 * < 0 if (p1.x < p2.x) || (p1.x==p2.x && p1.y < p2.y),
 *
 * < 0 if (p1.x > p2.x) || (p1.x==p2.x && p1.y > p2.y),
 */
int CmpPoints(
	const POINT2D *p1, /* point 1 */
	const POINT2D *p2) /* point 2 */
{
	int xc = CmpDouble(&(p1->x), &(p2->x));
	if (!xc)
		return CmpDouble(&(p1->y), &(p2->y));
	return xc;
}

/* multiply point by a constant
 * returns argument p
 */
POINT2D *MultiplyPoint(
	POINT2D *p, /* read-write, point that is multiplied */
	double f) /* multiplication factor */
{

	p->x *= f;
	p->y *= f;
	return p;
}

/* multiply point by a constant given an origin
 * The vector org,src is multiplied by d resulting
 * in vector org,p
 * returns argument p
 *
 * Example: MulPoint_0(p, {2,2}, 3, {1,1}),
 * p becomes {4,4}
 */
POINT2D *MulPoint_0(
 POINT2D 	*p,	   /* write-only, result */
 const POINT2D *src, /* point to multiply   */
 double	d,         /* multiplication factor */
 const POINT2D *org) /* origin of multiplication */
{
	(void)CopyPoint(p,src);

	return(
		AddPoint(
			MultiplyPoint(
				SubtrPoint(p, org),
				d),
			org)
	      );
}

/* add two points
 * returns argument d
 */
POINT2D *AddPoint(
 POINT2D *d,      /* read-write, point modified */
 const POINT2D *s)/* other point */
{
	d->x += s->x;
	d->y += s->y;
	return(d);
}

/* Subtract two points
 * returns argument d
 */
POINT2D *SubtrPoint(
 POINT2D *d,      /* read-write, point modified */
 const POINT2D *s)/* other point */
{
	d->x -= s->x;
	d->y -= s->y;
	return(d);
}

/* copy point
 * returns argument d
 */
POINT2D *CopyPoint(
	POINT2D *d, /* destination point */
 const	POINT2D *s) /* source point */
{
	d->x = s->x;
	d->y = s->y;
	return(d);
}

/* rotate a point
 * Rotate a point by a counter clockwise
 * angle. For example rotating (x=1,y=0) by
 * pi/4 (45 degrees) results in (x=0.7,y=0.7).
 * returns argument p
 */
POINT2D *RotPoint(
	POINT2D *p, /* read-write, point to rotated */
	double a) /* angle in radians, counter clock wise */
{
	double c, s, x;

	c = cos(a);
	s = sin(a);

	x =  (PTYPE)(
			((p->x) * c) - ((p->y) * s)
			);
	p->y =  (PTYPE)(
			((p->x) * s) + ((p->y) * c)
			);
	p->x = (PTYPE)x;

	return(p);
}

/* minimum X co-ordinate of polygon
 * returns minimum X co-ordinate of polygon
 */
double MinXPolygon(
	const POINT2D *p,/* the polygon */
	int   n)       /* number of points defining the polygon */
{
	int i;
	double m = p[0].x;
	PRECOND(n > 0);
	for(i=1; i < n; i++)
		m = MIN(p[i].x,m);
	return m;
}

/* minimum Y co-ordinate of polygon
 * returns minimum Y co-ordinate of polygon
 */
double MinYPolygon(
	const POINT2D *p,/* the polygon */
	int   n)       /* number of points defining the polygon */
{
	int i;
	double m = p[0].y;
	PRECOND(n > 0);
	for(i=1; i < n; i++)
		m = MIN(p[i].y,m);
	return m;
}

/* maximum X co-ordinate of polygon
 * returns maximum X co-ordinate of polygon
 */
double MaxXPolygon(
	const POINT2D *p,/* the polygon */
	int   n)       /* number of points defining the polygon */
{
	int i;
	double m = p[0].x;
	PRECOND(n > 0);
	for(i=1; i < n; i++)
		m = MAX(p[i].x,m);
	return m;
}

/* maximum Y co-ordinate of polygon
 * returns maximum Y co-ordinate of polygon
 */
double MaxYPolygon(
	const POINT2D *p,/* the polygon */
	int   n)       /* number of points defining the polygon */
{
	int i;
	double m = p[0].y;
	PRECOND(n > 0);
	for(i=1; i < n; i++)
		m = MAX(p[i].y,m);
	return m;
}

/* special case of intersection of 2 rectangles r1 and r2.
 * IntersectAllignedRectangles is a special case where r1 and
 * r2 must have sides parallel to x and y axis.
 * returns number of points in resulting polygon:
 *
 * 0 if rectangles do not intersect,
 *
 * 4 if rectangles do intersect
 */
int IntersectAllignedRectangles(
	POINT2D *pol,      /* intersecting rectangle stored in polygon format */
	const POINT2D *r1, /* rectangle 1 */
	const POINT2D *r2) /* rectangle 2 */
{
	double yMaxR1 = MaxYPolygon(r1,4);
	double yMinR1 = MinYPolygon(r1,4);
	double xMaxR1 = MaxXPolygon(r1,4);
	double xMinR1 = MinXPolygon(r1,4);

	double yMaxR2 = MaxYPolygon(r2,4);
	double yMinR2 = MinYPolygon(r2,4);
	double xMaxR2 = MaxXPolygon(r2,4);
	double xMinR2 = MinXPolygon(r2,4);

	double yMax= MIN(yMaxR1, yMaxR2);
	double yMin= MAX(yMinR1, yMinR2);
	double xMax= MIN(xMaxR1, xMaxR2);
	double xMin= MAX(xMinR1, xMinR2);

  // bug/sf463 and sf485
  // Test if rectangles R1 and R2 are adjacent.
  // If so, do not create an intersection ploygon
  // that represents the line between R1 and R2.
  // Such a line/polygon may incorrectly get a very
  // small area assigned due to floating point operations
#if BOOST_VERSION > 105800
  static boost::math::fpc::close_at_tolerance<double> tester(
         boost::math::fpc::fpc_detail::fraction_tolerance<double>(double(1e-8)),
         boost::math::fpc::FPC_STRONG);
#else
  boost::test_tools::close_at_tolerance<double>
         tester(boost::test_tools::fraction_tolerance_t<double>(double(1e-8)),
         boost::test_tools::FPC_STRONG);
#endif

  if(tester(static_cast<double>(yMax), static_cast<double>(yMin)) ||
         tester(static_cast<double>(xMax), static_cast<double>(xMin))){
    return 0;
  }

	/* centre of intersection must be in both rectangle
	 * test one to see if they intersect
	 */
	double  xCtr = (xMax+xMin)/2;
	double  yCtr = (yMax+yMin)/2;

	if((yMinR1 <= yCtr && yCtr <= yMaxR1) &&
	    (xMinR1 <= xCtr && xCtr <= xMaxR1) )
	{
		/* they intersect */

		/* bottom left */
		pol[0].x = xMin;
		pol[0].y = yMin;
		/* top left */
		pol[1].x = xMin;
		pol[1].y = yMax;
		/* top right */
		pol[2].x = xMax;
		pol[2].y = yMax;
		/* bottom right */
		pol[3].x = xMax;
		pol[3].y = yMin;
		/* copy last point */
		pol[4]   = pol[0];

		return 4;
	}
	return 0;
}
