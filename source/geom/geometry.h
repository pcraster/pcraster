#ifndef GEOMETRY_H 
#define GEOMETRY_H 

#ifdef __cplusplus
 extern "C" {
#endif

typedef double PTYPE;
#define P_PR_TYPE	"%12.4f"
#define P_IN_TYPE	"%lg"

typedef struct POINT2D		/* a point */
{
	PTYPE	x;		/* x-coordinate */
	PTYPE	y;		/* y-coordinate */
} POINT2D;

typedef POINT2D   POLYGON;
/* POLYGON  is an array of points
        if N points are neccessary to describe an polygon
	then the polygon array is N+1 elements long.
	By storing point 0 again as point N makes it easier to
	adrress all cords of the polygon
	So a polygon described by N points, has an array of N+1 
	 elements where element 0 equals element N+1
*/
typedef POINT2D   ARC;
/* ARC a part of a polygon and also an array of POINT2DS
       an N point ARC is desrcibed by the cords:
       	arc[0] to arc[1]
	arc[1] to arc[2]
	arc[..] to arc[..]
	arc[N-2] to arc[N-1]
*/

typedef struct ARCS  {
	int n;
	ARC    p[68];	 /* temporary */
}ARCS;

typedef struct SEG
{
	int	nrPoints;	/* number of points in segment */
	int	start;
		/* start index in points en pointIds array */
} SEG;

/* a cord is a piece of line, or a line with two end points */ 


typedef struct LINE
{
	int	parY;	/* slope and yInt not defined if parY = TRUE */
			/* if parY == TRUE then line is parallel to 
			 *	y-axis  xInt identifies the line:
		         *	x = xInt
			 *  if parY == FALSE
                         *      y = yInt + slope * x
			 */
	double	slope;  /* slope is undefined if parY == TRUE */
#ifdef ANONYMOUS_UNION_MEMBERS     /* assume the use of anonymous unions */
union
	{
	double	yInt;	/* y intercept : the place where the y-axis is 
			    intersected */
	double	xInt;	/* x intercept : the place where the x-axis is 
			    intersected if line is parallel to y-axis  */
	};
#else	
	double	yInt;
#define xInt	yInt
#endif
} LINE;

/* point.c */
extern int geomPointOnPolygonIsIn;
extern double geomFittingRectangleStep;
extern LINE *CalcLine(LINE *l, const POINT2D *a, const POINT2D *b);
extern LINE *PerpLine(LINE *perp, const POINT2D *k, const LINE *l);
extern void Projected( POINT2D *projected, const POINT2D *p, const LINE    *line);
extern double DistPointLine( const POINT2D *p, const LINE    *line);
extern POINT2D *IntersectLines(POINT2D *p, const LINE *l1, const LINE *l2);
extern POINT2D *IntersectCords(POINT2D *i, const POINT2D *l1p1, const POINT2D *l1p2, const POINT2D *l2p1, const POINT2D *l2p2);
extern POINT2D *IntersectLineCord(POINT2D *i, const LINE *l, const POINT2D *c1, const POINT2D *c2);
extern int PointOnLineAlsoOnCord(const POINT2D *p, const POINT2D *c1, const POINT2D *c2);
extern POINT2D *PerpOnCord(POINT2D *cut, LINE *perp, const POINT2D *p, const POINT2D *c1, const POINT2D *c2);
extern int MiddleOnLine(const LINE *l, const POINT2D *p1, const POINT2D *p2, const POINT2D *p3);
extern double XgivenY(const LINE *l, double y);
extern double YgivenX(const LINE *l, double x);
extern int PointOnLine(const LINE *l, const POINT2D *p);
extern int PointInPolygon(const POINT2D *p, const POINT2D *pol, int nr);
extern int CentroidOfPolygon(POINT2D *c, const POINT2D *pol, int nr);
extern int SmallestFittingRectangleCentre(POINT2D *c, const POINT2D *pol, int nr);
extern double AreaRectangle(double maxX, double minX, double maxY, double minY);
extern double AreaOfPolygon(const POINT2D *p, int nr);
extern double CWAngle(const POINT2D *p);
extern double Dist(const POINT2D *p1, const POINT2D *p2);
extern int PointsEq(const POINT2D *p1, const POINT2D *p2);
extern int CmpPoints(const POINT2D *p1, const POINT2D *p2);
extern POINT2D *MultiplyPoint(POINT2D *p, double f);
extern POINT2D *MulPoint_0(POINT2D *p, const POINT2D *src, double d, const POINT2D *org);
extern POINT2D *AddPoint(POINT2D *d, const POINT2D *s);
extern POINT2D *SubtrPoint(POINT2D *d, const POINT2D *s);
extern POINT2D *CopyPoint(POINT2D *d, const POINT2D *s);
extern POINT2D *RotPoint(POINT2D *p, double a);
extern double MinXPolygon(const POINT2D *p, int n);
extern double MinYPolygon(const POINT2D *p, int n);
extern double MaxXPolygon(const POINT2D *p, int n);
extern double MaxYPolygon(const POINT2D *p, int n);
extern int IntersectAllignedRectangles(POINT2D *pol, const POINT2D *r1, const POINT2D *r2);
extern int IntersectRectangles(POINT2D *pol, const POINT2D *r1, const POINT2D *r2);

#ifdef __cplusplus
 }
#endif

#endif /* GEOMETRY_H */
