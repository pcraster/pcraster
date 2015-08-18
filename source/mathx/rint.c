#include "stddefx.h"

#include "mathx.h"

/* round to closest integer
* On most systems rint() is available, but not part of the
* ANSI-library.
*
* both Linux and HP-UX were doing this once wrong
*
* returns
* the integer value as a floating-point number.
*/
double Rint(double x) /* value to be rounded  */
{
	double t = fabs(x);
	if (t-floor(t) >= (double)0.5)
		t = ceil(t);
	else	
		t = floor(t);
	if (x < (double)0.0)
		t = -t;
	return t;
}

/* integer division
* Fdiv() divides x by abs(y), returning an integer value I (in double
* format) such that abs(I)*abs(y) <= x < abs(I+1)*abs(y). I has the
* same sign as x. If y is zero, a runtime error will occur
* returns
*  the integer division value as a floating-point number.
*/
double Fdiv(double x, /* x    */
           double y) /*  y */
{
 double t = floor(fabs(x)/fabs(y)); 
 return x < 0 ? -t : t;
}
