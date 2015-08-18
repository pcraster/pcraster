#include "stddefx.h"
#include "misc.h"

/* Comparison function for unsigned char
 * Usable for qsort(),bsearch(), lfind() type comparison arguments
 * returns
 * 
 * < 0 if e1 is less than e2
 * 
 * = 0 if e1 is equivalent to e2
 * 
 * > 0 if e1 is greater than e2
 */
int CmpUchar(
	const unsigned char *e1,  /* pointer to single character */
	const unsigned char *e2)  /* pointer to single character */
{ return( ((int)(*e1))- ((int)(*e2))); }

/* Comparison function for integer 
 * Usable for qsort(),bsearch(), lfind() type comparison arguments
 * returns
 * 
 * < 0 if e1 is less than e2
 * 
 * = 0 if e1 is equivalent to e2
 * 
 * > 0 if e1 is greater than e2
 */
int CmpInt(
	const int *e1, /* pointer to single integer */
	const int *e2) /* pointer to single integer */
{ return((*e1)-(*e2)); }

/* Comparison function for float 
 * Usable for qsort(),bsearch(), lfind() type comparison arguments
 * returns
 * 
 * < 0 if e1 is less than e2
 * 
 * = 0 if e1 is equivalent to e2
 * 
 * > 0 if e1 is greater than e2
 */
int CmpFloat(
	const float *e1, /* pointer to single float */
	const float *e2) /* pointer to single float */
{ double e1_min_e2 = (*e1)-(*e2);
  if (e1_min_e2 < 0)
	return(-1);
  return (e1_min_e2 > 0);
}

/* Comparison function for double 
 * Usable for qsort(),bsearch(), lfind() type comparison arguments
 * returns
 * 
 * < 0 if e1 is less than e2
 * 
 * = 0 if e1 is equivalent to e2
 * 
 * > 0 if e1 is greater than e2
 */
int CmpDouble(
	const double *e1,  /* pointer to single double */
	const double *e2)  /* pointer to single double */
{ 
/* see cmpdoubl.s for GNU def */
  register double e1_min_e2 = (*e1)-(*e2);
  if (e1_min_e2 < 0)
	return(-1);
  return (e1_min_e2 > 0);
}
