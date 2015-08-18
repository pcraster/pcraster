#include "stddefx.h"
/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include "misc.h"

/* global header (opt.) and swapbyte's prototypes "" */


/* headers of this app. modules called */ 

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


/* perform endian swap on 2-byte entity (like short int)
 * Performs an endian swap. See example for correct use.
 *
 * EXAMPLE
 * .so examples/swap.tr
 */
void SwapByte2(void *b) /* read-write, entity to swap */
{
	char tmp,*p = (char *)b;
	/* 01 => 10 */
	tmp = p[0]; p[0] = p[1]; p[1] = tmp;
}

/* perform endian swap on 4-byte entity (like float, int)
 * Performs an endian swap. See example for correct use.
 *
 * EXAMPLE
 * .so examples/swap.tr
 */
void SwapByte4(void *b) /* read-write, entity to swap */
{
	char tmp,*p = (char *)b;
        /* 0123 => 3210 */
	tmp = p[0]; p[0] = p[3]; p[3] = tmp;
	tmp = p[1]; p[1] = p[2]; p[2] = tmp;
}

/* perform endian swap on 8-byte entity (like double)
 * Performs an endian swap. See example for correct use.
 *
 * EXAMPLE
 * .so examples/swap.tr
 */
void SwapByte8(void *b) /* read-write, entity to swap */
{
	char tmp,*p = (char *)b;
	/* 01234567 => 76543210 */
	tmp = p[0]; p[0] = p[7]; p[7] = tmp;
	tmp = p[1]; p[1] = p[6]; p[6] = tmp;
	tmp = p[2]; p[2] = p[5]; p[5] = tmp;
	tmp = p[3]; p[3] = p[4]; p[4] = tmp;
}
