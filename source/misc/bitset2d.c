#include "stddefx.h" 

 /* algoritm is taken from 
  *  Holub's compiler design in C (or something like that)
  *  Bentley's Programming Pearls (2nd ed. or vol. 2) also has
  * a few words on this one
  */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h> /* memcpy */
#include "misc.h" 

/* global header (opt.) and bitset's prototypes "" */


/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
#define INDEX_BITSET(setBit, major, minor) \
	major = (((int) (setBit))/8), minor = (((int) (setBit))%8)

#define BYTE_SETSIZE(bitSetSize) \
	( (((int) (bitSetSize))/8)+(((((int) (bitSetSize))%8)==0) ? 0 : 1))

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* dsd
 * Use Free2d to delete
 */
unsigned char **NewBitMatrix(
	size_t nrRows,
	size_t nrCols)
{
	size_t nrByteCols = (size_t)((nrCols/8) + ((nrCols%8)!=0));
	return (unsigned char **)Malloc2d(nrRows,nrByteCols,
			sizeof(unsigned char));
}

void SetAllBitMatrix(
	unsigned char **m,
	int nrRows,
	int nrCols,
	int      v ) /* 0 or 1 */
{
	int i,nrByteCols = (nrCols/8) + ((nrCols%8)!=0);
	PRECOND(v == 0 || v == 1);
	if (v)
		v = 0xFF;
	for(i = 0; i < nrRows; i++)
		(void)memset(m[i],v,(size_t)nrByteCols);
}

int Set1BitMatrix(
	unsigned char **m,
	int r, int c)
{
	return SetBit1(m[r], c);
}

int Set0BitMatrix(
	unsigned char **m,
	int r, int c)
{
	return SetBit0(m[r], c);
}

/* Checks given bit on being 1.
 * Returns non-zero if this is the case, 0 otherwise.
 */
int BitMatrixSet(
	const unsigned char **m,
	int   r, int c)		/* nr. of bit to check */
{

	return BitSet(m[r],c);
}
