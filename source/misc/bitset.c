#include "stddefx.h"

 /* algoritm is taken from
  *  Holub's compiler design in C (or something like that)
  *  Bentley's Programming Pearls (2nd ed. or vol. 2) also has
  * a few words on this one
  * More good bit counting stuff in DDJ sept 2000 p.133, with
  *   reply in oct/nov 2001 on an improvement
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

static const unsigned char nbits[] =
{
	/* 0   - 15  */ 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
	/* 16  - 31  */ 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
	/* 32  - 47  */ 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
	/* 48  - 63  */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 64  - 79  */ 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
	/* 80  - 95  */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 96  - 111 */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 112 - 127 */ 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
	/* 128 - 143 */ 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
	/* 144 - 159 */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 160 - 175 */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 176 - 191 */ 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
	/* 192 - 207 */ 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
	/* 208 - 223 */ 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
	/* 224 - 239 */ 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
	/* 240 - 255 */ 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};
/******************/
/* IMPLEMENTATION */
/******************/

/* compute the number of bits set
 * NrBitSet calculates the numbers of
 * bits that are 1 in the array.
 *
 * Also there is a macro NRBITSET_TYPE(set, type) in misc.h.
 *
 * For example
 *
 * int n,a = 7;
 *
 * n = NRBITSET_TYPE(a,int);
 *
 * POSTCOND(n == 3);
 * returns number of bits that are 1.
 */
int NrBitSet(
	const unsigned char *set,	/* array of setSize bytes */
	int   setSize)			/* number of bits in set */
{
	int i =0,n=0;
	/* whole bytes */
	int b,B = (setSize)/8;
	for(; i < B; i++)
		n += (int)nbits[set[i]];
	b = (setSize)%8;
	for(i=0; i < b; i++)
		n += ( (set[B]&(1<<b)) != 0) ? 1 : 0;
	return n;
}

/* Set given bit on 1.
 * Returns non-zero if the old value is 1, 0 otherwise.
 */
int SetBit1(
	unsigned char *set,		/* array of setSize bytes */
	int   setBit)			/* bit number to set to 1 */
{
	int i,n;
	int p = BitSet(set,setBit);
	INDEX_BITSET(setBit, n,i);
	PRECOND(setBit >= 0);
	set[n] |= 1 << i;
	return p;
}

/* Set given bit on 0.
 * Returns non-zero if the old value is 1, 0 otherwise.
 */
int SetBit0(
	unsigned char *set,		/* array of setSize bytes */
	int   setBit)			/* bit number to set to 0 */
{
	int i,n;
	int p = BitSet(set,setBit);
	INDEX_BITSET(setBit, n,i);
	PRECOND(setBit >= 0);
	set[n] &= ~(1<<i);  /* AND with complement */
	return p;
}

/* Checks given bit on being 1.
 * Returns non-zero if this is the case, 0 otherwise.
 */
int BitSet(
	const unsigned char *set, /* array of setSize bytes */
	int   bitIndex)		/* index nr of bit to check */
{
	int i,n;
	INDEX_BITSET(bitIndex, n,i);
	return set[n] & 1 << i;
}


/*ARGSUSED*/

/* compute the index of the lowest bit set
 * FirstBitSet calculates the index of
 * the lowest bit set to 1 in the array.
 * Also there is a macro FIRSTBITSET_TYPE(set, type) in misc.h.
 *
 * For example
 *
 * int i,a = 4;
 *
 * i = FIRSTBITSET_TYPE(a,int);
 *
 * POSTCOND(i == 2);
 * returns index of the lowest bit set to 1 or -1 if no bits are set to 1.
 */
int FirstBitSet(
	const unsigned char *set, /* array of at least setSize bits */
	int   setSize) /* number of bits in set to check */
{
	/* TODO: we can optimize this function
	 * by stepping through machine word size first
	 * and doing
	 * if set[indWordSize] == 0)
	 *   continue
	 * or something like that
	 */
	int i,j;
	int s, indFound=-1;  /* not found */
	s = BYTE_SETSIZE(setSize);
	for(i=0; i < s; i++)
	 for(j=0; j < 8; j++)
	  if (set[i] & (1<<j)) {
	   indFound = (i*8)+j;
	   goto found;
	  }
found:
	if (indFound == -1
	    || indFound >= setSize) /* 1-bit could be in un-initialized
	                             * last bits of last byte
	                             */
		return -1;
        return indFound;
}

/* compute the index of the highest bit set
 * LastBitSet calculates the index of
 * the highest bit set to 1 in the array.
 *
 * returns index of the highest bit set to 1 or -1 if no bits are set.
 */
int LastBitSet(
	const unsigned char *set, /* array of at least setSize bits */
	int   setSize) /* number of bits in set (to check) */
{
	/* TODO: something like the TODO in FirstBitSet
	 */
	int i,j;
	int s, bitsInLastByte;
	s = BYTE_SETSIZE(setSize);
	if (!s) /* set is 0-size, not found  */
	  return -1;

	/* do last partial filled byte */
	bitsInLastByte = setSize % 8;
	s--; /* this is the potentially partial filled byte */
	for(j= bitsInLastByte-1;  j >= 0; j--)
	  if (set[s] & (1<<j))
	   return (s*8)+j;
	/* now set s to first we will examin
	 * depending on if there is a partial filled byte
	 * already processed above
	 */
	if (bitsInLastByte)
		s--;
	/* do others */
	for(i=s; i >= 0; i--)
	 for(j=7; j >= 0; j--)
	  if (set[i] & (1<<j))
	   return (i*8)+j;
        return -1;
}

#ifdef CPU_BIG_ENDIAN
static const unsigned char *EndianFix(const unsigned char *set, int setByteSize)
{
	static unsigned char b[8];
	PRECOND(setByteSize == 1 || setByteSize == 2
	        || setByteSize == 4 || setByteSize == 8);
	if (setByteSize == 1)
		return set;
	
	(void)memcpy(b,set,setByteSize); /* never overlap */
	switch(setByteSize) {
	 case 2 : SwapByte2(b); break;
	 case 4 : SwapByte4(b); break;
	 case 8 : SwapByte8(b); break;
	}
	return (const unsigned char *)b;
}

#else
# ifndef CPU_LITTLE_ENDIAN
# error  NO CPU_LITTLE_ENDIAN or CPU_BIG_ENDIAN set
#endif
# define EndianFix(set, setByteSize) (set)
#endif

/* implementation for FIRSTBITSET_TYPE (LIBRARY_INTERNAL)
 */
extern int FirstBitSetType(const unsigned char *set, int setByteSize)
{
 DEVELOP_PRECOND(setByteSize == 1 || setByteSize == 2
        || setByteSize == 4 || setByteSize == 8);
 return FirstBitSet(EndianFix(set, setByteSize),setByteSize<<3);
}

/* implementation for NRBITSET_TYPE (LIBRARY_INTERNAL)
 */
extern int NrBitSetType(const unsigned char *set, int setByteSize)
{
 DEVELOP_PRECOND(setByteSize == 1 || setByteSize == 2
        || setByteSize == 4 || setByteSize == 8);
 /* << 3 = * 8 ;  bytes -> bits  */
 return NrBitSet(EndianFix(set, setByteSize),setByteSize<<3);
}
