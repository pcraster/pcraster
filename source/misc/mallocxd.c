#include "stddefx.h"
#include "misc.h"
#include <string.h> /* memmove */

#ifndef INTEL16

/* Allocate memory to stuff a linear array in a 2d-array.
 * Allocation is done by allocating memory for an array of pointers to a row.
 * Every row start is copied to this array
 * The contents of the 2d-array can be addressed in a linear fashion,
 *  in a non INTEL16 app, by using Linear2d(void * a), where a is a pointer 
 * returned by Malloc2d or MallocIndex2d. 
 * MallocIndex2d is not available for a INTEL16 app.
 * Returns a void pointer to the 2d array or NULL if 
 * there is insufficient memory available
 */
void **MallocIndex2d(
	size_t row,       /* number of rows. > 0 */
	size_t col,       /* number of cols. > 0 */
	size_t size,   /* size of each array element in bytes. > 0 */
	const void *l) /* linear array of row*col*size bytes */
{
 size_t i;
 const char **prow;
 const char *ll = (const char *)l;

 PRECOND(row  > 0);
 PRECOND(col  > 0);
 PRECOND(size > 0);

 prow = (const char **)ChkMalloc(((size_t)row)*sizeof(void *));
 if (prow == NULL)
   return(NULL);

 for (i=0; i < (size_t)row; i++)
    {
     prow[i] = ll;
     ll += ((size_t)size) * ((size_t)col);
    }

 return((void **)prow);
}

/* Allocate memory that can be addressed as a 2d-array.
 * Allocation is done by allocating memory for each row and
 * memory for an array of pointers to a row.
 * Thus memory requirements are (row * sizeof(void *))+(row*col*size_t)
 * The contents of the 2d-array can be addressed in a linear fashion,
 *  in a non INTEL16 app, by using Linear2d(void * a), where a is a pointer 
 * returned by Malloc2d. 
 * Returns a void pointer to the 2d array or NULL if 
 * there is insufficient memory available
 */
void **Malloc2d(
	size_t row,       /* number of rows. > 0 */
	size_t col,       /* number of cols. > 0 */
	size_t size)   /* size of each array element in bytes. > 0 */
{
 void **prow, *pdata;

 PRECOND(row  > 0);
 PRECOND(col  > 0);
 PRECOND(size > 0);

 pdata = (void *)ChkMalloc(((size_t)row)*((size_t)col)*size);
 if (pdata == NULL)
   return(NULL);

 
 prow = MallocIndex2d(row,col,size, pdata);
 if (prow == NULL)
   Free(pdata);

 return prow;
} /* Malloc2d */

static void CopyColumns(
	void *ptr, 
	size_t   nrRows,
	size_t   newCol, 
	size_t   oldCol,
	size_t size)
{
  /* use memmove since mem can overlap 
   */
  size_t r;
  size_t dInc = newCol*size;  
  size_t sInc = oldCol*size;  
  char  *d    = ((char *)ptr);
  char  *s    = ((char *)ptr);

  PRECOND(newCol <= oldCol);
  for(r=0; r<nrRows; r++)
  {
  	(void)memmove(d,s,dInc);
  	d+=dInc;
  	s+=sInc;
  }
}


/* reallocate memory allocated by Malloc2d
 * Realloc2d reallocates memory allocated by Malloc2d 
 * in realloc(3) style.  The number of columns can only decrease.
 * Returns a void pointer to the 2d array or NULL if 
 * there is insufficient memory available
 */
void **Realloc2d(
	void **ptr,    /* existing 2d array */
	size_t newRow,    /* new number of rows. > 0 */
	size_t newCol,    /* new number of cols. > 0 AND <= oldCol */
	size_t oldRow,    /* old number of rows. > 0 */
	size_t oldCol,    /* old number of cols. > 0 */
	size_t size)   /* size of each array element in bytes. > 0.
	                * Must be the same as when creating the 2d array
	                * initially with Malloc2d!
	                */
{
 void *pdata;

 PRECOND(newRow > 0);
 PRECOND(newCol > 0);
 PRECOND(oldRow > 0);
 PRECOND(oldCol > 0);
 PRECOND(size   > 0);
 PRECOND(newCol <= oldCol);

 if (newCol < oldCol)
 	CopyColumns(ptr[0], MIN(oldRow, newRow), newCol, oldCol, size);

 pdata = (void *)ChkRealloc(ptr[0], ((size_t)newRow)*((size_t)newCol)*size);
 if (pdata == NULL)
   return(NULL);

 /* free old index array */
 FreeIndex2d(ptr);
 
 /* make new one */
 ptr = MallocIndex2d(newRow,newCol,size, pdata);
 if (ptr == NULL)
   Free(pdata);

 return ptr;
} /* Realloc2d */

/* deallocate the index of a 2d array created through a call to Malloc2d or MallocIndex2d
 */
void FreeIndex2d(
	void **array2d) /* Row-index is destructed, not the underlying array. 
	                 * Array created through a call
	                 * to Malloc2d 
	                 */
{
	 PRECOND(array2d != NULL);
	 Free(array2d);
}

/* deallocate a 2d array created through a call to Malloc2d or MallocIndex2d
 */
/*ARGSUSED*/
void Free2d(
	void **array2d, /* Destructed. Array created through a call
	                 * to Malloc2d 
	                 */
	size_t nrRows )    /* number of rows of the array */
{
	 PRECOND(*array2d != NULL);
	 PRECOND(array2d != NULL);
	 Free(*array2d);
	 Free(array2d);
  (void)nrRows; // Shut up compiler
} /* Free2d */

/* address of continuous area representing the 2d array contents
 * 2d array must be created through a call to Malloc2d 
 * If, for example, a 3x3 2d array name a is allocated, Linear2d(a)[5]
 * equals a[1][2]. 
 * This function can not be used in 16-bit mode (INTEL16).
 */
void *Linear2d(
	void **array2d) /* Array created through a call
	                       * to Malloc2d 
	                       */
{
 	PRECOND(*array2d != NULL);
	return(*array2d);
} /* Linear2d */

#else
/* DOS 16-bit compiler */

void **Malloc2d(size_t row, size_t col, size_t size)
{
	 size_t i;
	 char **prow;

	 prow = (char **)ChkMalloc(((size_t)row)*((size_t)sizeof(char *)));
	 if (prow == NULL)
	   return(NULL);

	 for (i=0; i < (size_t)row; i++)
	    {
	     prow[i] = (char *)ChkMalloc(((size_t)col)*((size_t)size));
	     if (prow[i] == NULL)
		 return(NULL);
	    }

	 return((void **)prow);
}

void Free2d(void **pa, size_t row)
{
	 size_t i;
	 char **prow;

	 prow = (char **)pa;

	 for (i=0; i < (size_t)row; i++)
	     Free(prow[i]);
	 Free(prow);
}

#endif
