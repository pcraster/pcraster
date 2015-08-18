#include "stddefx.h" 

#include "misc.h" 

static void  *AddBlock(struct  RECMEM_HEAP *r);
static void  AddToFreeList(struct  RECMEM_HEAP *r);
/********/
/* USES */
/********/

#include <string.h>	/* memcpy */

/***************/
/* EXTERNALS   */
/***************/



/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 
/* struct used for linking records (LIBRARY_INTERNAL)
 */
typedef struct RECMEM_LINK {
	struct RECMEM_LINK *next;
} RECMEM_LINK;

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/


/* create a 'heap' for records
 * Control an allocation structure for (typically) small records.
 * It provides a fast way to free all records by simply calling 
 * FreeRecMemHeap(). And it diminishes fragmentation of the true heap
 * if you choose blockSize right, which was the initial reason to program 
 * this due to a brain dead allocator in Turbo C (<= 3.0).
 *
 * Preconditions: 
 * The record size (recSize) must be equal or larger than 
 * sizeof(void *).
 *
 * Warning:
 * Use a sizeof(something) construct for the recSize argument to assure
 * proper allignment. (See malloc()).
 */
RECMEM_HEAP *NewRecMemHeap(
	size_t recSize,        /* size of each record */
	size_t blockSize,      /* number of records that goes in one malloc */
	void    *(*mallocFunction)(size_t size), /* ptr to function that does the allocation
			                          * if NULL then ChkMalloc is used
			                          */
	void    (*freeFunction)(void *ptr))      /* ptr to function that does the freeing 
			                          * if NULL then Free is used
			                          */
{
	RECMEM_HEAP *r;
	void *(*mallocFunc)(size_t size);
	void  (*freeFunc)(void *ptr);

	if (mallocFunction == NULL)
		mallocFunc = ChkMalloc;
	else
		mallocFunc = mallocFunction;

	if (freeFunction == NULL)
		freeFunc = FreeFuncPtr;
	else
		freeFunc = freeFunction;

	PRECOND(recSize >= sizeof(void *));

	r = mallocFunc(sizeof(RECMEM_HEAP));
	if (r != NULL)
	{
		r->recSize    = recSize;
		r->blockSize  = blockSize;
		r->Malloc = mallocFunc;
		r->_Free   = freeFunc;
		r->nrBlocks   = 0;
		if (AddBlock(r) == NULL)
		{
			freeFunc(r);
			r = NULL;
		}
#ifdef DEBUG_DEVELOP
		r->count = 0;
#endif 
	}
	return(r);
}


/* Allocates space for one record in a record heap
 * Allocates the amount of space that is defined  when creating
 * the RECMEM_HEAP by calling NewRecMemHeap().
 * This space must be freed by calling FreeRecord().
 * returns pointer to space or NULL if an error occured.
 */
void *NewRecord(
	RECMEM_HEAP *r) /* Abstract. RECMEM_HEAP to allocate from. Must be
	                 * created by NewRecMemHeap()
	                 */
{
	RECMEM_LINK *v;

	if (r->freeList == NULL) /* list exhausted */
		(void)AddBlock(r);     /* add a block */
        /* if add blocks fails 
	 * then is r->freeList still NULL 
	 */
	v = (RECMEM_LINK *)r->freeList;  

	if (v != NULL)
	{
		r->freeList = ((RECMEM_LINK *)r->freeList)->next;
#		ifdef DEBUG_DEVELOP
		 r->count++;
#		endif 
	}
	return((void *)v);
}

/* return space of one record to record heap
 * deallocates the space that is allocated by NewRecord().
 * This space must be allocated by NewRecord().
 * In DEBUG_DEVELOP mode, the pointer is checked if it is really from
 * the RECMEM_HEAP specified by calling VerifyRecPtr().
 */
void FreeRecord(
	void *m,        /* deallocated. pointer to space to free */
	RECMEM_HEAP *r) /* Abstract. pointer m should come from 
	                 * this heap
	                 */
{
#	ifdef DEBUG_DEVELOP
	PRECOND(VerifyRecPtr(m, r)); 
		/* it must be pointer from this recmem_heap */
#	endif 
	((RECMEM_LINK *)m)->next = r->freeList;
	r->freeList = m;
#	ifdef DEBUG_DEVELOP
	 r->count--;
#	endif 
}

/* deallocates all space associated with the record heap
 * deallocates all space that is as ever used by the record heap
 * and return that space the normal heap.
 */
void FreeAllRecords(RECMEM_HEAP *r)
{
	size_t i;

	for (i=0; i< r->nrBlocks; i++)
		r->_Free(r->blocks[i]);
        if (r->nrBlocks > 0)	
		r->_Free(r->blocks);
	r->_Free(r);
}

/*C the array of blocks is only modified/extended if the function
    is succesfull */
static void *AddBlock(RECMEM_HEAP *r)
{
	void **blockArray;

	blockArray = (void **)r->Malloc((r->nrBlocks+1)*sizeof(void *));
	if (blockArray != NULL)
	{
		blockArray[r->nrBlocks] = r->Malloc(r->blockSize * r->recSize);
		if (blockArray[r->nrBlocks] == NULL)
		{
			r->_Free(blockArray);
			blockArray  = NULL;
		}
		else
		{
			/* copy old entries in new array */
			(void)memcpy(blockArray, r->blocks, 
				r->nrBlocks*sizeof(void *));
			/* free old array */
			if (r->nrBlocks > 0)
				r->_Free(r->blocks);
			/* assign new array */
			r->blocks = blockArray;
			r->nrBlocks++;
			r->freeList = NULL; /* nothing on freeList yet */
			AddToFreeList(r);	
		}
	}
	return(blockArray);
}

/*C put records of just allocated block in freeList */
static void AddToFreeList(RECMEM_HEAP *r)
{
	char *b;  /* pointer in buffer */
	size_t i;

	PRECOND(r->freeList == NULL);
	PRECOND(r->nrBlocks > 0);

	r->freeList = (void *)r->blocks[r->nrBlocks-1]; /* first record in block */

	b = (char *)r->freeList;  /* start of block */
	for (i = 0; i < ((r->blockSize-1) * r->recSize); i+= r->recSize)
		((RECMEM_LINK *)(b+i))->next = (RECMEM_LINK *)(b+i+r->recSize);
        ((RECMEM_LINK *)(b+i))->next = NULL;
}


#ifdef DEBUG

/* checks if m is a pointer that is allocated in the proper way 
 * VerifyRecPtr check if memory is returned from NewRecord() and is
 * currently in use. It is only declared in DEBUG mode. Typical use is
 * PRECOND(VerifyRecPtr(m, heap)). If FALSE is returned a message to stderr
 * is printed.
 *
 * Returns 
 * TRUE if valid, FALSE if not.
 */
BOOL VerifyRecPtr(
	void *m,
	RECMEM_HEAP *r)
{
	size_t i;
	RECMEM_LINK *p;

	for (i=0; i< r->nrBlocks; i++)
		if (    m >= r->blocks[i] && 
			((char *)m) < ((char *)r->blocks[i])+(r->blockSize*r->recSize))
	        {
			/* ok it's valid pointer */
			/* now check if it's not in the free list */
			for(p = (RECMEM_LINK *)r->freeList; p != NULL; p = p->next)
				if(p == m)
				{
					(void)fprintf(stderr,"Trace used record is on"
						"freeList!\n");
					return(FALSE); /* record is
							used: can't be in
							free list */
				}
			return(TRUE);	
		}
	(void)fprintf(stderr,"this pointer is not from this heap\n");
	return(FALSE);
}

#endif /* DEBUG */
