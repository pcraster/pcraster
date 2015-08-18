#define CHKMEM_C

#include "stddefx.h"

/**************************************************************************/
/*  chkmem.c                                                              */ 
/*    Functions to handle dynamic memory allocation                       */
/*                                                                        */
/*                                                                        */
/**************************************************************************/

/********/
/* USES */
/********/
#ifdef DEBUG_DEVELOP
#include <string.h>  /* memset */
#endif
#include "misc.h" 

/***************/
/* EXTERNALS   */
/***************/


/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/* struct used for ChkTmp functions (LIBRARY_INTERNAL)
 */
typedef struct TMP_NODE {
	struct TMP_NODE *next;
} TMP_NODE;


static void PrintNoCore(void);
static int  NoReleaserInstalled(void);

#ifdef DEBUG_DEVELOP
# ifdef LIMITMALLOC
#  undef free
#  undef malloc
#  undef realloc
#  undef calloc
# endif
static BOOL stopLimitMalloc = FALSE;
static int nrMallocs=0;
static int limitMalloc=0;
#endif

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 
/* function that is called if there
   is no more memory */ 

static void (* NoMoreMemory)(void) = PrintNoCore;
static int  (* TryRelease)(void) = NoReleaserInstalled;
static TMP_NODE *tmpList = NULL;
/******************/
/* IMPLEMENTATION */
/******************/

#ifdef DEBUG_DEVELOP
	/* wrapper around free
	 * This wrapper is only active if DEBUG_DEVELOP is defined
	 * Free is a macro otherwise.
	 */
	void Free(void *ptr)
	{
		if (ptr == NULL)
			return;
		if (limitMalloc)
		{
		  if (nrMallocs < 0)
		  { exitOnError = 128;
		    Error("call to free (or Free) while malloc counter = 0");
		  }
		  nrMallocs--;
		}
		free(ptr);
	}
#endif

#ifdef __C2MAN__
typedef int entityType;


/* macro that calls ChkMalloc with proper typecasts
 * macro that calls ChkMalloc with proper typecasts
 * 
 * CHK_MALLOC_TYPE(type,nr)   ( (type *)ChkMalloc(sizeof(type)*((size_t)(nr))))
 */
entityType *CHK_MALLOC_TYPE(
	entityType t, /* type of entity
	               */
	size_t nr) /* number of entities
	            */
{
  /* MACRO DOCUMENTATION */
}
#endif


static void DoNoMoreMemory(void)
{
	TMP_NODE *n,*t = tmpList;
	while(t != NULL) 
	{
		n = t->next;
		Free(t);
		t = n;
	}
	if (NoMoreMemory != NULL)
		NoMoreMemory();
}

static void PrintNoCore(void)
{
	Error("Not enough memory\n");
}

/* Dummy memory releaser
 * 
 */
static int  NoReleaserInstalled(void)
{
	return(0);
}


/* register function to call on allocation failure
 * ChkRegisterNoMoreMemory stores a function ptr that
 * will be called if allocation fails (after the function
 * passed to ChkRegisterTryReleaseMemory has failed)
 */
void ChkRegisterNoMoreMemory(
 	void (* f)(void)) /* function to call at allocation failure */
{
	NoMoreMemory = f;
}

/* register function to call on allocation failure for getting memory
 * The function passed to ChkRegisterTryReleaseMemory tries to free
 * space that is returned to the global heap managed by the default
 * memory manager (malloc, realloc, etc.). It should return non-zero
 * if it succeeds to free up memory and zero if it fails.
 */
void ChkRegisterTryReleaseMemory(
 	int (* f)(void)) /* function to call to release memory */
{
	TryRelease = f;
}

/* wrapper around malloc()
 * Allocates a block. If the misc library is compiled with
 * -DDEBUG_DEVELOP then each byte of the block is set to a value 124.
 * This results in the following values for data types
 * 
 * double 4.44172e+291
 * 
 * float  5.24393e+36
 * 
 * int  2088533116
 * 
 * short  int  31868
 * 
 * ptr  7b033390 (HP) or 0x7c7c7c7c (Linux)
 * 
 * char  |
 * returns
 * 
 * pointer to memory block or
 * 
 * NULL, if there is not enough memory, after Error() is called
 */
void *ChkMalloc(
	size_t size) /* size in bytes, larger than 0 */
{
	void *b;

#ifdef DEBUG_DEVELOP
	if (size == 0)
#endif
		PRECOND(size > 0);

#ifdef DEBUG_DEVELOP
	if(limitMalloc)
	{
		if (nrMallocs >= limitMalloc)
	 	{
	   		DoNoMoreMemory();
	 		return NULL;
	        }
	}
#endif

	while ( (b = malloc(size)) == NULL)
 	 if (!TryRelease())
	 {
	   DoNoMoreMemory();
	   return NULL;
	 }
#ifdef DEBUG_DEVELOP
	nrMallocs++;
	if (b != NULL)
		(void)memset(b,124,size);
        /* test program for value picking (here 124)
	 * is at the bottom of this file
	 */
#endif
	return(b);
}

/* wrapper around calloc()
 * Allocates an array of elements. 
 * returns
 * 
 * pointer to memory block or
 * 
 * NULL, if there is not enough memory, after Error() is called
 */
void *ChkCalloc(
	size_t nnemb, /* number of elements.larger than 0 */
	size_t size) /* size of each element in bytes, larger than 0 */
{
	void *b;


#ifdef DEBUG_DEVELOP
	if (size == 0)
#endif
		PRECOND(size > 0);

#ifdef DEBUG_DEVELOP
	if(limitMalloc)
	{
		if (nrMallocs >= limitMalloc)
	 	{
	   		DoNoMoreMemory();
	 		return NULL;
	        }
	}
#endif

	while ( (b = calloc(nnemb,size)) == NULL)
 	 if (!TryRelease())
	 {
	   DoNoMoreMemory();
	   return NULL;
	 }
#ifdef DEBUG_DEVELOP
	nrMallocs++;
#endif
	return(b);
}

#ifdef DEBUG_DEVELOP
/* stop test environment for allocation failure
 * StopLimitMalloc checks if all allocations between StartLimitMalloc
 * and this call are mirrored by a free call.
 * If not, an error message is printed and the program returns with an
 * exit status of 128.
 * StopLimitMalloc is always called on exit if StartLimitMalloc is
 * called and it is not already called.
 */
void StopLimitMalloc(void)
{
	if(limitMalloc && (! stopLimitMalloc))
	{
	  stopLimitMalloc = TRUE;
	  limitMalloc=0; 
	  if (nrMallocs < 0)
	  { exitOnError = 128;
	    Error("'%d' more calls to free than to"
		  " allocation functions", -nrMallocs);
	  }
	  if (nrMallocs > 0)
	  { exitOnError = 128;
	    Error("'%d' more calls to allocation functions than to free", nrMallocs);
	  }
	  if (nrMallocs == 0)
	  	(void)fprintf(stderr,
	  	 "OK: allocation and free functions are in balance\n");

	}
}

/* initialize test environment for allocation failure
 * StartLimitMalloc reads the env. variable LIMITMALLOC
 * that holds the number of allocations that must succeed. If
 * more allocation request are done then an allocation function will
 * fail. On exit() or StopLimitMalloc a check is done if all memory is
 * freed. This procedure is garantueed to work if all allocation request
 * are done through Chk-allocation function familly and deallocation
 * with Free. Addionately alloc, realloc and free are renamed in misc.h to
 * ChkMalloc, ChkRealloc and Free if the application or library 
 * is compiled with LIMITMALLOC defined
 * 
 * StartLimitMalloc exits with an exit code of 130 if the env. variable
 * LIMITMALLOC is not set or contains a non-positive value.
 */
	void StartLimitMalloc(void)
	{
		 char *s = getenv("LIMITMALLOC");
		 int atExitRet;
		 if (s == NULL)
		 {
		  exitOnError = 130;
		  Error("StartLimitMalloc is called while LIMITMALLOC"
		        " is not set");
		  }
		 limitMalloc = atoi(s);
		 nrMallocs = 0;
		 if (limitMalloc <= 0)
		 {
		  exitOnError = 130;
		  Error("StartLimitMalloc: LIMITMALLOC <= 0 : '%s'",s);
		 }
		 atExitRet = atexit(StopLimitMalloc);
		 POSTCOND(atExitRet == 0);
	}
#endif

/* Wrapper around ChkMalloc for 'temporary' allocation
 * ChkTmpMalloc is designed to ease proper memory allocation
 * and deallocation in a sequence of allocations. If a
 * successive call to ChkMalloc(), ChkRealloc() or
 * ChkTmpMalloc() fails then all memory allocated through
 * ChkTmpMalloc()-calls is deallocated in that call. Thus, an
 * explicit call to ChkTmpFree() is not necessary in the error
 * handling of such a code sequence.  Normal deallocation 
 * must be done with ChkTmpFree().
 * returns
 * 
 * pointer to memory block or
 * 
 * NULL, if there is not enough memory, after Error() is called
 *
 * WARNING
 * use nothing else but ChkTmpFree to deallocate memory
 * returned by ChkTmpMalloc.
 */ 
void *ChkTmpMalloc(
	size_t size) /* size in bytes, larger than 0 */
{
	TMP_NODE *b;


#ifdef DEBUG_DEVELOP
	if (size == 0)
#endif
		PRECOND(size > 0);

       /* allocate block preceeded by a TMP_NODE structure
	*/
	b = (TMP_NODE *)ChkMalloc(sizeof(TMP_NODE) + size );
	if (b == NULL)
		return NULL;

       /* add TMP_NODE pointer to list */
	b->next = tmpList;
	tmpList = b;

	/* return pointer to block
	*/
	return((void *)(((char *)b) + sizeof(TMP_NODE)));

} /* ChkTmpMalloc */


/* deallocates memory allocated through a call to ChkTmpMalloc
 */ 
void ChkTmpFree(
	void *v)  /* Destructed. Pointer returned by ChkTmpMalloc */
{
        /* TMP_NODE lies before the block 
         */
	TMP_NODE *n = (TMP_NODE *)((char *)v - sizeof(TMP_NODE));
	TMP_NODE *t = tmpList;

	/*
	 * remove node from list
	 */
	if (t == n)	/* first node */
		tmpList = n->next;
	else {
		while(t != NULL && t->next != n)
			t = t->next;
		PRECOND(t != NULL);
		t->next = n->next;
	}

	Free(n);

} /* ChkTmpFree */

/* wrapper around realloc()
 * Read ChkMalloc(misc) for details.
 * returns
 * 
 * pointer to reallocated memory block or
 * 
 * NULL, if there is not enough memory, after Error() is called
 */
void *ChkRealloc(
	void *ptr,   /* pointer to old block */
	size_t size) /* new size in bytes, larger than 0 */
{
	void *b;
	
#ifdef DEBUG_DEVELOP
	if (size == 0)
#endif
		PRECOND(size > 0);
		
#ifdef DEBUG_DEVELOP
	if (limitMalloc && ptr == NULL) 
	{
		if (nrMallocs >= limitMalloc)
	 	{
	   		DoNoMoreMemory();
	 		return NULL;
	        }
	}
#endif
	while ( (b = realloc(ptr, size)) == NULL)
	 if (!TryRelease())
	 {
	   DoNoMoreMemory();
	   return NULL;
	 }
#ifdef DEBUG_DEVELOP
	if (limitMalloc && ptr == NULL)
		nrMallocs++;
#endif
	return(b);
} /* ChkRealloc */

/* reallocate block, and free in case of failure
 * ChkReallocFree reallocates a memory block and
 * frees the old block in case of an error.
 * RETURNS 1 if succesfull, 0 if a memory error occurred
 */
int ChkReallocFree(
	void **ptr, /*  read-write, ptr to address of 
	             *  block to be modified, set to NULL
	             * in case of an error.
	             */
	size_t n)  /* new size of block.
	            */
{
	void *new = ChkRealloc(*ptr, n);
	if (new == NULL)
	{
		Free(*ptr);
		*ptr = NULL;
		return 1;
	}
	*ptr = new;
	return 0;
}

#ifdef __C2MAN__

/* macro that calls Free, and set x to NULL
 * macro that calls Free, and set x to NULL
 * 
 * FREE_NULL(x)  Free(x), x = NULL
 */
void FREE_NULL(
	void *x) /* memory block to free, pointer is set to NULL
	          */
{
  /* MACRO DOCUMENTATION */
}

#endif

#ifdef NEVER_DEF_THIS

# include <stdio.h>
# include <string.h>

	void main(void)
	{
	   int l;
		
	   for(l =0; l < 256; l++)
	   {
		double buf;
		float  *f = (float *)&buf;
		int    *i = (int *)&buf;
		short int    *s = (short int *)&buf;
		void   *p = (void *)&buf;
		char   *c = (char *)&buf;
		
		(void)memset((void *)&buf, l, sizeof(double));
		

		(void)printf("l = %d\n", l);
		(void)printf(" double %g\n", buf);
		(void)printf(" float  %g\n", (float)(*f));
		(void)printf(" int  %d\n", *i);
		(void)printf(" short  int  %d\n", *s);
		(void)printf(" ptr  %p\n", p);
		(void)printf(" char  %c\n", *c);
	   }
	}


 #include <malloc.h>

/*
 * piece of code t debug malloc/free issues on gcc
 * C++ new and delete are implemnted with malloc/free
 */

default_malloc_hook = __malloc_hook; __malloc_hook = TraceMallocHook;
default_free_hook   = __free_hook;   __free_hook   = TraceFreeHook;

static int nrMallocs=0;

typedef void *(*MALLOC_HOOK)(size_t s, const void *ad);
extern MALLOC_HOOK __malloc_hook;
static MALLOC_HOOK default_malloc_hook;
static void *TraceMallocHook(size_t s, const void *ad) {
 __malloc_hook = default_malloc_hook;
 void *ptr = malloc(s);
 if (s > 450000) {
  nrMallocs++;
  printf("malloc %p %u %08d\n",ptr,malloc_usable_size(ptr),nrMallocs);
 }
 __malloc_hook = TraceMallocHook;
 return ptr;
}

typedef void (*FREE_HOOK)(void *ptr, const void *ad);
extern FREE_HOOK __free_hook;
static FREE_HOOK default_free_hook;

static void TraceFreeHook(void *ptr, const void *ad) {
 size_t s = malloc_usable_size(ptr);
 if (s > 450000) {
  nrMallocs--;
  printf("free %p %u %08d\n",ptr,malloc_usable_size(ptr),nrMallocs);
 }
 __free_hook = default_free_hook;
 free(ptr);
 __free_hook = TraceFreeHook;
}
#endif
