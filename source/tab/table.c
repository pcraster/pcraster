#include "stddefx.h"

/********/
/* USES */
/********/
/* libs ext. <>, our ""  */
#include <string.h>  /* memmove */
#include "misc.h"
#include "table.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */ 

/***************/
/* EXTERNALS   */
/***************/

#ifdef DEBUG_DEVELOP
/* nr of table allocated
 * Only declared in DEBUG_DEVELOP mode
 */
int nrSearchTables = 0;
#endif

/*********************/ 
/* LOCAL DEFINITIONS */
/*********************/ 

/**********************/ 
/* LOCAL DECLARATIONS */
/**********************/ 

/******************/
/* IMPLEMENTATION */
/******************/

/* insert a record in a sorted array with unique ids
 * InsertSorted copies key to the appropriate place, moving
 * other elements if necessary. Therefore the array must
 * have space for one more element. NOTE that no records,
 * including the key (the new one),
 * must be equal (all unique id's)
 * returns the copied record (part of array)
 */
static void *InsertSorted(
 const void *key,    /* key to be inserted */
       void *base,   /* array of num+1 elements
                      * with element num being vacant
                      */
       size_t num,   /* number of elements initially in
                      * base
                      */
       size_t width, /* sizeof element */
       QSORT_CMP cmp)/* comparisson function */
{
 int x=0; /* num == 0 case */
 int c,l=0;
 int r=num-1;
 if (num == 0)
   goto done;
 do {
      x = (l+r)/2;
      if ( (c = cmp(key, ((char *)base)+(x*width))) < 0)
       r = x-1;
      else
       l = x+1;
       } while (( c != 0) && l <= r );
       POSTCOND(c != 0); /* NOT FOUND */
       if (c > 0)
           x++; /* insertion point is after x */
       PRECOND(x <= (int)num);
       if (x != (int)num) /* no memmove if insertion point is at end */
        /* move part of array after insertion point 1 to the right */
        (void)memmove( (((char *)base)+((x+1)*width)),
          (((char *)base)+(x*width)),
          (num-x)*width );
done:
       return memcpy((((char *)base)+(x*width)), key, width);
}

/* Free search table
 * Free search table. No action if table is NULL.
 * Returns nothing.
 *
 */
void STfree(SEARCH_TABLE *table) /* table to deallocate */
{
 if(table == NULL)
  return;
 if(table->slowList != NULL)
  free(table->slowList);
 free(table->fastList);
 free(table);
}

void STforAll(
 SEARCH_TABLE *t, /* table to deallocate */
 ACTION_REC   action) /* */
{
 size_t i;

 for(i = 0; i< t->nrSlowList; i++)
  action(((char *)(t->slowList))+(i*t->recSize));
 for(i = 0; i< t->nrFastList; i++)
  action(((char *)(t->fastList))+(i*t->recSize));
}

void STfreeAction(
 SEARCH_TABLE *t, /* table to deallocate */
 ACTION_REC   action) /* */
{

 if (t!=NULL)
 {
  STforAll(t, action);
  STfree(t);
 }
}

void *STsearch(
 const SEARCH_TABLE *t, /* table to search */
 SEARCH_REC   f) /*  */
{
 size_t i,fastBegin;
 const void *best;

 PRECOND(t != NULL && f != NULL);

 if (t->nrSlowList != 0)
 {
  best = t->slowList;
  for(i = 1; i< t->nrSlowList; i++)
   best = f(best, (((const char *)(t->slowList))+(i*t->recSize)));
  fastBegin = 0;
 }
 else 
 {
  best = t->fastList;
  fastBegin = 1;
 }
 for(i = fastBegin; i< t->nrFastList; i++)
  best = f(best, (((const char *)(t->fastList))+(i*t->recSize)));
 return (void *)best;
}

/* Initializes a search table.
 * Returns pointer to initialized table.
 *
 */
SEARCH_TABLE *STnew(
 size_t nrFastList, /* nr. of elements in fast list
                          * these are indexed by their position.
                          * if > 0 then specify ReturnId.
                          */
 size_t recSize,  /* size of a record */
 RETURN_ID ReturnId, /* pointer to function that
                          * returns the id, can be NULL 
                          * if nrFastList is 0 or
                          * STinsertOrFind is never used
                          */
 INIT_REC InitRec,    /* pointer to function to
                       * initialize records in the
                       * fastlist or in the InsertOrFind call,
                       * can be NULL if nrFastList is 0 or
                       * STinsertOrFind is never used
                       */
 QSORT_CMP cmp)         /* pointer to compare function */
{
 SEARCH_TABLE *t;
 PRECOND(cmp != NULL);
#ifdef DEBUG
 if (nrFastList > 0)
  PRECOND(nrFastList > 0 && ReturnId != NULL && InitRec != NULL);
#endif 
 t = (SEARCH_TABLE *)ChkMalloc(sizeof(SEARCH_TABLE));
 if(t == NULL)
  return NULL;  /* memory allocation failed */
 t->nrFastList = nrFastList; 
 t->recSize = recSize; 
 /* init slowList here so STfree works in this function */
 t->slowList = NULL;  /* slowlist = empty */
 t->nrSlowList = 0;  /* nr. elements in slow list */
 t->ReturnId = ReturnId;
 t->cmp = cmp;   /* for binary search */
 t->InitRec = InitRec;
 if(nrFastList != 0)
 {
  size_t i;
  char *r;
  t->fastList = ChkMalloc(nrFastList * recSize);
  if(t->fastList == NULL)
  {
   STfree(t);
   return NULL;
  }
  r = t->fastList;
  for ( i = 0 ; i  < nrFastList; i++)
  { 
   InitRec((void *)r,(int)i);
   r += recSize;
  } 
 }
 else
  t->fastList = NULL;
# ifdef DEBUG_DEVELOP
  nrSearchTables++;
# endif
 return t;
}

/* Inserts a record with given id in the slow list.
 * This is done by using a binary search through the list and
 * extending the list.
 * Returns pointer to the new record or NULL if memory allocation fails
 *
 */
void *STinsert(
 SEARCH_TABLE *t,  /* read-write table */
 const void *f)   /* key to insert */
{
 void *c;
 t->nrSlowList++;
 c = (void *)ChkRealloc(t->slowList, t->nrSlowList*t->recSize);
 if(c == NULL)
  return NULL;
 t->slowList = c;
 return InsertSorted(f,t->slowList, t->nrSlowList-1,t->recSize,t->cmp);
}

/* Searches the table to find a given id.
 * Searches the table to find a given id.
 * Returns pointer to wanted record, NULL if not found
 *
 */
void *STfind(
 const SEARCH_TABLE *t,  /* read-write table */
 const void *record)  /* record to find */
{
 PRECOND(t != NULL);
 PRECOND(record != NULL);

 if(t->nrFastList != 0)
 {
  int id;
  PRECOND(t->ReturnId != NULL);
  id = t->ReturnId(record);
  if( 0 <= id && id < (int)t->nrFastList)
  { /* id is in fast list */ 
   return (void*) ((char *)t->fastList +
            (id * t->recSize));
  } 
 }

 if(t->nrSlowList == 0)
  return NULL;
 /* search id in slowList */
 return bsearch(record, t->slowList, t->nrSlowList, t->recSize, t->cmp);
}

void *STfindOrInsert(
       SEARCH_TABLE *t, /* read-write table */
 const void *r)  /* record to find */
{
 void *c = STfind(t,r);
 PRECOND(t->InitRec != NULL);
 PRECOND(t->ReturnId != NULL);
 if (c != NULL)
  return c;
 c = STinsert(t,r);
 if (c != NULL)
  t->InitRec(c,t->ReturnId(r));
 return c;
}
