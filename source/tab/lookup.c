#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <search.h>
#ifdef _MSC_VER
#define lfind _lfind
#endif
#include "misc.h"
#include "table.h"

/* global header (opt.) and test's prototypes "" */

/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

/* typedef for search functions (LIBRARY_INTERNAL)
 */
typedef LOOK_UP_KEY **(*SEARCH)( const void *key, const void *base,
      size_t nmemb, size_t size, QSORT_CMP cmp);

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/

/* Get number of keys in look up table
 * Returns number of keys in look up table
 */
int NrKeysLookupTable(
  const struct LOOK_UP_TABLE *table) /* table */
{
  PRECOND(table != NULL);
  return table->nrKeys;
}

static int TestOne   (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_ONE);

  return  CmpDouble(v,&(k->l));
}

/* ARGSUSED */
static int TestInfInf(const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_INF_INF);

  (void)v;// Shut up compiler
  (void)k;// Shut up compiler

  return 0;
}

static int TestGeInf (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GE_INF);
  if (*v < k->l)
    return -1;
  return 0;
}

static int TestGtInf (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GT_INF);
  if (*v <= k->l)
    return -1;
  return 0;
}

static int TestInfLe (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_INF_LE);
  if (*v > k->h)
    return 1;
  return 0;
}

static int TestGeLe  (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GE_LE);
  if (*v < k->l)
    return -1;
  if (*v > k->h)
    return 1;
  return 0;
}

static int TestGtLe  (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GT_LE);
  if (*v <= k->l)
    return -1;
  if (*v > k->h)
    return 1;
  return 0;
}

static int TestInfLt (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_INF_LT);
  if (*v >= k->h)
    return 1;
  return 0;
}

static int TestGeLt  (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GE_LT);
  if (*v < k->l)
    return -1;
  if (*v >= k->h)
    return 1;
  return 0;
}

static int TestGtLt  (const double *v, const LOOK_UP_KEY *k)
{
  PRECOND(k->t == TEST_GT_LT);
  if (*v <= k->l)
    return -1;
  if (*v >= k->h)
    return 1;
  return 0;
}

static int cmpNrKeys;
static int CmpKey(
  const double *keyValues,
  const LOOK_UP_KEY **keys)
{
  int i,r=0;
  const QSORT_CMP Cmp[] = {
   (QSORT_CMP)TestOne   ,
   (QSORT_CMP)TestInfInf,
   (QSORT_CMP)TestGeInf ,
   (QSORT_CMP)TestGtInf ,
   (QSORT_CMP)TestInfLe ,
   (QSORT_CMP)TestGeLe  ,
   (QSORT_CMP)TestGtLe  ,
   (QSORT_CMP)TestInfLt ,
   (QSORT_CMP)TestGeLt  ,
   (QSORT_CMP)TestGtLt  };
  PRECOND(cmpNrKeys > 0 );
  for(i = cmpNrKeys-1; i >= 0;  i--) {
    const LOOK_UP_KEY *k = (*keys)+i;
    PRECOND(k->t < ARRAY_SIZE(Cmp));
    r = Cmp[k->t](keyValues+i, k);
    if (r)
      break;
  }
  POSTCOND( (r==0 && i == -1) || (r!=0 && i >= 0) );
  return r;
}

/* search table to find a key
 * Applies binary or linear search at a lookup table.
 * The first key that fits is returned (in case of linear search)
 * returns the key or NULL if not found
 */
LOOK_UP_KEY *FindLookupKey(
    const LOOK_UP_TABLE *t,   /* lookup table */
    const double *keyValues) /* values to match, no MV's allowed
                              */
{
  LOOK_UP_KEY **k;
  size_t nr= (size_t)(t->nrRecords);
  PRECOND(t->searchMethod == 1 || t->searchMethod == 0);
  cmpNrKeys = t->nrKeys;
  if (t->searchMethod==0)
  {
    k = lfind(keyValues,t->records,
              &nr, sizeof(LOOK_UP_KEY *), (QSORT_CMP)CmpKey);
  } else {
    k = bsearch(keyValues,t->records,
              nr, sizeof(LOOK_UP_KEY *), (QSORT_CMP)CmpKey);
  }
  if (k != NULL)
     return *k;
  return NULL;
}

size_t FindCrossKey(
    const LOOK_UP_TABLE *t,   /* lookup table */
    const double *keyValues, /* values to match, no MV's allowed
                              */
    size_t   startHere)         /* index to start from
                              */
{
  // SEARCH m[2] = { (SEARCH)lfind,(SEARCH)bsearch};
  LOOK_UP_KEY **k;
  size_t nr= (size_t)(t->nrRecords-startHere);
  PRECOND(t->searchMethod == 1 || t->searchMethod == 0);
  cmpNrKeys = t->nrKeys;
  if (t->searchMethod==0)
  {
    k = lfind(keyValues,t->records+startHere,
              &nr, sizeof(LOOK_UP_KEY *), (QSORT_CMP)CmpKey);
  } else {
    k = bsearch(keyValues,t->records+startHere,
              nr, sizeof(LOOK_UP_KEY *), (QSORT_CMP)CmpKey);
  }
  if (k != NULL)
      return (size_t)(k-(t->records));
  return t->nrRecords;
}

/* Determines the output value according to look up table and keys.
 * The keyvalues are checked on fitting a searchkey in the lookup
 * table. If this is the case the according value is put in result.
 * returns 1 if match found, 0 if no match found
 */
int Lookup(  double *result,    /* write-only, MV if no match */
    const LOOK_UP_TABLE *t,  /* lookup table */
    const double *keyValues)/* values to match, no MV's allowed
                             */
{
  LOOK_UP_KEY *k = FindLookupKey(t,keyValues);
  if (k == NULL) {
   SET_MV_REAL8(result);
   return 0;
  } else {
   PRECOND(k[t->nrKeys].t == TEST_ONE);
   *result = k[t->nrKeys].l;
   return 1;
  }
}
