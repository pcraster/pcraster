
#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h> /* memcpy */
#include "misc.h"
#include "app.h"
#include "table.h"

/* global header (opt.) and test's prototypes "" */
#include "table_p.h"

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
#define LEX_EOL LEX_FIRST_UNUSED_TOKEN_VALUE

/******************/
/* IMPLEMENTATION */
/******************/

static int SetNumber(
  LOOK_UP_KEY *k,
  BOOL low,
  CSF_VS vs)
{
  const char *sv = LexGetTokenValue();
  double *v = low ? &(k->l) : &(k->h);
  CnvrtDouble(v,sv);
  if (AppCheckVal(sv, vs,CR_UNDEFINED))
    k->t = TEST_ERROR;
  return k->t == TEST_ERROR;
}

static int IllegalState(
  LOOK_UP_KEY *k,
  int token,
  const char *legals) /* number must be first one ! */
{
  k->t = TEST_ERROR;
  if (!LexError(token))
  {
    const char *p = "end of line";
    const char *num = "";
    const char *l = legals;
    const char *one = " one of";
    if (legals[0] == '$')
    {
      l++;
      num = " or a number";
    }
    /* PRECOND(strlen(l) >= 1);  always a symbol */
    if (l[1] == '\0')
      one = "";

    if (token != LEX_EOL)
      p = LexGetTokenValue();
    (void)ErrorNested("read '%s' expected%s '%s'%s",p,one,l,num);
  }
  return 1;
}

/* Parses a key, checks whether or not it is legal.
 * Returns
 * 1 if the key is illegal (a nested error message is printed)
 * , 0 if succesfull.
 */
static int ParseKey(
  LOOK_UP_KEY *k,  /* write-only key, if k->t == TEST_NOKEY
                   * then the end of file is reached
                   */
   CSF_VS vs)  /* value scale */
 {
      typedef enum STATE { STATE_START, STATE_LOWNUM, STATE_COMMA,
             STATE_HIGHNUM, STATE_HIGHTOKEN } STATE;
      STATE state = STATE_START;
      int t; /* token */
      long    startLineNr = LexGetLineNr();
  while(1)
  {
      t = LexGetToken();
      if (t >= 0 && LexGetLineNr() != startLineNr)
      {
        if (state == STATE_START) /* parsed empty line */
          startLineNr = LexGetLineNr();
        else
          t = LEX_EOL;
      }
      switch(state)
      {
    case STATE_START: switch(t) {
       case LEX_NUMBER:
      k->t = TEST_ONE;
          if (SetNumber(k, TRUE, vs))
            return 1;
          return 0;
           case '[' :
                 k->t = TEST_GE_INF;
                 state = STATE_LOWNUM;
                 break;
           case '<' :
                 k->t = TEST_GT_INF;
                 state = STATE_LOWNUM;
                 break;
           case 0   :
                 k->t = TEST_NOKEY;
                 return 0;
           default : return IllegalState(k,t,"$[<");
        } break;
        case STATE_LOWNUM:
     PRECOND(k->t == TEST_GE_INF || k->t == TEST_GT_INF);
         switch(t) {
       case LEX_NUMBER:
          if (SetNumber(k, TRUE, vs))
            return 1;
          state = STATE_COMMA;
          break;
       case ',':
      k->t = TEST_INF_INF;
          state = STATE_HIGHNUM;
          break;
           default : return IllegalState(k,t,"$,");
         } break;
        case STATE_COMMA:
          if (t != ',')
            return IllegalState(k,t,",");
          state = STATE_HIGHNUM;
          break;
        case STATE_HIGHNUM:
         POSTCOND(k->t==TEST_GE_INF||k->t==TEST_GT_INF ||k->t==TEST_INF_INF);
          switch(t) {
       case LEX_NUMBER:
          if (SetNumber(k, FALSE, vs))
            return 1;
          state = STATE_HIGHTOKEN;
          if (k->t != TEST_INF_INF && (k->l > k->h) )
          {
           k->t = TEST_ERROR; // pcrcalc/test69
           return RetErrorNested(1,"low value ('%g') of range larger than high value ('%g')", k->l,k->h);
          }
          break;
           case ']':
           case '>':
                 /* already set, by choosing
                  * intermediate states of k->t
                  */
                return 0;
           default : return IllegalState(k,t,"$]>");
          }break;
    case STATE_HIGHTOKEN:
         POSTCOND(k->t==TEST_GE_INF||k->t==TEST_GT_INF ||k->t==TEST_INF_INF);
         switch(t) {
           /* inc over enums, that's why particular order */
           case ']':
             k->t += 3;
             return 0;
           case '>':
             k->t += 6;
             return 0;
           default : return IllegalState(k,t,"]>");
         } break;
      } /* eoswitch state */
      } /* eowhile */
}

static size_t DetectNrColsTable(
  FILE *tableFile)  /* file to read */
{
  LOOK_UP_KEY k;
  long l =0;
  int nrCols=0;
  LexInstall(tableFile,  "[]<>,");
  while( (!ParseKey(&k, VS_UNDEFINED))
          && !TEST_NOKEYREAD(k.t)
       )
  {
       if (nrCols == 0)
    l = LexGetLineNr();
       else
       {
    PRECOND(l!=0);
    if ( l != LexGetLineNr())
     break;
       }
       nrCols++;
  }
  if (k.t == TEST_ERROR)
   return RetErrorNested(0,"line '%ld' column '%d':",l,nrCols+1);
  if (k.t == TEST_NOKEY && nrCols == 0)
   return RetErrorNested(0,"no columns found");
  rewind(tableFile);
  return nrCols;
}

/* kk
 * returns allocated list of records, NULL in case of error (stored
 * in ErrorNested
 */
static LOOK_UP_KEY *ReadLookupRecs(
  size_t  *nrCols,  /* write-only */
  size_t  *nrRecs,  /* write-only */
  size_t  nrKeysExp,/* numbers of keys expected, only relevant
                  * if vsTargetValue != VS_UNDEFINED
                  * this is exclusive the targetColumn
                  */
  CSF_VS vsTargetVal, /* if VS_UNDEFINED then skip test on exact number
                      * of columns (= nrKeysExp+1)
                      */
  FILE *tableFile)
{
  size_t n,nrK,c = DetectNrColsTable(tableFile);
  LOOK_UP_KEY *k = NULL;
  long recStartAt=0;
  if (c == 0)
    goto error;

  if (c == 1 && vsTargetVal != VS_UNDEFINED )
  {
   ErrorNested("only 1 column found");
   return NULL;
  }

  if (vsTargetVal != VS_UNDEFINED && c != (nrKeysExp+1))
  {
   ErrorNested("contains %s columns ('%d' read,'%d' expected)",
    (c < (nrKeysExp+1) ? "not enough" : "too many"), c, nrKeysExp+1);
   goto error;
  }

  LexInstall(tableFile,  "[]<>,");
  for (nrK = n = 0; /* break from code */ ; n++)
  {
    CSF_VS vs = VS_UNDEFINED;
    if (n == nrK)
    {
      nrK += 40;
      if (ChkReallocFree((void **)&k,nrK * sizeof(LOOK_UP_KEY)))
        return NULL;
    }
    if (vsTargetVal != VS_UNDEFINED && (n%c) == nrKeysExp)
     vs = vsTargetVal;
    ParseKey(k+n, vs);
    if (k[n].t == TEST_NOKEY) {
     if ( (n%c)==0 )
      break;
     else
          goto notEnough;
    }
    if (k[n].t == TEST_ERROR)
    {
      ErrorNested("while reading at line '%ld' column '%d'",
      LexGetLineNr(), (n%c)+1);
      goto error;
    }
    if (vsTargetVal != VS_UNDEFINED && (n%c) == nrKeysExp
        && k[n].t != TEST_ONE )
    {
      ErrorNested(
      "value field at line '%ld' column '%d' is not a single value"
      , LexGetLineNr(), (n%c)+1);
      goto error;
    }
    if ( (n%c)==0) /* start new record */
    {
     if (recStartAt == LexGetLineNr()) /* prev had more */
     {
      ErrorNested(
      "Too many columns on line '%ld', expected '%d' columns",
      recStartAt,c);
      goto error;
     }
     recStartAt = LexGetLineNr();
          }
          else /* add to this record */
    {
     if (recStartAt != LexGetLineNr()) /* this does not have enough */
     {
notEnough:
      ErrorNested(
      "Not enough columns on line '%ld', expected '%d' columns",
      recStartAt,c);
      goto error;
     }
    }
  } /* eofor */
  POSTCOND( (n % c) == 0);
  *nrCols = c;
  *nrRecs = n/c;
  return k;
error:
  Free(k);
  return NULL;
}

static int CheckMatrixCell(
  size_t row,  /* 1-based index */
  size_t col,  /* 1-based index */
  const LOOK_UP_KEY *k,
  CSF_VS vs)
{
  if (k->t != TEST_ONE)
    return RetErrorNested(1,
   "value field in matrix at row '%ld' column '%d' is not a single value"
    , row, col);
  if (vs != VS_UNDEFINED)
    if (AppCheckValNum(k->l,vs,CR_UNDEFINED))
     return RetErrorNested(1, "value field in matrix at row '%ld' column '%d'"
        , row, col);
  return 0;
}

static void CnvrtDirectional(
  double *val)
{
  if (*val != -1)
    *val = AppInputDirection(*val);
}

/* read a lookup or cross table
 * ReadLookupTable reads a table in matrix or table format.
 * The exact number of columns is not checked, unless outputVs
 * not equal to VS_UNDEFINED.
 * The number of actual columns read is plus one for the key column
 * Errors are printed in ErrorNested.
 * Returns the lookup table or NULL in case of an error.
 */
LOOK_UP_TABLE *ReadLookupTable(
  FILE *f,            /* the tablefile */
  const CSF_VS *keyVs, /* valuescales of key columns,
                       * undefined if outputVs == VS_UNDEFINED
                       */
  size_t  nrKeys,      /* > 0
                       * undefined if outputVs == VS_UNDEFINED
                       */
  CSF_VS outputVs)     /* VS_UNDEFINED if we don't care */
{
  size_t r,c,nrCols, nrRecs;
  LOOK_UP_KEY *k;
  LOOK_UP_TABLE *t;
  size_t nrColsExp;
  BOOL matrRead = (nrKeys == 2 && app2dMatrix);
  k = ReadLookupRecs(&nrCols, &nrRecs, nrKeys,
                      matrRead ? VS_UNDEFINED : outputVs, f);
  if (outputVs == VS_UNDEFINED)
  {
          nrColsExp = nrCols;
    nrKeys = nrCols-1;
    keyVs = NULL;
  }
  else
        nrColsExp = nrCols;
  if (k == NULL || (t = ChkMalloc(sizeof(LOOK_UP_TABLE))) == NULL)
    return NULL;
  t->nrKeys = nrKeys;
  t->keyVs = NULL; t->records = NULL;
  t->searchMethod = SEARCH_LINEAR;

  if (matrRead)
  { /* build lookup from matrix and check matrix output/target values */
    size_t i;
    t->nrRecords = (nrRecs-1)*(nrCols-1);
    t->nrMatrCols = nrCols-1;
    t->nrKeys = 2; /* CW */
    if (t->nrRecords <= 0)
    {
      ErrorNested("matrix must have at least 2 rows and 2 columns");
      goto error;
    }
    if (AllocLookupTable(t))
      goto error;
    for (r=0,i=0; i < nrRecs*nrCols; i++)
     if ( (i/nrCols) != 0     /* not first row */
         && (i%nrCols) != 0 ) /* not first col */
      {
    /* col index is key 0 */
    t->records[r][0] = k[i%nrCols];
    /* row index is key 1 */
    t->records[r][1] = k[nrCols*(i/nrCols)];
    /* outputValue */
    t->records[r++][2] = k[i];
    if (CheckMatrixCell(i/nrCols,i%nrCols,k+i,outputVs))
      goto error;
      }
    POSTCOND(r == t->nrRecords);
  }
  else /* table (not matrix) */
  {
    t->nrRecords = nrRecs;
    if (nrColsExp > nrCols)
    {
      ErrorNested("not enough columns, read '%d' expected '%d' columns",
                  nrCols, nrColsExp);
      goto error;
    }
    if (AllocLookupTable(t))
      goto error;
    for(r=0; r < nrRecs; r++)
     (void)memcpy(t->records[r],k+(nrCols*r),nrColsExp*sizeof(LOOK_UP_KEY));
  }

  Free(k);
  if (keyVs != NULL)
   (void)memcpy(t->keyVs, keyVs, t->nrKeys*sizeof(CSF_VS));
  else for (r = 0; r < t->nrKeys; r++)
        t->keyVs[r] = VS_UNDEFINED;
  t->keyVs[t->nrKeys] = outputVs;

  /* adjust for directional
   */
   nrCols = nrKeys + (outputVs != VS_UNDEFINED);
   for (c=0; c<nrCols; c++)
   if (t->keyVs[c] == VS_DIRECTION)
    for (r=0; r<t->nrRecords; r++)
    {
      if (LOW_DEFINED(t->records[r][c].t))
       CnvrtDirectional(&(t->records[r][c].l));
      if (HIGH_DEFINED(t->records[r][c].t))
       CnvrtDirectional(&(t->records[r][c].h));
    }
  return t;
error:
  Free(k);
  FreeLookupTable(t);
  return NULL;
}


#ifdef NEVER
/*
 * int main(
 *   int argc,
 *   char **argv
 *   )
 * {
 *   FILE *f;
 *   LOOK_UP_TABLE *t;
 *   CSF_VS vs[3] = {VS_SCALAR,VS_SCALAR, VS_NOMINAL};
 *   double keys[3][3] =
 *   {
 *    { 4 , 3, 2 },
 *    { -1 , 3, 3 },
 *    { 6 , 3, 1 }
 *   };
 *   int r=0;
 *   double res;
 *
 *   PRECOND(argc >= 2);
 *
 *   f = fopen(argv[1],"r");
 *   POSTCOND(f != NULL);
 *   app2dMatrix = TRUE;
 *   if ( (t=ReadLookupTable(f,vs,2, VS_SCALAR)) == NULL)
 *   {
 *     Error("While parsing lookup table '%s':",argv[1]);
 *     exit(1);
 *   }
 *   printf("%s contains %d keys and %d recs\n",
 *      argv[1],t->nrKeys,t->nrRecords);
 *   for (r=0; r < 0; r++)
 *   {
 *    if ( Lookup(&res, t, keys[r]) )
 *      printf(" %g %g %g -> %g\n", keys[r][0],
 *                                  keys[r][1],
 *                                  keys[r][2],
 *                                  res);
 *    else
 *      printf(" %g %g %g -> NO\n", keys[r][0],
 *                                  keys[r][1],
 *                                  keys[r][2]);
 *   }
 *   WriteLookupTable(argv[2],t);
 *   FreeLookupTable(t);
 *   exit(0);
 * }
 */
 #endif
