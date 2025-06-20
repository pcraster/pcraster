/* DO NOT EDIT: CREATED FROM rancom.tem */
#line 1 "rancom.tem"
#include "stddefx.h"
/* vim: syntax=c
 */

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.) and rspatial's prototypes "" */
#include "misc.h"
#include "csftypes.h"
#include "api.h"
#include "api_p.h"


/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/

#define INSIDE_MAP(thisRow,thisCol,m)\
  ( 0 <= (thisRow) && (thisRow) < ((m)->nrRows) &&\
   0 <= (thisCol) && (thisCol) < ((m)->nrCols))
/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/


#ifdef DEBUG_DEVELOP
  /* tests if [r,c] is in map
   * used as PRECOND in the Put methods
   * returns TRUE if [r,c] is a map-co-ordinate
   *         FALSE otherwise
   */
  static void SpatialAndCoordInMapINT4(
    int    r,           /* row number */
    int    c,           /* column number */
          const MAP_INT4 *m)    /* map this function belongs to */
  {
   if(!m->spatial) {
    Error("EXTERNAL API:Called Put function on nonspatial object",r,c);
    exit(1);
   }
   if (!(INSIDE_MAP(r,c , m))) {
    Error("EXTERNAL API:Called Put or PutMV, (r,c) out of range (r,c)=%d,%d",r,c);
    exit(1);
   }
  }
#else
# define SpatialAndCoordInMapINT4(r,c,m)
#endif

static BOOL GetSpatialINT4(
  INT4 *v,      /* write-only. Value */
  int    r,      /* row number */
  int    c,      /* column number */
const  MAP_INT4 *m)  /* map this function belongs to */
{

  PRECOND(m != NULL);

  if (INSIDE_MAP(r,c,m)) {
    m->getPrivate(v, (const void **)m->spatialValue, r ,c);
    return TRUE;
  }
  return FALSE;
}

static BOOL GetSpatialMVtestINT4(
  INT4 *v,      /* write-only. Value */
  int    r,      /* row number */
  int    c,      /* column number */
const  MAP_INT4 *m)  /* map this function belongs to */
{
  if (GetSpatialINT4(v,r,c,m))
    return ! IS_MV_INT4(v);
  return FALSE;
}

static void PutMV_INT4(
  int    r,           /* row number */
  int    c,           /* column number */
  MAP_INT4 *m)   /* map this function belongs to */
{
  SpatialAndCoordInMapINT4(r,c,m);

  m->putMVPrivate(m->spatialValue,r,c);
}

static void PutAllMVINT4(
  MAP_INT4 *m)
{
  int r = 0;
  int c = 0;
  PRECOND(m->spatial);

  for (r=0; r < m->NrRows(m); r++)
   for (c=0; c < m->NrCols(m); c++)
     m->PutMV(r,c, m);
}

/*
 * static CSF_VS GetValueScaleINT4(
 *   const MAP_INT4 *m)
 * {
 *   return m->valueScale;
 * }
 *
 *
 * static CSF_CR GetInCellReprINT4(
 *   const MAP_INT4 *m)
 * {
 *   return m->inCellRepr;
 * }
 *static void CopyINT4(
 * const  MAP_INT4 *src,  // copy from this map
 *  MAP_INT4 *dest) // write-only, destination, this map will
 *                       // be filled with the values of the src map
 *                       //
 *{
 *  // not checked for new interface
 *  // ?? What new interface ?? CW
 *  int r,c;
 *  INT4 value;
 *  GET_TEST  state;
 *
 *  PRECOND(dest->spatial);
 *  PRECOND(dest->NrRows(dest) == src->NrRows(src));
 *  PRECOND(dest->NrCols(dest) == src->NrCols(src));
 *
 *  state = src->GetGetTest(src);
 *  src->SetGetTest(GET_MV_TEST, src);
 *  for (r=0; r < src->NrRows(src); r++)
 *   for (c=0; c < src->NrCols(src); c++)
 *     if (src->Get(&value, r, c, src))
 *         dest->Put(value, r, c, dest);
 *     else
 *         dest->PutMV( r, c, dest);
 *  src->SetGetTest(state, src);
 * }
 */

static void PutSpatialINT4(
  INT4 v,         /* Value */
  int    r,           /* row number */
  int    c,           /* column number */
        MAP_INT4 *m)    /* map this function belongs to */
{
  SpatialAndCoordInMapINT4(r,c,m);
#ifdef DEBUG_DEVELOP
  if (IS_MV_INT4(&v))
  {
   Error("EXTERNAL API:Called Put with MV (instead of PutMV) (r,c)=%d,%d",r,c);
   exit(1);
  }
#endif
  m->putPrivate(m->spatialValue, &v, r, c);
}

static int  NrRowsINT4(
  const MAP_INT4 *m)
{
  return (m->nrRows);
}

static int  NrColsINT4(
  const MAP_INT4 *m)
{
  return (m->nrCols);
}

/* ARGSUSED */
static void PutNonSpatialINT4(
  INT4 v,      /* Value */
  int    r,           /* row number */
  int    c,           /* column number */
  MAP_INT4 *m)   /* map this function belongs to */
{
  (void)v; // Shut up compiler
  (void)r; // Shut up compiler
  (void)c; // Shut up compiler
  (void)m; // Shut up compiler
  Error("Can't Put to a nonspatial INT4\n");
  POSTCOND(FALSE && r > c && v == 0 && m != NULL);
}

/* ARGSUSED */
static void PutNonSpatialMV_INT4(
  int    r,           /* row number */
  int    c,           /* column number */
        MAP_INT4 *m)    /* map this function belongs to */
{
  (void)m; // Shut up compiler
  (void)r; // Shut up compiler
  (void)c; // Shut up compiler
  Error("Can't Put a MV to a nonspatial INT4\n");
  POSTCOND(FALSE && r > c && m != NULL);
}
static BOOL GetNonSpatialINT4(
  INT4 *v,      /* write-only. Value */
  int    r,      /* row number */
  int    c,      /* column number */
const  MAP_INT4 *m)  /* map this function belongs to */
{

  PRECOND(m != NULL);

  *v = m->nonSpatialValue;
  return INSIDE_MAP(r,c,m);
}


/* Changes test performed in m->Get() function, affecting its return value
 */
static void SetGetTestINT4(
  GET_TEST  t,    /* new get test */
  const MAP_INT4 *mIn)   /* read-write. Structure SetGetTest works on. */
{
  MAP_INT4 *m = (MAP_INT4 *)mIn; /* cheat on const ptr */
  PRECOND(m != NULL);
  PRECOND(t == GET_NO_MV_TEST || t == GET_MV_TEST);

  m->Get = (t == GET_NO_MV_TEST)? m->getNOtest : m->getMVtest;
  m->getType = t;
}

static GET_TEST GetGetTestINT4(
  const MAP_INT4 *m)
{
  return m->getType;
}


static int  HintNrFastListINT4(
  const MAP_INT4 *m)
{
#ifdef DEBUG
  PRECOND(CR_INT4 != CR_REAL8); /* using on scalar is nonsens */
#endif
  return MIN( MAX_NR_FAST_LIST, m->maxVal);
}

static REAL8 SideMap(
  const MAP_INT4 *argNeverUsed)
{
  (void)argNeverUsed; // Shut up compiler
#ifdef DEBUG_DEVELOP
  /* supress not used warning */
  DEVELOP_PRECOND(argNeverUsed);
#endif
  return Side();
}

/* Initialize MAP_INT4 structure
 * Default test in get function is with MV test
 */
MAP_INT4 *InitMapINT4(
  size_t r,          /* number of rows */
  size_t c,          /* number of columns */
        void *v,        /* value buffer, ptr to 1 or all values */
        BOOL spatial,   /* does v contains a spatial or nonSpatial.*/
        CSF_CR inCr)    /* map contents cell representation */
{
  MAP_INT4 *m = ChkMalloc(sizeof(MAP_INT4));
          /* Structure to be initialized.*/
  if (m == NULL)
    return NULL;

  m->cr = (PCR_CR)CR_INT4;
  m->vs = 0;
  /* TEMPORAY CODE fix type */
  if (inCr == CR_REAL8)
    inCr = CR_REAL4;
  PRECOND(inCr == CR_UINT1 || inCr == CR_INT4 || inCr == CR_REAL4);
  /*  precondition we do not downcast, only promotion:
   */
  PRECOND(CELLSIZE(inCr) <= CELLSIZE(CR_INT4));
  /* above grabs all except: */
  PRECOND(! (inCr == CR_REAL4 && CR_INT4 == CR_INT4));

  /* not affected by inCr value: */
  m->nrRows = r;
  m->nrCols = c;
  m->spatial = spatial;
  m->NrRows = NrRowsINT4;
  m->NrCols = NrColsINT4;
  m->CellLength = SideMap;
  m->SetGetTest = SetGetTestINT4;
  m->GetGetTest = GetGetTestINT4;
  m->PutAllMV =  PutAllMVINT4;
  m->HintNrFastList = HintNrFastListINT4;
  /*
     m->GetValueScale = GetValueScaleINT4;
     m->GetInCellRepr = GetInCellReprINT4;
  */
  m->maxVal = MAX_NR_FAST_LIST; /* re-init if non-sp UINT1 or INT4 */
  m->getType = GET_MV_TEST;

  /* not used (I hope), breaks now if called
   * m->Copy = CopyINT4;
   */

  if (spatial) {
     m->st = PCR_ST_SPATIAL;
     m->spatialValue = MallocIndex2d(r,c,(size_t)CELLSIZE(inCr),v);
     if (m->spatialValue == NULL) {
       Free(m);
       return NULL;
     }
     switch (inCr) {
     case CR_UINT1:
       m->putMVPrivate = (PUT_MV_FUNC)PutMVPrivate_UINT1;
       m->putPrivate   = (PUT_VAL_FUNC)Put_INT4_in_UINT1;
       m->getPrivate   = (GET_FUNC)Get_in_UINT1_to_INT4;
       break;
     case CR_INT4:
       m->putMVPrivate = (PUT_MV_FUNC)PutMVPrivate_INT4;
       m->putPrivate   = (PUT_VAL_FUNC)Put_INT4_in_INT4;
       m->getPrivate   = (GET_FUNC)Get_in_INT4_to_INT4;
       break;
     case CR_REAL4:
       m->putMVPrivate = (PUT_MV_FUNC)PutMVPrivate_REAL4;
       m->putPrivate   = (PUT_VAL_FUNC)Put_INT4_in_REAL4;
       m->getPrivate   = (GET_FUNC)Get_in_REAL4_to_INT4;
       break;
      default:
        PRECOND(FALSE);
     }
     POSTCOND(m->getPrivate != NULL);
     m->getMVtest = GetSpatialMVtestINT4;
     m->getNOtest = GetSpatialINT4;
     m->PutMV = PutMV_INT4;
     m->Put   = PutSpatialINT4;
  } else {
    /* simply typecast to correct value
     *  works since we do not downcast, only promotion
     */
    m->st = PCR_ST_NONSPATIAL;
    switch (inCr) {
       case CR_UINT1:
         PRECOND(*((UINT1 *)v) != MV_UINT1);
         m->nonSpatialValue = (INT4)(*(UINT1*)v);
         m->maxVal = (int)m->nonSpatialValue;
         break;
       case CR_INT4:
         PRECOND(*((INT4 *)v) != MV_INT4);
         m->nonSpatialValue = (INT4)(*(INT4*)v);
         m->maxVal = (int)m->nonSpatialValue;
         break;
       case CR_REAL4:
         PRECOND(! IS_MV_REAL4(v));
         m->nonSpatialValue = (INT4)(*(REAL4*)v);
         break;
       default:
         PRECOND(FALSE);
       }
       m->getMVtest = GetNonSpatialINT4;
       m->getNOtest = GetNonSpatialINT4;
       m->Put       = PutNonSpatialINT4; /* error routine */
       m->PutMV     = PutNonSpatialMV_INT4; /* error routine */
  }
  /* Default test in get function is with MV test */
  m->Get        = m->getMVtest;
  m->inCellRepr = inCr;
  m->ownPtr     = m;
  return m;
}


void DeleteInternalMAP_INT4(
  MAP_INT4 *m)
{ /* delete map object and index but not
   * the data area
   */
  if (m->spatial)
      FreeIndex2d(m->spatialValue);
  Free(m);
}
