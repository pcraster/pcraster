#include "stddefx.h"

/********/
/* USES */
/********/

/* libs ext. <>, our ""  */

/* global header (opt.) and dassfunc's prototypes "" */
#include "calctypes.h"
#include "calc.h"
#include "dassfunc.h"


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

int Do_accu(void *s, void *f, const void **ins)
{
 return Accu((MAP_REAL8 *)s, (MAP_REAL8 *)f,
          (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1]);
}

int Do_accucapacity(void *s, void *f, const void **ins)
{
 return AccuLtc((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_accuthreshold(void *s, void *f, const void **ins)
{
 return AccuTt((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}
int Do_accufraction(void *s, void *f, const void **ins)
{
 return AccuFraction((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_accutrigger(void *s, void *f, const void **ins)
{
 return AccuTrigger((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_accutraveltime(void *s, void *f, const void **ins)
{
 return 1;
 /*
 return TravelTime((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
 */
}

int Do_diffuse(void *s, void *f, const void **ins)
{
 return Diffuse1((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_spread_impl(void *c, void *z, const void **ins)
{
 return Spread((MAP_REAL8 *)c, (MAP_INT4 *)z,
           (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_spreadmax_impl(void *c, void *z, const void **ins)
{
 return SpreadMax((MAP_REAL8 *)c, (MAP_INT4 *)z,
           (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3]);
}

int Do_spreadldd_impl(void *c, void *z, const void **ins)
{
 return SpreadLdd((MAP_REAL8 *)c, (MAP_INT4 *)z,
           (const MAP_UINT1 *)ins[0],
           (const MAP_INT4 *)ins[1],
           (const MAP_REAL8 *)ins[2], (const MAP_REAL8 *)ins[3]);
}

int Do_dynamicwave(void *q, void *h, const void **in)
{
 return DynamicWave(
     (MAP_REAL8 *)q,
     (MAP_REAL8 *)h,
     (const MAP_UINT1 *)in[ 0], /* ldd */
     (const MAP_REAL8 *)in[ 1], /* inQ */
     (const MAP_REAL8 *)in[ 2], /* inH */
     (const MAP_REAL8 *)in[ 3], /* bottomHeight */
     (const MAP_REAL8 *)in[ 4], /* roughness */
     (const MAP_REAL8 *)in[ 5], /* channelLength */
     (const MAP_REAL8 *)in[ 6], /* channelBottomWidth */
     (const MAP_REAL8 *)in[ 7], /* channelDepth */
     (const MAP_REAL8 *)in[ 8], /* channelForm */
     (const MAP_REAL8 *)in[ 9], /* floodplainWidth */
     (const MAP_REAL8 *)in[10], /* timeStepInSeconds */
     (const MAP_REAL8 *)in[11], /* nrTimeSlices */
     (const MAP_UINT1 *)in[12], /* structures */
     (const MAP_REAL8 *)in[13], /* structureA */
     (const MAP_REAL8 *)in[14], /* structureB */
     (const MAP_REAL8 *)in[15]);/* structureCrestLevel */
}

int Do_lddcreate_impl(void *l, void *d, const void **ins)
{
  MAP_UINT1* ldd=(MAP_UINT1 *)l;
  MAP_INT4 *t = CreateSpatialINT4(CR_INT4,ldd->nrRows,ldd->nrCols);
  int r;

  if ( t == NULL)
    return 1;
  r = Lddm(ldd, (const MAP_REAL8 *)ins[0]);
  if (r)
    goto end;
#ifdef NEVER
    printf("PIT REMOVER DISABLED\n");
#error ARE YOU SURE?
#else
  r = PitRem((MAP_UINT1 *)l, (MAP_REAL8 *)d,(MAP_INT4 *)t,
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3],
           (const MAP_REAL8 *)ins[4]);
#endif
end:
  DeleteMAP_INT4(t);
  return r;
}

int Do_route(void *l, void *d, const void **ins)
{
  PRECOND(l != NULL && d != NULL &&  ins != NULL);
  PRECOND(FALSE);
  return 1;
}
