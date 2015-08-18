#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>, our ""  */
#include <string.h> /* memset */
#include "mathx.h"
#include "csftypes.h"

/* global header (opt.) and sbinfunc's prototypes "" */
#include "sbinfunc.h"


/* headers of this app. modules called */

/***************/
/* EXTERNALS   */
/***************/

/**********************/
/* LOCAL DECLARATIONS */
/**********************/
#define TWO_UINT1 ((UINT1)2)

/*********************/
/* LOCAL DEFINITIONS */
/*********************/

/******************/
/* IMPLEMENTATION */
/******************/


int Do_fdiv_ss(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
    if (IS_MV_REAL4(vR+i) || vR[i] == 0)
    SET_MV_REAL4(vL+i);
    else
    vL[i] /= vR[i];
     }
     return 0;
}

int Do_fdiv_ns(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vR+i))
  {
    if (vR[i] == 0)
    SET_MV_REAL4(vR+i);
    else
    vR[i] = valL / vR[i];
     }
     return 0;
}

int Do_fdiv_sn(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valR = *vR;
 PRECOND(! IS_MV_REAL4(vR));
 if (valR == 0.0) { // pcrcalc/test64
  for(i=0;i < n; i++)
    SET_MV_REAL4(vL+i);
  return 0;
 }
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
    vL[i] /= valR;
     return 0;
}

int Do_idiv_ss(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
    if (IS_MV_REAL4(vR+i) || vR[i] == 0)
    SET_MV_REAL4(vL+i);
    else
    vL[i] = (REAL4)Fdiv(vL[i], vR[i]);
     }
     return 0;
}

int Do_idiv_ns(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vR+i))
  {
    if (vR[i] == 0)
    SET_MV_REAL4(vR+i);
    else
    vR[i] = (REAL4)Fdiv( valL , vR[i]);
     }
     return 0;
}

int Do_idiv_sn(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valR = *vR;
 PRECOND(! IS_MV_REAL4(vR));
 if (valR == 0.0)
  return 1;
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
    vL[i] = (REAL4)Fdiv( vL[i] , valR);
     return 0;
}


int Do_mod_ss(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
    if (IS_MV_REAL4(vR+i) || vR[i] == 0)
    SET_MV_REAL4(vL+i);
    else
    vL[i] = (REAL4)fmod(vL[i], vR[i]);
     }
     return 0;
}

int Do_mod_ns(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vR+i))
  {
    if (vR[i] == 0)
    SET_MV_REAL4(vR+i);
    else
    vR[i] = (REAL4)fmod( valL , vR[i]);
     }
     return 0;
}

int Do_mod_sn(REAL4 *vL, REAL4 *vR, size_t n)
{
 size_t i;
 REAL4  valR = *vR;
 PRECOND(! IS_MV_REAL4(vR));
 if (valR == 0.0)
  return 1;
 for(i=0;i < n; i++)
  if (! IS_MV_REAL4(vL+i))
    vL[i] = (REAL4)fmod( vL[i] , valR);
 return 0;
}

/*
 Power x**y
 The pow function return the value of <x> raised to the
 power of <y>. The result varies, depending on <x> and <y>:
                               case
 (x == 0.0 && y <= 0.0)        1, 2a
 (x < 0.0  && floor(y) != y)   2b
case:
   1. If <x> is 0.0 and <y> is negative, the functions set errno to
      EDOM and return 0.0.

   2. (a) If both <x> and <y> are 0.0, or if (b) <x> is negative and <y> is
      not an integer, the functions print a DOMAIN error message to
      stderr, set errno to EDOM, and return 0.0.
      (b) If an overflow results, the functions set errno to ERANGE
      and return .HUGE_VAL (for pow) or ._LHUGE_VAL (for powl). If
      an underflow results, errno is not set and 0.0 is returned.
*/

int Do_pow_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for(i=0; i < n; i++)
 if (! IS_MV_REAL4(vL+i))
 {
  if ( IS_MV_REAL4(vR+i) ||
   (vL[i] == 0.0F && vR[i] <= 0.0F) ||
   (vL[i] < 0.0F  && floor(vR[i]) != vR[i])
 )
   SET_MV_REAL4(vL+i);
  else
   vL[i] = (REAL4)pow(vL[i],vR[i]);
 }
 return 0;
}

int Do_pow_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));

 for(i=0; i < n; i++)
 if (! IS_MV_REAL4(vR+i))
 {
  if ( (valL == 0.0F && vR[i] <= 0.0F) ||
   (valL < 0.0F  && floor(vR[i]) != vR[i])
 )
   SET_MV_REAL4(vR+i);
  else
   vR[i] = (REAL4)pow(valL,vR[i]);
 }
 return 0;
}

static void powAsMultiply(
 REAL4 *vL,
 size_t nrTimes,
 size_t n)
{
 size_t i;
 for(i=0; i < n; i++)
 if (! IS_MV_REAL4(vL+i))
 {
  size_t p;
  double val = vL[i];
  vL[i]=1;
  for(p=0; p <nrTimes; p++)
   vL[i] *= val;
 }
}

int Do_pow_sn(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valR = *vR;
 PRECOND(! IS_MV_REAL4(vR));

 if (valR >= 0 && floor(valR) == valR && valR < 10 ) {
  powAsMultiply(vL,(size_t)valR,n);
  return 0;
 }

 for(i=0; i < n; i++)
 if (! IS_MV_REAL4(vL+i))
 {
  if ( (vL[i] == 0.0F && valR <= 0.0F) ||
   (vL[i] < 0.0F  && floor(valR) != valR)
  )
   SET_MV_REAL4(vL+i);
  else
   vL[i] = (REAL4)pow(vL[i],valR);
 }
 return 0;
}


int Do_mul_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
   if (IS_MV_REAL4(vR+i))
    SET_MV_REAL4(vL+i);
   else
    vL[i] *= vR[i];
  }
 return 0;
}

int Do_mul_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vR+i))
   vR[i] *= valL;
 return 0;
}

int Do_badd_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
   if (IS_MV_REAL4(vR+i))
    SET_MV_REAL4(vL+i);
   else
    vL[i] += vR[i];
  }
 return 0;
}

int Do_badd_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(!IS_MV_REAL4(vL));
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vR+i))
   vR[i] += valL;
 return 0;
}

int Do_bmin_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vL+i))
  {
   if (IS_MV_REAL4(vR+i))
    SET_MV_REAL4(vL+i);
   else
    vL[i] -= vR[i];
  }
 return 0;
}

int Do_bmin_sn(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valR = *vR;
 PRECOND(! IS_MV_REAL4(vR));
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vL+i))
   vL[i] -= valR;
 return 0;
}

int Do_bmin_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for (i = 0; i < n; i++)
  if (! IS_MV_REAL4(vR+i))
   vR[i] = valL - vR[i];
 return 0;
}

/* boolean operands, in strict env.
 * all boolean are 1 or 0 (AND NOT 0 or NONZERO)
 * we do bitwise operands
 */

int Do_and_ss(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (vL[i] != MV_UINT1)
  {
   if (vR[i] == MV_UINT1)
    vL[i] = MV_UINT1;
   else
   {
    PRECOND(vL[i] < TWO_UINT1 && vR[i] < TWO_UINT1);
    vL[i] &= vR[i];
   }
  }
 return 0;
}

int Do_and_ns(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 UINT1 valL = *vL;
 PRECOND(valL != MV_UINT1 && valL < TWO_UINT1);
 for (i = 0; i < n; i++)
  if (vR[i] != MV_UINT1)
  {
   PRECOND(vR[i] < TWO_UINT1);
   vR[i] &= valL;
  }
 return 0;
}


int Do_or_ss(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (vL[i] != MV_UINT1)
  {
   if (vR[i] == MV_UINT1)
    vL[i] = MV_UINT1;
   else
   {
    PRECOND(vL[i] < TWO_UINT1 && vR[i] < TWO_UINT1);
    vL[i] |= vR[i];
   }
  }
 return 0;
}

int Do_or_ns(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 UINT1 valL = *vL;
 PRECOND(valL != MV_UINT1 && valL < TWO_UINT1);
 for (i = 0; i < n; i++)
  if (vR[i] != MV_UINT1)
  {
   PRECOND(vR[i] < TWO_UINT1);
   vR[i] |= valL;
  }
 return 0;
}

int Do_xor_ss(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (vL[i] != MV_UINT1)
  {
   if (vR[i] == MV_UINT1)
    vL[i] = MV_UINT1;
   else
   {
    PRECOND(vL[i] < TWO_UINT1 && vR[i] < TWO_UINT1);
    vL[i] ^= vR[i];
   }
  }
 return 0;
}

int Do_xor_ns(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 size_t i;
 UINT1 valL = *vL;
 PRECOND(valL != MV_UINT1 && valL < TWO_UINT1);
 for (i = 0; i < n; i++)
  if (vR[i] != MV_UINT1)
  {
   PRECOND(vR[i] < TWO_UINT1);
   vR[i] ^= valL;
  }
 return 0;
}

int Do_max_4_ns(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
 size_t i;
 INT4 valL = *vL;
 PRECOND(valL != MV_INT4);
 for (i = 0; i < n; i++)
  if (vR[i] != MV_INT4)
  {
   if (valL > vR[i])
    vR[i] = valL;
  }
 return 0;
}

int Do_max_4_ss(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (vL[i] != MV_INT4)
  {
   INT4 valR = vR[i];
   if (valR == MV_INT4 || valR > vL[i])
   vL[i] = valR;
  }
 return 0;
}

int Do_min_4_ns(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
 size_t i;
 INT4 valL = *vL;
 PRECOND(valL != MV_INT4);
 for (i = 0; i < n; i++)
  if (vR[i] != MV_INT4)
  {
   if (valL < vR[i])
    vR[i] = valL;
  }
 return 0;
}

int Do_min_4_ss(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (vL[i] != MV_INT4)
  {
   INT4 valR = vR[i];
   if (valR == MV_INT4 || valR < vL[i])
   vL[i] = valR;
  }
 return 0;
}

int Do_min_s_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for (i = 0; i < n; i++)
  if (!IS_MV_REAL4(vR+i)) {
   if (valL < vR[i])
    vR[i] = valL;
  }
 return 0;
}

int Do_min_s_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (!IS_MV_REAL4(vL+i))
  {
    if (IS_MV_REAL4(vR+i))
  SET_MV_REAL4(vL+i);
    else if (vR[i] < vL[i])
  vL[i] = vR[i];
}
 return 0;
}


int Do_max_s_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 REAL4 valL = *vL;
 PRECOND(! IS_MV_REAL4(vL));
 for (i = 0; i < n; i++)
  if (!IS_MV_REAL4(vR+i))
  {
   if (valL > vR[i])
    vR[i] = valL;
  }
 return 0;
}

int Do_max_s_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 size_t i;
 for (i = 0; i < n; i++)
  if (!IS_MV_REAL4(vL+i))
  {
   if (IS_MV_REAL4(vR+i))
    SET_MV_REAL4(vL+i);
   else if (vR[i] > vL[i])
    vL[i] = vR[i];
  }
 return 0;
}

/* cover_ns
 * is a simple memset
 */
int Do_cover_1_ns(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
 PRECOND(*vL != MV_UINT1);
 memset(vR,*vL,n);
 return 0;
}

static void DwordSet(
 INT4 *dest,
 INT4 value,
 size_t n)
{
 size_t i;
 for(i=0; i < n; i++)
  dest[i] = value;
}

int Do_cover_4_ns(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
 PRECOND(*vL != MV_INT4);
 DwordSet(vR,*vL,n);
 return 0;
}

int Do_cover_s_ns(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
 PRECOND(! IS_MV_REAL4(vL));
 DwordSet((INT4 *)vR,*((INT4 *)vL),n);
 return 0;
}


int Do_cover_1_ss(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
    size_t i;
    for(i=0; i < n; i++)
     if (vL[i] == MV_UINT1)
       vL[i] = vR[i];
    return 0;
}
int Do_cover_4_ss(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
    size_t i;
    for(i=0; i < n; i++)
     if (vL[i] == MV_INT4)
       vL[i] = vR[i];
    return 0;
}

int Do_cover_4_sn(
 INT4 *vL,
 INT4 *vR,
 size_t n)
{
    size_t i;
    INT4 valR = *vR;
    PRECOND(valR != MV_INT4);
    for(i=0; i < n; i++)
     if (vL[i] == MV_INT4)
       vL[i] = valR;
    return 0;
}

int Do_cover_1_sn(
 UINT1 *vL,
 UINT1 *vR,
 size_t n)
{
    size_t i;
    UINT1 valR = *vR;
    PRECOND(valR != MV_UINT1);
    for(i=0; i < n; i++)
     if (vL[i] == MV_UINT1)
       vL[i] = valR;
    return 0;
}

/* scalar cover
 * MV bit-patterm is equal for REAL4 and UINT4
 */
int Do_cover_s_ss(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
    size_t i;
    for(i=0; i < n; i++)
     if (IS_MV_REAL4(vL+i))
       vL[i] = vR[i];
    return 0;
}

int Do_cover_s_sn(
 REAL4 *vL,
 REAL4 *vR,
 size_t n)
{
    size_t i;
    REAL4 valR;
    PRECOND(!IS_MV_REAL4(vR));
    valR = *vR;
    for(i=0; i < n; i++)
     if (IS_MV_REAL4(vL+i))
       vL[i] = valR;
    return 0;
}

/*
 see tcl source for most portable solution
int matherr(struct exception *xPtr)
{
  return 1;
}

typedef int (*MathErr)(struct exception *xPtr);

MathErr dummyPtr = matherr;
*/
