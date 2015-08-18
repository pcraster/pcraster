#include "stddefx.h"


/********/
/* USES */
/********/

/* libs ext. <>(void * out, const void **ins) our "" */
#include "calctypes.h"
#include "calc.h" /* and api.h: CreateSpatialMap(), tmp layers */

/* global header (opt.) and globfunc's prototypes "" */
#include "globfunc.h"

/***************/
/* EXTERNALS */
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

int Do_areaaverage(void * out, const void **ins)
{
 return AreaAverage((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_areaarea(void * out, const void **ins)
{
 return AreaCount( (MAP_REAL8 *)out, (const MAP_INT4 *)ins[0]);
}
int Do_areadiversity(void * out, const void **ins)
{
 return AreaDiversity((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_areamajority(void * out, const void **ins)
{
 return AreaMajority((MAP_INT4 *)out, (const MAP_INT4 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_areamaximum(void * out, const void **ins)
{
 return AreaMax((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_areauniform(void * out, const void **ins)
{
 return AreaUniform((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0]);
}
int Do_areanormal(void * out, const void **ins)
{
 return AreaNormal((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0]);
}

int Do_areaminimum(void * out, const void **ins)
{
 return AreaMin((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_areatotal(void * out, const void **ins)
{
 return AreaTotal((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_catchment(void * out, const void **ins)
{
 return Catch((MAP_INT4 *)out, (const MAP_UINT1 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_subcatchment(void * out, const void **ins)
{
 return SubCatchment((MAP_INT4 *)out, (const MAP_UINT1 *)ins[0], (const MAP_INT4 *)ins[1]);
}
int Do_catchmenttotal(void * out, const void **ins)
{
 return PerformCatchStat((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_UINT1 *)ins[1]);
}
int Do_clump(void * out, const void **ins)
{
 return Clump((MAP_INT4 *)out, (const MAP_INT4 *)ins[0]);
}
int Do_downstream(void * out, const void **ins)
{
 return DownStream((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_drain(void * out, const void **ins)
{
 return Drain((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_ldddist(void * out, const void **ins)
{
 return Ldddist((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0], (const MAP_UINT1 *)ins[1],
  (const MAP_REAL8 *)ins[2], TRUE);
}
int Do_downstreamtotal(void * out, const void **ins)
{
 return Downstreamtotal((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0],
     (const MAP_REAL8 *)ins[1]);
}

int Do_upstream(void *out, const void **ins)
{
 return Upstream((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1]);
}

int Do_streamorder(void *out, const void **ins)
{
 return StreamOrder((MAP_INT4 *)out, (const MAP_UINT1 *)ins[0]);
}
int Do_transient(void *out, const void **ins)
{
  return Transient(out,ins,7);
#ifdef EFFE_NIET
 return Transient((MAP_INT4 *)out, 
     (const MAP_REAL8 *)ins[0], /* dem */
     (const MAP_REAL8 *)ins[1], /* recharge */
     (const MAP_REAL8 *)ins[2], /* transmissivity */
     (const MAP_INT4  *)ins[3], /* flow condition */
     (const MAP_REAL8 *)ins[4], /* storage coeff. */
     (const MAP_REAL8 *)ins[5], /* interval */
     (const MAP_REAL8 *)ins[6]); /* tolerance */
#endif
}

int Do_aspect(void * out, const void **ins)
{
 return Orient((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0]);
}
int Do_path(void * out, const void **ins)
{
 return Path((MAP_UINT1 *)out, (const MAP_UINT1 *)ins[0], (const MAP_UINT1 *)ins[1]);
}
int Do_slope(void * out, const void **ins)
{
 return Slope((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0]);
}
int Do_window4total(void * out, const void **ins)
{
 return Window4total((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0]);
}
int Do_plancurv(void * out, const void **ins)
{
 return PlanformCurvature((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0]);
}

int Do_profcurv(void * out, const void **ins)
{
 return ProfileCurvature((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0]);
}

int Do_slopelength(void * out, const void **ins)
{
 return Slopelength((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_view(void * out, const void **ins)
{
 return View((MAP_UINT1 *)out, (const MAP_REAL8 *)ins[0], (const MAP_UINT1 *)ins[1]);
}
int Do_extentofview(void * out, const void **ins)
{
 return ExtentOfView((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_inversedistance(void * out, const void **ins)
{
 return Idi((MAP_REAL8 *)out,
             (const MAP_UINT1 *)ins[0],
             (const MAP_REAL8 *)ins[1],
             (const MAP_REAL8 *)ins[2],
             (const MAP_REAL8 *)ins[3],
             (const MAP_REAL8 *)ins[4]);
}

int Do_windowaverage(void * out, const void **ins)
{
 return WindowAverage((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_markwhilesumle(void * out, const void **ins)
{
 return MarkWhileSumLe((MAP_UINT1 *)out,
  (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
  (const MAP_REAL8 *)ins[2]);
}
int Do_markuntilsumge(void * out, const void **ins)
{
 return MarkUntilSumGe((MAP_UINT1 *)out,
  (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
  (const MAP_REAL8 *)ins[2]);
}
int Do_ellipseaverage(void * out, const void **ins)
{
 return EllipseAverage((MAP_REAL8 *)out,
  (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
  (const MAP_REAL8 *)ins[2], (const MAP_REAL8 *)ins[3]);
}
int Do_windowdiversity(void * out, const void **ins)
{
 return WindowDiversity((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_windowhighpass(void * out, const void **ins)
{
 return WindowHighpass((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_windowmajority(void * out, const void **ins)
{
 return WindowMajority((MAP_INT4 *)out, (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_windowmaximum(void * out, const void **ins)
{
 return WindowMax((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_windowminimum(void * out, const void **ins)
{
 return WindowMin((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
int Do_windowtotal(void * out, const void **ins)
{
 return WindowTotal((MAP_REAL8 *)out, (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}

int Do_kinematic(void * out, const void **ins)
{
 return Kinematic((MAP_REAL8 *)out, (const MAP_UINT1 *)ins[0],
   (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2],
   (const MAP_REAL8 *)ins[3], (const MAP_REAL8 *)ins[4],
   (const MAP_REAL8 *)ins[5], (const MAP_REAL8 *)ins[6]);
}

int Do_order(void * out, const void **ins)
{
  MAP_REAL8 *o= (MAP_REAL8 *)out;
  MAP_INT4 *t = CreateSpatialINT4(CR_INT4,o->nrRows,o->nrCols);
  int r;

  if (t == NULL)
    return 1;
  r = Order(o, (const MAP_REAL8 *)ins[0], t);
  DeleteMAP_INT4(t);
  return r;
}

int Do_lddmask(void * out, const void **ins)
{
  return MaskLdd((MAP_UINT1 *)out,
           (const MAP_UINT1 *)ins[0],
           (const MAP_UINT1 *)ins[1]);
}
int Do_move(void * out, const void **ins)
{
 return Move((MAP_UINT1 *)out,
           (const MAP_UINT1 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
}
int Do_shift(void * out, const void **ins)
{
 return Shift((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8  *)ins[1],
           (const MAP_REAL8  *)ins[2]);
}
int Do_shift0(void * out, const void **ins)
{
 return Shift0((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8  *)ins[1],
           (const MAP_REAL8  *)ins[2]);
}

int Do_lddrepair(void *out, const void **ins)
{
 return RepairLdd((MAP_UINT1 *)out, (const MAP_UINT1 *)ins[0]);
}


int Do_brenner(void *out, const void **ins)
{
 return BirdsSpread((MAP_UINT1 *)out,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3],
           (const MAP_REAL8 *)ins[4],
           (const MAP_REAL8 *)ins[5]);
}

int Do_influencesimplegauss(void *out, const void **ins)
{
 return InfluenceSimpleGauss((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
}
int Do_distributesimplegauss(void *out, const void **ins)
{
 MAP_REAL8 *o=(MAP_REAL8 *)out;
 MAP_REAL8 *t = CreateSpatialREAL8(CR_REAL8,o->nrRows,o->nrCols);
 return DistributeSimpleGauss(
           o, t,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
  DeleteMAP_REAL8(t);
}
int Do_ibngauss(void *out, const void **ins)
{
 return IBNGauss((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
}

int Do_horizontan(void *out, const void **ins)
{
 return HorizonTangent((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}
