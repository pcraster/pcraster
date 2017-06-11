#include "stddefx.h"

#ifndef INCLUDED_CALC_GLOBFUNC
#include "calc_globfunc.h"
#define INCLUDED_CALC_GLOBFUNC
#endif

#include "calc_types.h"
#include "calc.h"

#include <new>
#include <stdexcept>

namespace calc {

static void throwAtNoMem(void *ptr) {
 if (!ptr)
  throw std::bad_alloc();
}

struct MapReal8 {
  MAP_REAL8 *ptr;
  MapReal8(int nrRows, int nrCols) {
   ptr = CreateSpatialREAL8(CR_REAL8, nrRows, nrCols);
   throwAtNoMem(ptr);
  }
  MapReal8(int nrRows, int nrCols, const float& v) {
   ptr = InitMapREAL8(nrRows, nrCols, (void *)&v, false, CR_REAL4);
   throwAtNoMem(ptr);
  }
  ~MapReal8() {
   DeleteMAP_REAL8(ptr);
  }

  MAP_REAL8* map() const {
   return ptr;
  }
};

struct MapInt4 {
  MAP_INT4 *ptr;
  MapInt4(int nrRows, int nrCols) {
   ptr = CreateSpatialINT4(CR_INT4, nrRows, nrCols);
   throwAtNoMem(ptr);
  }
  ~MapInt4() {
   DeleteMAP_INT4(ptr);
  }

  MAP_INT4* map() const {
   return ptr;
  }
};

struct MapUint1 {
  MAP_UINT1 *ptr;
  MapUint1(int nrRows, int nrCols) {
   ptr = CreateSpatialUINT1(CR_UINT1, nrRows, nrCols);
   throwAtNoMem(ptr);
  }
  ~MapUint1() {
   DeleteMAP_UINT1(ptr);
  }

  MAP_UINT1* map() const {
   return ptr;
  }
};

struct AccumTT : public MapReal8 {
  int lddDistResult;
  AccumTT(const MAP_UINT1*  ldd,
          const MAP_REAL8*  velocity):
     MapReal8(ldd->nrRows, ldd->nrCols)
 {
   // do ldddist(ldd, ldd==5, 1/Velocity)
   ldd->SetGetTest(GET_MV_TEST, ldd);
   velocity->SetGetTest(GET_MV_TEST, velocity);

   MapUint1 pits(ldd->nrRows, ldd->nrCols);
   MapReal8 friction(ldd->nrRows,ldd->nrCols);
   for(int r = 0; r < ldd->nrRows; ++r)
    for(int c = 0; c < ldd->nrCols; ++c)
    {
      UINT1 p;
      if (ldd->Get(&p, r, c, ldd))
         pits.map()->Put(p==5, r, c, pits.map());
      else
         pits.map()->PutMV(r,c, pits.map());

      REAL8 v; // velocity -> friction
      if (velocity->Get(&v, r, c, velocity)) {
         if (v <= 0)
           v = 0; // WRONG
         else
           v = 1/v;
         friction.map()->Put(v, r, c, friction.map());
      } else
         friction.map()->PutMV(r,c, friction.map());
    }
    lddDistResult = Ldddist((MAP_REAL8 *)map(),ldd, pits.map(), friction.map(), false);
 }
};

int Do_accutraveltime(void *s, void *f, const void **ins)
{
 const MAP_UINT1 *ldd = (const MAP_UINT1*)ins[0];

 MapReal8 removed(ldd->nrRows,ldd->nrCols);
 MapReal8 constantTransportFraction(ldd->nrRows, ldd->nrCols, 1.0F);
 AccumTT  accumTT(ldd,(const MAP_REAL8 *)ins[2]);

 return TravelTime((MAP_REAL8 *)s, (MAP_REAL8 *)f,removed.map(),
           (const MAP_UINT1 *)ldd,
           (const MAP_REAL8 *)ins[1], accumTT.map(),
           constantTransportFraction.map());
}

int Do_accutraveltimefraction(void *s, void *f, const void **ins)
{
 const MAP_UINT1 *ldd = (const MAP_UINT1*)ins[0];
 AccumTT  accumTT(ldd,(const MAP_REAL8 *)ins[2]);

 MapReal8 removed(ldd->nrRows,ldd->nrCols);
 return TravelTime((MAP_REAL8 *)s, (MAP_REAL8 *)f, removed.map(),
           ldd,
           (const MAP_REAL8 *)ins[1], accumTT.map(),
           (const MAP_REAL8 *)ins[3]);
}

int Do_accutraveltimefractionremoved(void *removed, const void **ins)
{
 const MAP_UINT1 *ldd = (const MAP_UINT1*)ins[0];
 AccumTT  accumTT(ldd,(const MAP_REAL8 *)ins[2]);
 MapReal8 s(ldd->nrRows,ldd->nrCols);
 MapReal8 f(ldd->nrRows,ldd->nrCols);
 return TravelTime(s.map(),f.map(), (MAP_REAL8 *)removed,
           ldd,
           (const MAP_REAL8 *)ins[1], accumTT.map(),
           (const MAP_REAL8 *)ins[3]);
}

int Do_diffuse(void *s, void *f, const void **ins)
{
 return Diffuse1((MAP_REAL8 *)s, (MAP_REAL8 *)f,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_spread(void *c, void *z, const void **ins)
{
 return Spread((MAP_REAL8 *)c, (MAP_INT4 *)z,
           (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1], (const MAP_REAL8 *)ins[2]);
}

int Do_spreadmax(void *c, void *z, const void **ins)
{
 return SpreadMax((MAP_REAL8 *)c, (MAP_INT4 *)z,
           (const MAP_INT4 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3]);
}

int Do_spreadldd(void *c, void *z, const void **ins)
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

int Do_lddcreate(void *l, void *d, const void **ins)
{
  MAP_UINT1* ldd=(MAP_UINT1 *)l;
  int r = Lddm(ldd, (const MAP_REAL8 *)ins[0]);
  if (r)
    return r;

  MapInt4 t(ldd->nrRows,ldd->nrCols);
  return PitRem((MAP_UINT1 *)l, (MAP_REAL8 *)d,(MAP_INT4 *)t.map(),
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3],
           (const MAP_REAL8 *)ins[4]);
}

int Do_lddcreatend(void *l, void *d, const void **ins)
{

  MAP_UINT1* ldd=(MAP_UINT1 *)l;
  int r = LddmND(ldd, (const MAP_REAL8 *)ins[0]);
  if (r)
    return r;

  MapInt4 t(ldd->nrRows,ldd->nrCols);
  return PitRemND((MAP_UINT1 *)l, (MAP_REAL8 *)d,t.map(),
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2],
           (const MAP_REAL8 *)ins[3],
           (const MAP_REAL8 *)ins[4]);
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
int Do_areauniform(void * out, const void **ins)
{
 return AreaUniform((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0]);
}
int Do_areanormal(void * out, const void **ins)
{
 return AreaNormal((MAP_REAL8 *)out, (const MAP_INT4 *)ins[0]);
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
 return Ldddist((MAP_REAL8 *)out,
     (const MAP_UINT1 *)ins[0], (const MAP_UINT1 *)ins[1], (const MAP_REAL8 *)ins[2], true);
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
  return Transient(&out,ins,7);
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
int Do_markwhilesumge(void * out, const void **ins)
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

int Do_order(void * out, const void **ins)
{
  MAP_REAL8 *o= (MAP_REAL8 *)out;
  MapInt4 t(o->nrRows,o->nrCols);
  return Order(o, (const MAP_REAL8 *)ins[0], t.map());
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
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
}
int Do_shift0(void * out, const void **ins)
{
 return Shift0((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0],
           (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
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
 MapReal8 t(o->nrRows,o->nrCols);
 return DistributeSimpleGauss(
           o, t.map(),
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
           (const MAP_REAL8 *)ins[2]);
}
int Do_ibngauss(void *out, const void **ins)
{
 return IBNGauss((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1],
                                      (const MAP_REAL8 *)ins[2]);
}

int Do_horizontan(void *out, const void **ins)
{
 return HorizonTangent((MAP_REAL8 *)out,
           (const MAP_REAL8 *)ins[0], (const MAP_REAL8 *)ins[1]);
}

int Do_riksfraction(void* out, void const** ins)
{
  return riksFraction(
         (MAP_REAL8 *)out,
         (MAP_UINT1 const*)ins[0],
         (MAP_REAL8 const*)ins[1],
         (MAP_REAL8 const*)ins[2]);
}

int Do_squarefraction(void* out, void const** ins)
{
  return squareFraction(
         (MAP_REAL8 *)out,
         (MAP_UINT1 const*)ins[0],
         (MAP_REAL8 const*)ins[1],
         (MAP_REAL8 const*)ins[2]);
}

int Do_gradx(void* out, void const** ins)
{
  return vf_gradx((MAP_REAL8 *)out, (MAP_REAL8 const*)ins[0]);
}
int Do_grady(void* out, void const** ins)
{
  return vf_grady((MAP_REAL8 *)out, (MAP_REAL8 const*)ins[0]);
}

int Do_divergence(void* out, void const** ins)
{
  return vf_divergence((MAP_REAL8 *)out,
  (MAP_REAL8 const*)ins[0],
  (MAP_REAL8 const*)ins[1]);
}
int Do_diver(void* out, void const** ins)
{
  return vf_diver((MAP_REAL8 *)out,
  (MAP_REAL8 const*)ins[0],
  (MAP_REAL8 const*)ins[1],
  (MAP_REAL8 const*)ins[2],
  (MAP_REAL8 const*)ins[3]);
}

int Do_lax(void* out, void const** ins)
{
  return vf_lax((MAP_REAL8 *)out,
  (MAP_REAL8 const*)ins[0],
  (MAP_REAL8 const*)ins[1]);
}

int Do_laplacian(void* out, void const** ins)
{
  return vf_laplacian((MAP_REAL8 *)out, (MAP_REAL8 const*)ins[0]);
}

} // namespace
