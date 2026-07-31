#include "stddefx.h"
#include "calc_globfunc.h"
#include "calc_types.h"
#include "calc.h"

#include <cmath>
#include <new>
#include <stdexcept>

namespace calc
{

static void throwAtNoMem(void *ptr)
{
  if (ptr == nullptr) {
    throw std::bad_alloc();
  }
}

struct MapReal8 {
  MAP_REAL8 *ptr;

  MapReal8(int nrRows, int nrCols)
  {
    ptr = CreateSpatialREAL8(CR_REAL8, nrRows, nrCols);
    throwAtNoMem(ptr);
  }

  MapReal8(int nrRows, int nrCols, const float &v)
  {
    ptr = InitMapREAL8(nrRows, nrCols, (void *)&v, false, CR_REAL4);
    throwAtNoMem(ptr);
  }

  ~MapReal8()
  {
    DeleteMAP_REAL8(ptr);
  }

  MAP_REAL8 *map() const
  {
    return ptr;
  }
};

struct MapInt4 {
  MAP_INT4 *ptr;

  MapInt4(int nrRows, int nrCols)
  {
    ptr = CreateSpatialINT4(CR_INT4, nrRows, nrCols);
    throwAtNoMem(ptr);
  }

  ~MapInt4()
  {
    DeleteMAP_INT4(ptr);
  }

  MAP_INT4 *map() const
  {
    return ptr;
  }
};

struct MapUint1 {
  MAP_UINT1 *ptr;

  MapUint1(int nrRows, int nrCols)
  {
    ptr = CreateSpatialUINT1(CR_UINT1, nrRows, nrCols);
    throwAtNoMem(ptr);
  }

  ~MapUint1()
  {
    DeleteMAP_UINT1(ptr);
  }

  MAP_UINT1 *map() const
  {
    return ptr;
  }
};

struct AccumTT : public MapReal8 {
  int lddDistResult;

  AccumTT(const MAP_UINT1 *ldd, const MAP_REAL8 *velocity) : MapReal8(ldd->nrRows, ldd->nrCols)
  {
    // do ldddist(ldd, ldd==5, 1/Velocity)
    ldd->SetGetTest(GET_MV_TEST, ldd);
    velocity->SetGetTest(GET_MV_TEST, velocity);

    MapUint1 const pits(ldd->nrRows, ldd->nrCols);
    MapReal8 const friction(ldd->nrRows, ldd->nrCols);
    for (int r = 0; r < ldd->nrRows; ++r) {
      for (int c = 0; c < ldd->nrCols; ++c) {
        UINT1 p = 0;
        if (ldd->Get(&p, r, c, ldd)) {
          pits.map()->Put(static_cast<UINT1_T>(p == 5), r, c, pits.map());
        } else {
          pits.map()->PutMV(r, c, pits.map());
        }

        REAL8 v = NAN;  // velocity -> friction
        if (velocity->Get(&v, r, c, velocity)) {
          if (v <= 0) {
            v = 0;  // WRONG
          } else {
            v = 1 / v;
          }
          friction.map()->Put(v, r, c, friction.map());
        } else {
          friction.map()->PutMV(r, c, friction.map());
        }
      }
    }
    lddDistResult = Ldddist(map(), ldd, pits.map(), friction.map(), false);
  }
};

int Do_accutraveltime(void *s, void *f, const void **ins)
{
  const auto *ldd = static_cast<const MAP_UINT1 *>(ins[0]);

  MapReal8 const removed(ldd->nrRows, ldd->nrCols);
  MapReal8 const constantTransportFraction(ldd->nrRows, ldd->nrCols, 1.0F);
  AccumTT const accumTT(ldd, static_cast<const MAP_REAL8 *>(ins[2]));

  return TravelTime(static_cast<MAP_REAL8 *>(s), static_cast<MAP_REAL8 *>(f), removed.map(), static_cast<const MAP_UINT1 *>(ldd),
                    static_cast<const MAP_REAL8 *>(ins[1]), accumTT.map(), constantTransportFraction.map());
}

int Do_accutraveltimefraction(void *s, void *f, const void **ins)
{
  const auto *ldd = static_cast<const MAP_UINT1 *>(ins[0]);
  AccumTT const accumTT(ldd, static_cast<const MAP_REAL8 *>(ins[2]));

  MapReal8 const removed(ldd->nrRows, ldd->nrCols);
  return TravelTime(static_cast<MAP_REAL8 *>(s), static_cast<MAP_REAL8 *>(f), removed.map(), ldd, static_cast<const MAP_REAL8 *>(ins[1]),
                    accumTT.map(), static_cast<const MAP_REAL8 *>(ins[3]));
}

int Do_accutraveltimefractionremoved(void *removed, const void **ins)
{
  const auto *ldd = static_cast<const MAP_UINT1 *>(ins[0]);
  AccumTT const accumTT(ldd, static_cast<const MAP_REAL8 *>(ins[2]));
  MapReal8 const s(ldd->nrRows, ldd->nrCols);
  MapReal8 const f(ldd->nrRows, ldd->nrCols);
  return TravelTime(s.map(), f.map(), static_cast<MAP_REAL8 *>(removed), ldd, static_cast<const MAP_REAL8 *>(ins[1]),
                    accumTT.map(), static_cast<const MAP_REAL8 *>(ins[3]));
}

int Do_diffuse(void *s, void *f, const void **ins)
{
  return Diffuse1(static_cast<MAP_REAL8 *>(s), static_cast<MAP_REAL8 *>(f), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                  static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_spread(void *c, void *z, const void **ins)
{
  return Spread(static_cast<MAP_REAL8 *>(c), static_cast<MAP_INT4 *>(z), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_spreadmax(void *c, void *z, const void **ins)
{
  return SpreadMax(static_cast<MAP_REAL8 *>(c), static_cast<MAP_INT4 *>(z), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                   static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]));
}

int Do_spreadldd(void *c, void *z, const void **ins)
{
  return SpreadLdd(static_cast<MAP_REAL8 *>(c), static_cast<MAP_INT4 *>(z), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_INT4 *>(ins[1]),
                   static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]));
}

int Do_dynamicwave(void *q, void *h, const void **in)
{
  return DynamicWave(static_cast<MAP_REAL8 *>(q), static_cast<MAP_REAL8 *>(h), static_cast<const MAP_UINT1 *>(in[0]), /* ldd */
                     static_cast<const MAP_REAL8 *>(in[1]),                                 /* inQ */
                     static_cast<const MAP_REAL8 *>(in[2]),                                 /* inH */
                     static_cast<const MAP_REAL8 *>(in[3]),                                 /* bottomHeight */
                     static_cast<const MAP_REAL8 *>(in[4]),                                 /* roughness */
                     static_cast<const MAP_REAL8 *>(in[5]),                                 /* channelLength */
                     static_cast<const MAP_REAL8 *>(in[6]),                                 /* channelBottomWidth */
                     static_cast<const MAP_REAL8 *>(in[7]),                                 /* channelDepth */
                     static_cast<const MAP_REAL8 *>(in[8]),                                 /* channelForm */
                     static_cast<const MAP_REAL8 *>(in[9]),                                 /* floodplainWidth */
                     static_cast<const MAP_REAL8 *>(in[10]),                                /* timeStepInSeconds */
                     static_cast<const MAP_REAL8 *>(in[11]),                                /* nrTimeSlices */
                     static_cast<const MAP_UINT1 *>(in[12]),                                /* structures */
                     static_cast<const MAP_REAL8 *>(in[13]),                                /* structureA */
                     static_cast<const MAP_REAL8 *>(in[14]),                                /* structureB */
                     static_cast<const MAP_REAL8 *>(in[15]));                               /* structureCrestLevel */
}

int Do_lddcreate(void *l, void *d, const void **ins)
{
  auto *ldd = static_cast<MAP_UINT1 *>(l);
  int const r = Lddm(ldd, static_cast<const MAP_REAL8 *>(ins[0]));
  if (r != 0) {
    return r;
  }

  MapInt4 const t(ldd->nrRows, ldd->nrCols);
  return PitRem(static_cast<MAP_UINT1 *>(l), static_cast<MAP_REAL8 *>(d), t.map(), static_cast<const MAP_REAL8 *>(ins[0]),
                static_cast<const MAP_REAL8 *>(ins[1]), static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]),
                static_cast<const MAP_REAL8 *>(ins[4]));
}

int Do_lddcreatend(void *l, void *d, const void **ins)
{

  auto *ldd = static_cast<MAP_UINT1 *>(l);
  int const r = LddmND(ldd, static_cast<const MAP_REAL8 *>(ins[0]));
  if (r != 0) {
    return r;
  }

  MapInt4 const t(ldd->nrRows, ldd->nrCols);
  return PitRemND(static_cast<MAP_UINT1 *>(l), static_cast<MAP_REAL8 *>(d), t.map(), static_cast<const MAP_REAL8 *>(ins[0]),
                  static_cast<const MAP_REAL8 *>(ins[1]), static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]),
                  static_cast<const MAP_REAL8 *>(ins[4]));
}

int Do_areaarea(void *out, const void **ins)
{
  return AreaCount(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]));
}

int Do_areadiversity(void *out, const void **ins)
{
  return AreaDiversity(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_INT4 *>(ins[1]));
}

int Do_areamajority(void *out, const void **ins)
{
  return AreaMajority(static_cast<MAP_INT4 *>(out), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_INT4 *>(ins[1]));
}

int Do_areauniform(void *out, const void **ins)
{
  return AreaUniform(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]));
}

int Do_areanormal(void *out, const void **ins)
{
  return AreaNormal(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]));
}

int Do_catchment(void *out, const void **ins)
{
  return Catch(static_cast<MAP_INT4 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_INT4 *>(ins[1]));
}

int Do_subcatchment(void *out, const void **ins)
{
  return SubCatchment(static_cast<MAP_INT4 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_INT4 *>(ins[1]));
}

int Do_catchmenttotal(void *out, const void **ins)
{
  return PerformCatchStat(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_UINT1 *>(ins[1]));
}

int Do_clump(void *out, const void **ins)
{
  return Clump(static_cast<MAP_INT4 *>(out), static_cast<const MAP_INT4 *>(ins[0]));
}

int Do_downstream(void *out, const void **ins)
{
  return DownStream(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_drain(void *out, const void **ins)
{
  return Drain(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_ldddist(void *out, const void **ins)
{
  return Ldddist(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_UINT1 *>(ins[1]),
                 static_cast<const MAP_REAL8 *>(ins[2]), true);
}

int Do_upstream(void *out, const void **ins)
{
  return Upstream(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_streamorder(void *out, const void **ins)
{
  return StreamOrder(static_cast<MAP_INT4 *>(out), static_cast<const MAP_UINT1 *>(ins[0]));
}

int Do_transient(void *out, const void **ins)
{
  return Transient(&out, ins, 7);
#ifdef EFFE_NIET
  return Transient((MAP_INT4 *)out, (const MAP_REAL8 *)ins[0], /* dem */
                   (const MAP_REAL8 *)ins[1],                  /* recharge */
                   (const MAP_REAL8 *)ins[2],                  /* transmissivity */
                   (const MAP_INT4 *)ins[3],                   /* flow condition */
                   (const MAP_REAL8 *)ins[4],                  /* storage coeff. */
                   (const MAP_REAL8 *)ins[5],                  /* interval */
                   (const MAP_REAL8 *)ins[6]);                 /* tolerance */
#endif
}

int Do_aspect(void *out, const void **ins)
{
  return Orient(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]));
}

int Do_path(void *out, const void **ins)
{
  return Path(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_UINT1 *>(ins[1]));
}

int Do_slope(void *out, const void **ins)
{
  return Slope(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]));
}

int Do_window4total(void *out, const void **ins)
{
  return Window4total(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]));
}

int Do_plancurv(void *out, const void **ins)
{
  return PlanformCurvature(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]));
}

int Do_profcurv(void *out, const void **ins)
{
  return ProfileCurvature(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]));
}

int Do_slopelength(void *out, const void **ins)
{
  return Slopelength(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_view(void *out, const void **ins)
{
  return View(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_UINT1 *>(ins[1]));
}

int Do_extentofview(void *out, const void **ins)
{
  return ExtentOfView(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_inversedistance(void *out, const void **ins)
{
  return Idi(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
             static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]), static_cast<const MAP_REAL8 *>(ins[4]));
}

int Do_windowaverage(void *out, const void **ins)
{
  return WindowAverage(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_markwhilesumle(void *out, const void **ins)
{
  return MarkWhileSumLe(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                        static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_markwhilesumge(void *out, const void **ins)
{
  return MarkUntilSumGe(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                        static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_ellipseaverage(void *out, const void **ins)
{
  return EllipseAverage(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                        static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]));
}

int Do_windowdiversity(void *out, const void **ins)
{
  return WindowDiversity(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_windowhighpass(void *out, const void **ins)
{
  return WindowHighpass(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_windowmajority(void *out, const void **ins)
{
  return WindowMajority(static_cast<MAP_INT4 *>(out), static_cast<const MAP_INT4 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_windowmaximum(void *out, const void **ins)
{
  return WindowMax(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_windowminimum(void *out, const void **ins)
{
  return WindowMin(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_windowtotal(void *out, const void **ins)
{
  return WindowTotal(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_order(void *out, const void **ins)
{
  auto *o = static_cast<MAP_REAL8 *>(out);
  MapInt4 const t(o->nrRows, o->nrCols);
  return Order(o, static_cast<const MAP_REAL8 *>(ins[0]), t.map());
}

int Do_lddmask(void *out, const void **ins)
{
  return MaskLdd(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_UINT1 *>(ins[1]));
}

int Do_move(void *out, const void **ins)
{
  return Move(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_UINT1 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
              static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_shift(void *out, const void **ins)
{
  return Shift(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
               static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_shift0(void *out, const void **ins)
{
  return Shift0(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_lddrepair(void *out, const void **ins)
{
  return RepairLdd(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_UINT1 *>(ins[0]));
}

int Do_brenner(void *out, const void **ins)
{
  return BirdsSpread(static_cast<MAP_UINT1 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                     static_cast<const MAP_REAL8 *>(ins[2]), static_cast<const MAP_REAL8 *>(ins[3]), static_cast<const MAP_REAL8 *>(ins[4]),
                     static_cast<const MAP_REAL8 *>(ins[5]));
}

int Do_influencesimplegauss(void *out, const void **ins)
{
  return InfluenceSimpleGauss(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                              static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_distributesimplegauss(void *out, const void **ins)
{
  auto *o = static_cast<MAP_REAL8 *>(out);
  MapReal8 const t(o->nrRows, o->nrCols);
  return DistributeSimpleGauss(o, t.map(), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                               static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_ibngauss(void *out, const void **ins)
{
  return IBNGauss(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]),
                  static_cast<const MAP_REAL8 *>(ins[2]));
}

int Do_horizontan(void *out, const void **ins)
{
  return HorizonTangent(static_cast<MAP_REAL8 *>(out), static_cast<const MAP_REAL8 *>(ins[0]), static_cast<const MAP_REAL8 *>(ins[1]));
}

int Do_riksfraction(void *out, void const **ins)
{
  return riksFraction(static_cast<MAP_REAL8 *>(out), static_cast<MAP_UINT1 const *>(ins[0]), static_cast<MAP_REAL8 const *>(ins[1]),
                      static_cast<MAP_REAL8 const *>(ins[2]));
}

int Do_squarefraction(void *out, void const **ins)
{
  return squareFraction(static_cast<MAP_REAL8 *>(out), static_cast<MAP_UINT1 const *>(ins[0]), static_cast<MAP_REAL8 const *>(ins[1]),
                        static_cast<MAP_REAL8 const *>(ins[2]));
}

int Do_gradx(void *out, void const **ins)
{
  return vf_gradx(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]));
}

int Do_grady(void *out, void const **ins)
{
  return vf_grady(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]));
}

int Do_divergence(void *out, void const **ins)
{
  return vf_divergence(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]), static_cast<MAP_REAL8 const *>(ins[1]));
}

int Do_diver(void *out, void const **ins)
{
  return vf_diver(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]), static_cast<MAP_REAL8 const *>(ins[1]),
                  static_cast<MAP_REAL8 const *>(ins[2]), static_cast<MAP_REAL8 const *>(ins[3]));
}

int Do_lax(void *out, void const **ins)
{
  return vf_lax(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]), static_cast<MAP_REAL8 const *>(ins[1]));
}

int Do_laplacian(void *out, void const **ins)
{
  return vf_laplacian(static_cast<MAP_REAL8 *>(out), static_cast<MAP_REAL8 const *>(ins[0]));
}

}  // namespace calc
