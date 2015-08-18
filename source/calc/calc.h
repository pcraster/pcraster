#ifndef __CALC__H
#define __CALC__H

#ifdef __cplusplus
 extern "C" {
#endif

/*
 * calc.h
 */

#include "api.h"

/* accu.c */
int Accu(
  MAP_REAL8 *state,     /* write-only output state */
  MAP_REAL8 *flux,     /* read-write flux output */
  const MAP_UINT1 *ldd,     /* ldd map */
  const MAP_REAL8 *amount);  /* amount input */

int AccuLtc(
  MAP_REAL8 *state,     /* write-only output state */
  MAP_REAL8 *flux,     /* read-write flux output */
  const MAP_UINT1 *ldd,     /* ldd map */
  const MAP_REAL8 *amount,  /* amount input */
  const MAP_REAL8 *transcap);  /* transport capacity */

int  Diffuse1(
     MAP_REAL8 *stateOut,    /* Read-write state map */
     MAP_REAL8 *fluxOut,    /* Read-write fluxOut map */
     const MAP_REAL8 *dir,     /* dir map */
     const MAP_REAL8 *in,     /* in map */
     const MAP_REAL8 *fluxIn);    /* fluxIn map */

int AccuTt(
  MAP_REAL8 *state,     /* write-only output state */
  MAP_REAL8 *flux,     /* read-write flux output */
  const MAP_UINT1 *ldd,     /* ldd map */
  const MAP_REAL8 *amount,  /* amount input */
  const MAP_REAL8 *threshold);  /* transport threshold */

int AccuFraction(
  MAP_REAL8 *state,     /* write-only output state */
  MAP_REAL8 *flux,     /* read-write flux output */
  const MAP_UINT1 *ldd,     /* ldd map */
  const MAP_REAL8 *amount,  /* amount input */
  const MAP_REAL8 *fracflux);  /* fraction of the flux */

int AccuTrigger(
  MAP_REAL8 *state,     /* write-only output state */
  MAP_REAL8 *flux,     /* read-write flux output */
  const MAP_UINT1 *ldd,     /* ldd map */
  const MAP_REAL8 *amount,  /* amount input */
  const MAP_REAL8 *trigger);  /* trigger of the flux */

/* upstream.c */
extern int Upstream(MAP_REAL8 *out,
              const MAP_UINT1 *ldd,
              const MAP_REAL8 *in);

/* dist.c */
extern int Ldddist(
  MAP_REAL8 *out,     /* write - only output map */
  const MAP_UINT1 *ldd,    /* ldd */
  const MAP_UINT1 *points,  /* BOOLEAN points map */
  const MAP_REAL8 *weight,  /* scalar weight map */
  BOOL  useWeightedFriction); /* true -> old ldddist, false in traveltime use */
extern int Downstreamtotal(
     MAP_REAL8 *out,                  /* write-only output map  */ 
     const MAP_UINT1 *ldd,             /* ldd map            */
     const MAP_REAL8 *amount);           /* amount map */

/* catch.c */
int Catch(
     MAP_INT4 *out,      /* Read-write output map  */
     const MAP_UINT1 *ldd,     /* ldd map  */
     const MAP_INT4 *points);     /* points map  */
int SubCatchment(
     MAP_INT4 *out,      /* Read-write output map  */
     const MAP_UINT1 *ldd,     /* ldd map  */
     const MAP_INT4 *points);     /* points map  */

/* lddm.c */
int Lddm(
    MAP_UINT1 *out1,  /* Read-write output map */
    const MAP_REAL8 *dem);  /* Dig. Elevation Map */
/* lddmnd.c */
int LddmND(
    MAP_UINT1 *out1,  /* Read-write output map */
    const MAP_REAL8 *dem);  /* Dig. Elevation Map */

/* pitrem.c */
int PitRem(
    MAP_UINT1 *out1,  /* Read-write output ldd map */
    MAP_REAL8 *out2,       /* Read-write modified dem.map */
    MAP_INT4 *catchMap,  /* read-write cathment map */
    const MAP_REAL8 *dem,  /* Dig. Elevation Map */
    const MAP_REAL8 *depth,  /* allowable depth */
    const MAP_REAL8 *volume,/* allowable volume */
    const MAP_REAL8 *area,  /* allowable area  */
    const MAP_REAL8 *mminput);  /* allowable mminput */
/* pitremnd.c */
int PitRemND(
    MAP_UINT1 *out1,  /* Read-write output ldd map */
    MAP_REAL8 *out2,       /* Read-write modified dem.map */
    MAP_INT4 *catchMap,  /* read-write cathment map */
    const MAP_REAL8 *dem,  /* Dig. Elevation Map */
    const MAP_REAL8 *depth,  /* allowable depth */
    const MAP_REAL8 *volume,/* allowable volume */
    const MAP_REAL8 *area,  /* allowable area  */
    const MAP_REAL8 *mminput);  /* allowable mminput */


/* slopelen.c */
int Slopelength(
  MAP_REAL8 *out,      /* write-only output map */
  const MAP_UINT1 *ldd,    /* ldd map */
  const MAP_REAL8 *friction);  /* friction map */

/* path.c */
int Path(
     MAP_UINT1 *out,      /* write-only output map  */
     const MAP_UINT1 *ldd,     /* ldd map  */
     const MAP_UINT1 *points);     /* points map  */

/* spreadldd.c */
int SpreadLdd(
     MAP_REAL8 *out1,      /* write-only output map  */
     MAP_INT4 *out2,      /* read-write output map  */
     const MAP_UINT1 *ldd,     /* ldd map    */
     const MAP_INT4 *points,    /* points  */
     const MAP_REAL8 *initCost,    /* initial costs */
     const MAP_REAL8 *friction);    /* friction */

/* birds.c */
extern int BirdsSpread(MAP_UINT1 *occupied, const MAP_REAL8 *nrBirds,
const MAP_REAL8 *cost, const MAP_REAL8 *friction, const MAP_REAL8 *maxRange,
const MAP_REAL8 *dispRange,
const MAP_REAL8 *habQual);

/* sprdmax.c */
extern int SpreadMax(MAP_REAL8 *outCost, MAP_INT4 *outId, const MAP_INT4 *points, const MAP_REAL8 *cost, const MAP_REAL8 *friction, const MAP_REAL8 *maxCost);

/* spread.c */
int Spread(
     MAP_REAL8 *out1,      /* write-only output map  */
     MAP_INT4 *out2,      /* read-write output map  */
     const MAP_INT4 *points,    /* points  */
     const MAP_REAL8 *initCost,    /* initial costs */
     const MAP_REAL8 *friction);  /* friction */

/* clump.c */
int Clump(
  MAP_INT4 *out,      /* write-only output map */
  const MAP_INT4 *in);    /* input map */

/* drain.c */
int Drain(
     MAP_REAL8 *out,      /* write-only output map  */
     const MAP_REAL8 *dem,     /* dem map  */
     const MAP_REAL8 *points);     /* points map  */

/* area.c */
int AreaMin(
     MAP_REAL8 *min,      /* Read-write output map  */
     const MAP_REAL8 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaUniform(
     MAP_REAL8 *result,      /* write output map  */
     const MAP_INT4 *classMap);    /* classses map  */

int AreaNormal(
     MAP_REAL8 *result,      /* write output map  */
     const MAP_INT4 *classMap);    /* classses map  */

int AreaMax(
     MAP_REAL8 *max,      /* Read-write output map  */
     const MAP_REAL8 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaDiversity(
     MAP_REAL8 *diversity,    /* Read-write output map  */
     const MAP_INT4 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaAverage(
     MAP_REAL8 *average,    /* Read-write output map  */
     const MAP_REAL8 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaMajority(
     MAP_INT4 *majority,    /* Read-write output map  */
     const MAP_INT4 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaTotal(
     MAP_REAL8 *total,      /* Read-write output map  */
     const MAP_REAL8 *val,     /* value map  */
     const MAP_INT4 *classMap);     /* classes map  */

int AreaCount(
  MAP_REAL8 *count,    /* Read-write output map */
  const MAP_INT4 *classMap);    /* classes map */

/* window.c */
int WindowMin(
  MAP_REAL8 *min,      /* Read-write output map */
  const MAP_REAL8 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowMax(
  MAP_REAL8 *max,      /* Read-write output map */
  const MAP_REAL8 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowDiversity(
  MAP_REAL8 *diversity,    /* Read-write output map */
  const MAP_INT4 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowAverage(
  MAP_REAL8 *average,    /* Read-write output map */
  const MAP_REAL8 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowTotal(
  MAP_REAL8 *total,    /* Read-write output map */
  const MAP_REAL8 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowMajority(
  MAP_INT4 *majority,    /* Read-write output map */
  const MAP_INT4 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

int WindowHighpass(
  MAP_REAL8 *highpass,    /* Read-write output map */
  const MAP_REAL8 *val,    /* value map */
  const MAP_REAL8 *winsize);  /* window size map */

/* repair.c */
extern int RepairLdd(MAP_UINT1 *outLdd, const MAP_UINT1 *inLdd);
extern int MaskLdd(MAP_UINT1 *outLdd,
                   const MAP_UINT1 *inLdd,
                   const MAP_UINT1 *mask);
/* dwnstrm.c */
int DownStream(
  MAP_REAL8 *out,      /* write-only output map */
  const MAP_UINT1 *inLdd,    /* input ldd map */
  const MAP_REAL8 *amount);  /* input value map */

/* view.c */
int View(
  MAP_UINT1 *out,      /* write-only output map */
  const MAP_REAL8 *dem,    /* input dem map */
  const MAP_UINT1 *points);  /* input points map */

/* extentofview.cc */
int ExtentOfView(
  MAP_REAL8 *out,      /* write-only output map */
  const MAP_INT4* classes,    /* input classes map */
  const MAP_REAL8* nrRays);  /* nr rays */

/* slope.c */
extern int Slope(MAP_REAL8 *slope, const MAP_REAL8 *dem);
extern int Window4total(MAP_REAL8 *out, const MAP_REAL8 *in);

/* orient.c */
int Orient(
  MAP_REAL8 *out,      /* write-only output map */
  const MAP_REAL8 *dem);    /* input dem map */

/* curv.c */
extern int ProfileCurvature(MAP_REAL8 *curv, const MAP_REAL8 *dem);
extern int PlanformCurvature(MAP_REAL8 *curv, const MAP_REAL8 *dem);

/* order.c */
int Order(
           MAP_REAL8 *out,    /* Read-write output map  */
     const MAP_REAL8 *in,
           MAP_INT4  *tmp);     /* Read-write input map  */

typedef struct TIME_TABLE{
  CSF_VS vs;    /* value scale */
  int nrSteps;    /* number of steps in table */
  int nrCols;    /* number of colums of table
                           * with time-index in column 0!
                           */
  REAL8 **vals;    /* values in table */
}TIME_TABLE;

/* timein.c */
extern int TimeInputSeries(MAP_REAL8 *out, const MAP_INT4 *id, const TIME_TABLE *t, int timeStep);
extern TIME_TABLE *ReadTimeInputTable(const char *fileName, int nrFirstStepsToSkip, int nrStepsToRead, CSF_VS vs);
extern TIME_TABLE *NewTimeTable(CSF_VS vs, int nrSteps);
extern void FreeTimeTable(TIME_TABLE *t);
#ifdef DEBUG
 extern int nrTimeTables;
#endif
/* area.c */
int AddToTssRowINT4(
  REAL8 *data,    /* write values, starts at col 1 of TIME_TABLE row! */
  size_t nrData,           /* nr of Cols of data */
  const MAP_INT4 *id,  /* id map */
  const MAP_INT4 *expr);  /* expression map */
int AddToTssRowREAL8(
  REAL8 *data,    /* write values, starts at col 1 of TIME_TABLE row! */
  size_t nrData,           /* nr of Cols of data */
  const MAP_INT4 *id,  /* id map */
  const MAP_REAL8 *expr); /* expression map */

/* summary.c */
struct TIME_TABLE *CreateSummaryTable(
  int nrTimeSteps,
  CSF_VS vs);

struct TIME_TABLE *AddToSummaryTable(
  struct TIME_TABLE *t,
  const MAP_REAL8 *expr,
  int currTimeStep);

/* move.c */
extern int Move(MAP_UINT1 *result, const MAP_UINT1 *in,
                const MAP_REAL8 *x, const MAP_REAL8 *y);
extern int Shift(
       MAP_REAL8       *result,   /* Read-write output map  */
       const MAP_REAL8 *in,
       const MAP_REAL8  *y,  /* Integer Northing */
       const MAP_REAL8  *x); /* Integer Easting */
extern int Shift0(
       MAP_REAL8       *result,   /* Read-write output map  */
       const MAP_REAL8 *in,
       const MAP_REAL8  *y,  /* Integer Northing */
       const MAP_REAL8  *x); /* Integer Easting */
/* strorder.c */
extern int StreamOrder(MAP_INT4 *order, const MAP_UINT1 *ldd);

/* kinemati.c */
extern int Kinematic(MAP_REAL8 *Qnew, const MAP_UINT1 *ldd, const MAP_REAL8 *Qold, const MAP_REAL8 *q, const MAP_REAL8 *alpha, const MAP_REAL8 *beta, const MAP_REAL8 *deltaT, const MAP_REAL8 *deltaX);

/* dynwave.cc */
extern int DynamicWave(
           MAP_REAL8 *resultQ,
           MAP_REAL8 *resultH,
     const MAP_UINT1 *in01ldd,
     const MAP_REAL8 *in02inQ,
     const MAP_REAL8 *in03inH,
     const MAP_REAL8 *in04bottomHeight,
     const MAP_REAL8 *in05roughness,
     const MAP_REAL8 *in06channelLength,
     const MAP_REAL8 *in07channelBottomWidth,
     const MAP_REAL8 *in08channelDepth,
     const MAP_REAL8 *in09channelForm,
     const MAP_REAL8 *in10floodplainWidth,
     const MAP_REAL8 *in11timeStepInSeconds,
     const MAP_REAL8 *in12nrTimeSlices,
     const MAP_UINT1 *in13structures,
     const MAP_REAL8 *in14structureA,
     const MAP_REAL8 *in15structureB,
     const MAP_REAL8 *in16structureCrestLevel);

/* tt.c */
extern int TravelTime(MAP_REAL8 *state, MAP_REAL8 *flux, MAP_REAL8 *removed,
const MAP_UINT1 *ldd, const MAP_REAL8 *amount, const MAP_REAL8 *velo,
const MAP_REAL8 *fraction);

/* catcstat.c */
extern int PerformCatchStat(MAP_REAL8 *result,
        const MAP_REAL8 *value, const MAP_UINT1 *ldd);

/* horizonangle.c */
extern int HorizonTangent(MAP_REAL8 *result,
        const MAP_REAL8 *dem, const MAP_REAL8 *angle);
int InfluenceSimpleGauss(
     MAP_REAL8 *out,
     const MAP_REAL8 *input,
     const MAP_REAL8 *range,
     const MAP_REAL8 *eps);
int DistributeSimpleGauss(
     MAP_REAL8 *m_out,
     MAP_REAL8 *m_tmp,
     const MAP_REAL8 *m_input,
     const MAP_REAL8 *m_range,
     const MAP_REAL8 *m_eps);
int IBNGauss(
     MAP_REAL8 *out,
     const MAP_REAL8 *units,
     const MAP_REAL8 *range,
     const MAP_REAL8 *nrPackages);

/* ellipse.c */
extern int EllipseAverage(MAP_REAL8 *average, const MAP_REAL8 *val, const MAP_REAL8 *xmajor, const MAP_REAL8 *yminor, const MAP_REAL8 *angle);

/* idi.cc */
extern int Idi(
     MAP_REAL8 *resultMap,      /* read-write output map  */
     const MAP_UINT1 *mask,     /* points to be spread */
     const MAP_REAL8 *input,    /* initial costs */
     const MAP_REAL8 *idiPow,   /* idi pow */
     const MAP_REAL8 *m_radius,/* max search radius (unit cell) */
     const MAP_REAL8 *m_maxNr); /* max. nr points */

/* marklowest.cc */
extern int MarkWhileSumLe(
     MAP_UINT1 *m_resultMap,
     const MAP_REAL8 *m_order,
     const MAP_REAL8 *m_amount,
     const MAP_REAL8 *m_treshold);
extern int MarkUntilSumGe(
     MAP_UINT1 *m_resultMap,
     const MAP_REAL8 *m_order,
     const MAP_REAL8 *m_amount,
     const MAP_REAL8 *m_treshold);

extern int Transient(void** out, const void** in, int nrArgs);

extern int riksFraction(
         MAP_REAL8* result,
         MAP_UINT1 const* cells,
         MAP_REAL8 const* innerRadius,
         MAP_REAL8 const* outerRadius);

extern int squareFraction(
         MAP_REAL8* result,
         MAP_UINT1 const* cells,
         MAP_REAL8 const* innerRadius,
         MAP_REAL8 const* outerRadius);

/* ldd.c */
#define NR_LDD_DIR 9
#define LDD_PIT 5
#define INVALID_LDD_DIR 10
#define SCALE ? Side():Diagonal()

#define FOR_ALL_LDD_NBS(i)   for(i=1 ; i<INVALID_LDD_DIR; i+=(i!=4)?1:2)
#define FOR_ALL_LDD_NONDIAGONAL_NBS(i)   for(i=2 ; i<INVALID_LDD_DIR; i+=2)
#define IS_VALID_LDD_CODE(i) ( (i) > 0 && (i) < INVALID_LDD_DIR )

#define COORD_EQ(r1,c1,r2,c2) (((r1)==(r2))&&((c1)==(c2)))
#define COORD_NE(r1,c1,r2,c2) (((r1)!=(r2))||((c1)!=(c2)))

extern BOOL FlowsTo(int lddFrom,   /* ldd value from (rFrom, cFrom) */
       int rFrom,    /* row from source cell */
       int cFrom,   /* column from source cell */
       int rTo,    /* row possible destination cell */
       int cTo);       /* column possible destination cell */

extern int DownStrR( int rowNr,   int index);
extern int DownStrC( int colNr, int index);
#ifdef DEBUG
 extern int RNeighbor( int rowNr, int index);
 extern int CNeighbor( int colNr, int index);
#else
# define CNeighbor DownStrC
# define RNeighbor DownStrR
#endif


extern BOOL NoInput(
        const MAP_UINT1 *ldd,  /* ldd.map */
        int rowNr,     /* row of current cell*/
        int colNr);   /* column of current cell */

extern UINT1 Ldddir(int r, int c, int rDS, int cDS);

extern int SumFluxUps(
    REAL8 *newState,  /* write-only new state */
    const MAP_REAL8 *flux,  /* map with fluxes */
    const MAP_UINT1 *ldd,  /* ldd map */
    int r,      /* row of current cell */
    int c);      /* column of current cell */
extern BOOL Corner(int direction);

extern int vf_gradx(MAP_REAL8 *result, const MAP_REAL8 *scalar);
extern int vf_grady(MAP_REAL8 *result, const MAP_REAL8 *scalar);
extern int vf_divergence(MAP_REAL8 *result,
  const MAP_REAL8 *vectorX,
  const MAP_REAL8 *vectorY);
extern int vf_diver(
    MAP_REAL8 *result,
    const MAP_REAL8 *vectorfieldx,
    const MAP_REAL8 *vectorfieldy,
    const MAP_REAL8 *deltax,
    const MAP_REAL8 *deltay);

extern int vf_lax(MAP_REAL8 *result,
  MAP_REAL8 const* input,
  MAP_REAL8 const* fraction);

extern int vf_laplacian(MAP_REAL8 *result, const MAP_REAL8 *input);


#ifdef __cplusplus
 }
#endif

#endif /* __CALC__H */
