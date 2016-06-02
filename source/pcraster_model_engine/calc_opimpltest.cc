#define BOOST_TEST_MODULE pcraster model_engine opimpl
#include <boost/test/unit_test.hpp>
#include "geo_rasterspace.h"
#include "com_file.h"
#include "calc_runtimeenv.h"
#include "calc_spatial.h"
#include "calc_nonspatial.h"
#include "com_csfcell.h"
#include "calc_findsymbol.h" // major2op
#include "calc_operator.h"
#include "calc_lookuptable.h"
#include "calc_timetable.h"
#include "calc_domainerror.h"
#include "calc_dvautoptr.h"

// NOTE use string failureExpected in files expected to fail, see style guide


namespace calc {
 //! exec all fields on stack, just a shorthand
 static void execAll(RunTimeEnv& rte, const Operator *o)
 {
   size_t nrArgs=rte.stackSize();
   o->exec(&rte,nrArgs);
 }
}


BOOST_AUTO_TEST_CASE(testSameBin)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  REAL4 v1=1;
  REAL4 vb[NR]={2,0,6};
  pcr::setMV(vb[1]);
  const Operator* a=major2op(OP_BADD);
  { // SS
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_S,vb,NR));
    rte.pushField(new Spatial(VS_S,vb,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());
    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==4);
    BOOST_CHECK(!r->getCell(v,1));
    BOOST_CHECK( r->getCell(v,2));
    BOOST_CHECK(v==12);
  }

  { // NS  - SN
    RunTimeEnv rte(rs);

    for(size_t i=0; i<2; ++i) {
       if (i) {
        // NS
        rte.pushField(new NonSpatial(VS_S,v1));
        rte.pushField(new Spatial(VS_S,vb,NR));
      } else { // SN
        rte.pushField(new Spatial(VS_S,vb,NR));
        rte.pushField(new NonSpatial(VS_S,v1));
      }

      execAll(rte,a);
      DVAutoPtr<Field>r(rte.popField());
      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs() == VS_S);
      double v;
      BOOST_CHECK(r->getCell(v,0));
      BOOST_CHECK(v==3);
      BOOST_CHECK(!r->getCell(v,1));
      BOOST_CHECK( r->getCell(v,2));
      BOOST_CHECK(v==7);
    }
  }
  { // NN as SS
    RunTimeEnv rte(rs);

    rte.pushField(new NonSpatial(VS_S,v1));
    rte.pushField(new NonSpatial(VS_S,v1));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==2);
  }
}

BOOST_AUTO_TEST_CASE(testCover)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  REAL4 v1=1;
  REAL4 vb[NR]={2,0,6};
  pcr::setMV(vb[1]);
  const Operator* a=major2op(OP_COVER);
  { // SS
    RunTimeEnv rte(rs);
    REAL4 vb2[NR]={8,-1,8};

    rte.pushField(new Spatial(VS_S,vb,NR));  // left
    rte.pushField(new Spatial(VS_S,vb2,NR)); // right

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_S);
    REAL4 res[NR]={2,-1,6};
    BOOST_CHECK(std::equal(res,res+NR,r->src_f()));
  }
  { // NS  - SN
    RunTimeEnv rte(rs);

    for(size_t i=0; i<2; ++i) {
       if (i) {
        // NS
        rte.pushField(new NonSpatial(VS_S,v1));
        rte.pushField(new Spatial(VS_S,vb,NR));
      } else { // SN
        rte.pushField(new Spatial(VS_S,vb,NR));
        rte.pushField(new NonSpatial(VS_S,v1));
      }

      execAll(rte,a);
      DVAutoPtr<Field>r(rte.popField());
      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs() == VS_S);
      if (i) {
        REAL4 res[NR]={1,1,1};
        BOOST_CHECK(std::equal(res,res+NR,r->src_f()));
      } else {
        REAL4 res[NR]={2,1,6};
        BOOST_CHECK(std::equal(res,res+NR,r->src_f()));
      }
    }
  }
  { // NN as SS
    RunTimeEnv rte(rs);

    rte.pushField(new NonSpatial(VS_S,v1));
    rte.pushField(new NonSpatial(VS_S,v1));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==1);
  }
}

BOOST_AUTO_TEST_CASE(testSameUn)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  REAL4 v1=1;
  REAL4 vb[NR]={4,0,-6};
  pcr::setMV(vb[1]);
  const Operator* a=major2op(OP_SQRT);
  { // S
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_S,vb,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);

    BOOST_CHECK(r->src_f()[0]==2);
    BOOST_CHECK(pcr::isMV(r->src_f()[1]));
    BOOST_CHECK(pcr::isMV(r->src_f()[2])); // domain error
  }

  { // N
    RunTimeEnv rte(rs);

    rte.pushField(new NonSpatial(VS_S,v1));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0]==1);
  }
}


BOOST_AUTO_TEST_CASE(testDomainError)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  { // N SameUn
    RunTimeEnv rte(rs);
    REAL4 minus1=-1;
    rte.pushField(new NonSpatial(VS_S,minus1));

    bool catched(false);
    try {
     major2op(OP_SQRT)->exec(&rte,1);
    } catch(const calc::DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  { // SN
    RunTimeEnv rte(rs);

    REAL4 vb[NR]={2,0,6};
    pcr::setMV(vb[1]);
    rte.pushField(new Spatial(VS_S,vb,NR));

    REAL4 v0=0;
    rte.pushField(new NonSpatial (VS_S,v0));


    const Operator* a=major2op(OP_FDIV);

    bool catched(false);
    try {
    execAll(rte,a);
    } catch(const calc::DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  { // NN
    RunTimeEnv rte(rs);
    REAL4 v0=0;
    rte.pushField(new NonSpatial (VS_S,v0));
    rte.pushField(new NonSpatial (VS_S,v0));

    const Operator* a=major2op(OP_FDIV);

    bool catched(false);
    try {
    execAll(rte,a);
    } catch(const calc::DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  { // NN  -0.047**1.5; test pcrcalc337a
    RunTimeEnv rte(rs);
    REAL4 vMin= -0.047f;
    REAL4 vFrac= 1.5f;
    rte.pushField(new NonSpatial (VS_S,vMin));
    rte.pushField(new NonSpatial (VS_S,vFrac));

    const Operator* a=major2op(OP_POW);

    bool catched(false);
    try {
    execAll(rte,a);
    } catch(const calc::DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
}

BOOST_AUTO_TEST_CASE(testDiffUn)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
 { // Manual typed in
    RunTimeEnv rte(rs);

    INT4 vb[NR]={2,MV_INT4,6};
    pcr::setMV(vb[1]);
    rte.pushField(new Spatial(VS_O,vb,NR));

    const Operator* a=major2op(OP_MAPMINIMUM);

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    BOOST_CHECK(r->vs()==VS_O);
    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==2);
 }
 { // Spatial
    RunTimeEnv rte(rs);

    INT4 vb[NR]={2,MV_INT4,6};
    pcr::setMV(vb[1]);
    rte.pushField(new Spatial(VS_O,vb,NR));

    const Operator* a=major2op(OP_SPATIAL);

    execAll(rte,a);
    { // nop
      DVAutoPtr<Field>r(rte.popField());

      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs()==VS_O);
      BOOST_CHECK(std::equal(vb,vb+3,r->src_4()));
    }

    rte.pushField(createNonSpatial<INT4>(VS_O,4));
    execAll(rte,a);
    { // did it
      INT4 vb4[NR]={4,4,4};
      DVAutoPtr<Field>r(rte.popField());
      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs()==VS_O);
      BOOST_CHECK(std::equal(vb4,vb4+3,r->src_4()));
    }
 }
 { // Generated DiffUn
    RunTimeEnv rte(rs);

    REAL4 vb[NR]={1,-1,-1};
    pcr::setMV(vb[1]);
    rte.pushField(new Spatial(VS_D,vb,NR));

    const Operator* a=major2op(OP_NODIRECTION);
    execAll(rte,a);

    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_B);

    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==0);
    BOOST_CHECK(!r->getCell(v,1));
    BOOST_CHECK( r->getCell(v,2));
    BOOST_CHECK(v==1);
 }
}

BOOST_AUTO_TEST_CASE(testCompare)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  REAL4 v1=1;
  REAL4 vb[NR]={2,0,6};
  pcr::setMV(vb[1]);
  const Operator* a=major2op(OP_LT);
  { // SS
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_S,vb,NR));
    rte.pushField(new Spatial(VS_S,vb,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_B);
    BOOST_CHECK(r->src_1()[0]==0);
    BOOST_CHECK(r->src_1()[1]==MV_UINT1);
    BOOST_CHECK(r->src_1()[2]==0);
  }
  { // NS  - SN
    RunTimeEnv rte(rs);

    for(size_t i=0; i<2; ++i) {
       if (i) {
        // NS  1 < ( 2,MV, 6) -> (1,MV,1)
        rte.pushField(new NonSpatial(VS_S,v1));
        rte.pushField(new Spatial(VS_S,vb,NR));
      } else { // SN (2,MV, 6) < 1 -> (0,MV,0)
        rte.pushField(new Spatial(VS_S,vb,NR));
        rte.pushField(new NonSpatial(VS_S,v1));
      }

      execAll(rte,a);
      DVAutoPtr<Field>r(rte.popField());

      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs() == VS_B);
      BOOST_CHECK(r->src_1()[0]==i);
      BOOST_CHECK(r->src_1()[1]==MV_UINT1);
      BOOST_CHECK(r->src_1()[2]==i);

    }
  }
  { // NN as SS
    RunTimeEnv rte(rs);

    rte.pushField(new NonSpatial(VS_S,v1));
    rte.pushField(new NonSpatial(VS_S,v1));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    BOOST_CHECK(r->vs() == VS_B);
    BOOST_CHECK(r->src_1()[0]==0);
  }
}

BOOST_AUTO_TEST_CASE(testIfThen)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  UINT1 sCond[NR]={1,1,0};
  REAL4 sTrue[NR]={2,0,6};
  pcr::setMV(sTrue[1]);
  const Operator* a=major2op(OP_IFTHEN);
  { // SS
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,sCond,NR));
    rte.pushField(new Spatial(VS_S,sTrue,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0]==2);
    BOOST_CHECK(pcr::isMV(r->src_f()[1]));
    BOOST_CHECK(pcr::isMV(r->src_f()[2]));
  }
  { // NS
    RunTimeEnv rte(rs);

    rte.pushField(createNonSpatial<UINT1>(VS_B,0));
    rte.pushField(new Spatial(VS_S,sTrue,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(pcr::isMV(r->src_f()[0]));
    BOOST_CHECK(pcr::isMV(r->src_f()[1]));
    BOOST_CHECK(pcr::isMV(r->src_f()[2]));
  }
  { // SN
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,sCond,NR));
    rte.pushField(createNonSpatial<REAL4>(VS_S,7));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0] == 7);
    BOOST_CHECK(r->src_f()[1] == 7);
    BOOST_CHECK(pcr::isMV(r->src_f()[2]));
  }
}

BOOST_AUTO_TEST_CASE(testIfThenElse)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  UINT1 sCond[NR] ={1,1,0};

  REAL4 sTrue[NR] ={2,0,6};
  pcr::setMV(sTrue[1]);

  REAL4 sFalse[NR]={3,4,8};
  const Operator* a=major2op(OP_IFTHENELSE);
  { // SSS
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,sCond,NR));
    rte.pushField(new Spatial(VS_S,sTrue,NR));
    rte.pushField(new Spatial(VS_S,sFalse,NR));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0]==2);
    BOOST_CHECK(pcr::isMV(r->src_f()[1]));
    BOOST_CHECK(r->src_f()[2]==8);
  }
  { // NNS cond false
    RunTimeEnv rte(rs);

    rte.pushField(createNonSpatial<UINT1>(VS_B,0));
    rte.pushField(new Spatial(VS_S,sTrue,NR));
    rte.pushField(createNonSpatial<REAL4>(VS_S,7));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0]==7);
    BOOST_CHECK(r->src_f()[1]==7);
    BOOST_CHECK(r->src_f()[2]==7);
  }
  { // NNS cond true
    RunTimeEnv rte(rs);

    rte.pushField(createNonSpatial<UINT1>(VS_B,1));
    rte.pushField(new Spatial(VS_S,sTrue,NR));
    rte.pushField(createNonSpatial<REAL4>(VS_S,7));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs() == VS_S);
    BOOST_CHECK(r->src_f()[0]==2);
    BOOST_CHECK(pcr::isMV(r->src_f()[1]));
    BOOST_CHECK(r->src_f()[2]==6);
  }
}

BOOST_AUTO_TEST_CASE(testGen)
{
  using namespace calc;

#define NR 3
  const geo::RasterSpace rs(1,NR);
  {
    RunTimeEnv rte(rs);

    const Operator* a=major2op(OP_TIMESLICE);

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(!r->isSpatial());
    BOOST_CHECK(r->vs()==VS_S);
    double v;
    BOOST_CHECK(r->getCell(v,0));
    BOOST_CHECK(v==1);
  }
}

BOOST_AUTO_TEST_CASE(testGlobal)
{
  using namespace calc;

#define NR6 6
  const geo::RasterSpace rs(1,NR6);
  const Operator* a=major2op(OP_AREAMAJORITY);

  {
    RunTimeEnv rte(rs);

    INT4 vbClass[NR6]={1,1,1,8,8,8};
    INT4 vbVals[NR6] ={2,6,6,4,4,1};
    INT4 vbRes[NR6]  ={6,6,6,4,4,4};

    rte.pushField(new Spatial(VS_N,vbVals,NR6));
    rte.pushField(new Spatial(VS_N,vbClass,NR6));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_N);
    BOOST_CHECK(std::equal(vbRes,vbRes+6,r->src_4()));
  }
  {
    RunTimeEnv rte(rs);

    INT4 vbClass[NR6] ={1,1,1,8,8,8};
    UINT1 vbVals[NR6] ={0,1,0,0,1,1};
    UINT1 vbRes [NR6] ={0,0,0,1,1,1};

    rte.pushField(new Spatial(VS_B,vbVals,NR6));
    rte.pushField(new Spatial(VS_N,vbClass,NR6));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_B);
    BOOST_CHECK(std::equal(vbRes,vbRes+6,r->src_1()));
  }
  {
    RunTimeEnv rte(rs);

    INT4 vbClass[NR6] ={1,1,1,8,8,8};
    UINT1 val =1;
    UINT1 vbRes [NR6] ={1,1,1,1,1,1};

    rte.pushField(new NonSpatial(VS_B,val));
    rte.pushField(new Spatial(VS_N,vbClass,NR6));

    execAll(rte,a);
    DVAutoPtr<Field>r(rte.popField());

    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_B);
    BOOST_CHECK(std::equal(vbRes,vbRes+6,r->src_1()));
  }
#ifdef NDEBUG
  {
    RunTimeEnv rte(rs);
    UINT1 vbLdd[NR6] ={6,6,6,6,6,5};
    REAL4 val =-1; // illegal friction material

    rte.pushField(new Spatial(VS_L,vbLdd,NR6));
    rte.pushField(new NonSpatial(VS_S,val));

    const Operator* l=major2op(OP_SLOPELENGTH);
    bool catched=false;
    try {
     execAll(rte,l);
    } catch (const DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);

  }
#else
  // OLS: I think this test never worked in Debug mode
  // sloplength is implemented in C (and others in source/calc)
  // and return error codes instead of raising exceptions
  BOOST_WARN_MESSAGE(false, "OP_SLOPELENGTH triggers partly 'empty exception' assert in Debug build");
#endif
}


BOOST_AUTO_TEST_CASE(testMRF)
{
  using namespace calc;

#define NR6 6
  const geo::RasterSpace rs(1,NR6);

  UINT1 vbPoints[NR6]={1,0,0,0,0,0};
  INT4 vbZone[NR6]   ={1,1,1,1,1,1};
  REAL4 vbCost[NR6]  ={3,4,5,6,7,8};

  {
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,vbPoints,NR6));
    rte.pushField(createNonSpatial<REAL4>(VS_S,3)); // initial costs
    rte.pushField(createNonSpatial<REAL4>(VS_S,1)); // friction

    const Operator* a=major2op(OP_SPREAD_MRF);
    execAll(rte,a);

    DVAutoPtr<Field>r1(rte.popField());
    BOOST_CHECK(r1->isSpatial());
    BOOST_CHECK(r1->vs()==VS_S);
    BOOST_CHECK(std::equal(vbCost,vbCost+6,r1->src_f()));

    DVAutoPtr<Field>r2(rte.popField());
    BOOST_CHECK(r2->isSpatial());
    BOOST_CHECK(r2->vs()==VS_B);
    BOOST_CHECK(std::equal(vbZone,vbZone+6,r2->src_1()));
  }
  { // spread only
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,vbPoints,NR6));
    rte.pushField(createNonSpatial<REAL4>(VS_S,3)); // initial costs
    rte.pushField(createNonSpatial<REAL4>(VS_S,1)); // friction

    const Operator* a=major2op(OP_SPREAD);
    execAll(rte,a);

    DVAutoPtr<Field>r2(rte.popField());
    BOOST_CHECK(r2->isSpatial());
    BOOST_CHECK(r2->vs()==VS_S);
    BOOST_CHECK(std::equal(vbCost,vbCost+6,r2->src_f()));

    BOOST_CHECK(rte.stackSize()==0);
  }
  { // spreadzone only
    RunTimeEnv rte(rs);

    rte.pushField(new Spatial(VS_B,vbPoints,NR6));
    rte.pushField(createNonSpatial<REAL4>(VS_S,3)); // initial costs
    rte.pushField(createNonSpatial<REAL4>(VS_S,1)); // friction

    const Operator* a=major2op(OP_SPREADZONE);
    execAll(rte,a);

    DVAutoPtr<Field>r1(rte.popField());
    BOOST_CHECK(r1->isSpatial());
    BOOST_CHECK(r1->vs()==VS_B);
    BOOST_CHECK(std::equal(vbZone,vbZone+6,r1->src_1()));

    BOOST_CHECK(rte.stackSize()==0);
  }
}

BOOST_AUTO_TEST_CASE(testConversion)
{
  using namespace calc;

#define NR6 6
  const geo::RasterSpace rs(1,NR6);

  UINT1 vbBol[NR6]={1  ,1  ,0,0,1,1};
  INT4 vbNom[NR6] ={3  ,4  ,0,0,7,8};
  REAL4 vbIn[NR6] ={3.3f,4.8f,0.0f,0.0f,7.0f,8.0f};

  {
    RunTimeEnv rte(rs);

    Field *f=new Spatial(VS_S,vbIn,NR6);
    rte.pushField(f);

    const Operator* a=major2op(OP_SCALAR);
    // A NO_OP
    execAll(rte,a);

    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_S);
    BOOST_CHECK(std::equal(vbIn,vbIn+6,r->src_f()));

    //  assert A NO_OP
    BOOST_CHECK(r.get() == f);

  }
  {
    RunTimeEnv rte(rs);

    Field *f=new Spatial(VS_S,vbIn,NR6);
    rte.pushField(f);

    const Operator* a=major2op(OP_NOMINAL);
    execAll(rte,a);

    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_N);
    BOOST_CHECK(std::equal(vbNom,vbNom+6,r->src_4()));
  }
  {
    RunTimeEnv rte(rs);

    Field *f=new Spatial(VS_S,vbIn,NR6);
    rte.pushField(f);

    const Operator* a=major2op(OP_BOOLEAN_);
    execAll(rte,a);

    DVAutoPtr<Field>r(rte.popField());
    BOOST_CHECK(r->isSpatial());
    BOOST_CHECK(r->vs()==VS_B);
    BOOST_CHECK(std::equal(vbBol,vbBol+6,r->src_1()));
  }
}


BOOST_AUTO_TEST_CASE(testLookup)
{
  using namespace calc;

/*
  <!-- should be non-spatial, number 1 -->
  <model>fileoutput lookupnominal(inp_1.tbl, 5, 8, 10);</model>
  <msg>1</msg>
</test>
*/
#define NR6 6
  const geo::RasterSpace rs(1,NR6);

  LookupTable* l(new LookupTable());
  l->setReadOnlyReference(true);

  std::vector<VS> colVs(4,VS_O);
  colVs.back()=VS_N;
  l->setRecords("inp_1.tbl",colVs);

  {
    RunTimeEnv rte(rs);

    rte.pushDataValue(l);
    rte.pushField(createNonSpatial<INT4>(VS_O,5));
    rte.pushField(createNonSpatial<INT4>(VS_O,8));
    rte.pushField(createNonSpatial<INT4>(VS_O,10));

    const Operator* a=major2op(OP_LOOKUPNOMINAL);

    a->exec(&rte,4); // 3 fields and the table
    { // nonspatial result original pcrcalc9 test
      DVAutoPtr<Field>r(rte.popField());

      BOOST_CHECK(!r->isSpatial());
      BOOST_CHECK( r->vs()==VS_N);
      BOOST_CHECK( r->src_4()[0]==1);
    }
  }
  {
    RunTimeEnv rte(rs);

    rte.pushDataValue(l);
    rte.pushField(createNonSpatial<INT4>(VS_O,5));
    INT4 vb[NR6] = { 8, 4, 4, 8, MV_INT4, MV_INT4 };
    rte.pushField(new Spatial(VS_O,vb,NR6));
    rte.pushField(createNonSpatial<INT4>(VS_O,10));

    const Operator* a=major2op(OP_LOOKUPNOMINAL);

    a->exec(&rte,4); // 3 fields and the table
    {
      DVAutoPtr<Field>r(rte.popField());

      INT4 vbRes[NR6] = { 1, 0, 0, 1, MV_INT4, MV_INT4 };
      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs()==VS_N);
      BOOST_CHECK(std::equal(vbRes,vbRes+NR6,r->src_4()));
    }
  }
  deleteAlways(l);
}

//! test class TimeinputTssOp
BOOST_AUTO_TEST_CASE(testTimeinputTssOp)
{
  using namespace calc;

  struct SetTimer {
    void operator()(RunTimeEnv& rte, size_t c) {
     Timer t(rte.timer());
     t.setCurrentInt(c);
     rte.setTimer(t);
    }
  } setTimer;

#define NR6 6
  const geo::RasterSpace rs(1,NR6);

  // 2 steps with 3 columns
  // -1 is important (--small cellrepr of ordinal testing)
  com::write("1 10 100 -1\n2 20 200 1e31","testTimeinputTssOp.tss");
  TimeTable* l(new TimeTable("testTimeinputTssOp.tss",VS_O,1));
  l->setReadOnlyReference(true);

  const Operator* a=major2op(OP_TIMEINPUTORDINAL);
  {
    RunTimeEnv rte(rs);
    setTimer(rte,2);

    rte.pushDataValue(l);
    rte.pushField(createNonSpatial<INT4>(VS_O,2));

    a->exec(&rte,2); // 1 id-field and the tss
    { // nonspatial result
      DVAutoPtr<Field>r(rte.popField());

      BOOST_CHECK(!r->isSpatial());
      BOOST_CHECK(r->vs()==VS_O);
      BOOST_CHECK(r->src_4()[0]==200);
    }
  }

  {
    RunTimeEnv rte(rs);
    setTimer(rte,2);

    INT4 vb[NR6] =    {  2, 1,        0,   2, 3,       MV_INT4 };
    INT4 vbRes[NR6] = {200, 20, MV_INT4, 200, MV_INT4, MV_INT4 };

    rte.pushDataValue(l);
    rte.pushField(new Spatial(VS_O,vb,NR6));

    a->exec(&rte,2); // 1 id-field and the tss
    {
      DVAutoPtr<Field>r(rte.popField());

      BOOST_CHECK(r->isSpatial());
      BOOST_CHECK(r->vs()==VS_O);
      BOOST_CHECK(std::equal(vbRes,vbRes+NR6,r->src_4()));
    }

    // also test too short of timesteps, can happen runtime
    setTimer(rte,4);

    rte.pushDataValue(l);
    rte.pushField(new Spatial(VS_O,vb,NR6));
    bool catched=false;
    try {
     a->exec(&rte,2); // 1 id-field and the tss
    } catch(const DomainError& ) {
      catched=true;
    }
    BOOST_CHECK(catched);
  }
  deleteAlways(l);
}
