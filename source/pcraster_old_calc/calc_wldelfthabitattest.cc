#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_WLDELFTHABITATTEST
#include "calc_wldelfthabitattest.h"
#define INCLUDED_CALC_WLDELFTHABITATTEST
#endif

// Library headers.
#ifndef INCLUDED_BOOST_SHARED_PTR
#include <boost/shared_ptr.hpp>
#define INCLUDED_BOOST_SHARED_PTR
#endif

#ifndef INCLUDED_BOOST_TEST_TEST_TOOLS
#include <boost/test/test_tools.hpp>
#define INCLUDED_BOOST_TEST_TEST_TOOLS
#endif

#ifndef INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#include <boost/test/unit_test_suite.hpp>
#define INCLUDED_BOOST_TEST_UNIT_TEST_SUITE
#endif

// PCRaster library headers.
#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif
#ifndef INCLUDED_COM_CLONE
#include "com_clone.h"
#define INCLUDED_COM_CLONE
#endif
// Module headers.
#ifndef INCLUDED_CALC_WLDELFTHABITAT
#include "calc_wldelfthabitat.h"
#define INCLUDED_CALC_WLDELFTHABITAT
#endif
#ifndef INCLUDED_CALC_STATTABLE
#include "calc_stattable.h"
#define INCLUDED_CALC_STATTABLE
#endif

/*!
  \file
  This file contains the implementation of the WlDelftHabitatTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC WLDELFTHABITAT MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::WlDelftHabitatTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<WlTestStat> instance(new WlTestStat());

  suite->add(BOOST_CLASS_TEST_CASE(&WlDelftHabitatTest::testStatTable, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&WlDelftHabitatTest::testWLStatTests, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&WlDelftHabitatTest::testXmlParsing, instance));

  return suite;
}

namespace calc {
 static com::PathName resultName(const com::PathName& subject) {
   com::PathName pn(subject);
   pn.setExtension("statTableTest");
   return pn;
 }
 static com::PathName resultName(const com::PathName& subject,
                                 const com::PathName& cross) {
   com::PathName pnS(subject);
   pnS.removeExtension();
   com::PathName pnC(cross);
   pnC.removeExtension();
   std::string s(pnC.toString()+"_X_"+pnS.toString());
   return resultName(s);
 }
 class WlTestStat : public WlDelftHabitat {
  public:
   WlTestStat():
     WlDelftHabitat("Case1.xml") // bogus for test
   {}
   StatTable *addStatTest(
    const com::PathName& result,
    const std::string& subjectName,
    const std::string& crossName="")
   {
     StatTable *s=addStatistics(subjectName);
     s->setVerbose(true);
     s->setResultTable(result);
     if (!crossName.empty())
       s->setCross(addFieldExpr(crossName));
     return s;
   }
 };

};

//------------------------------------------------------------------------------
// DEFINITION OF WLDELFTHABITAT MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::WlDelftHabitatTest::WlDelftHabitatTest()
{
}



//! setUp
void calc::WlDelftHabitatTest::setUp()
{
  { // test 1
    geo::RasterSpace rs(2,3);
    {
      com::PathName bil("ST_I4");
      INT4 d[]={1,2,3,3,5,6};
      geo::createBil<INT4>(bil,rs,d);
    }
    {
      com::PathName bil("ST_I1");
      UINT1 d[]={1,2,3,3,5,6};
      geo::createBil<UINT1>(bil,rs,d);
    }
    {
      com::PathName bil("ST_R4");
      REAL4 d[]={1,2,3,3,5,6};
      geo::createBil<REAL4>(bil,rs,d);
    }
  }
  {
    geo::RasterSpace rs(6,6);
    rs.setCellSize(10);
    {
      INT4 d[] = {
             1,1,1,2,2,2
            ,1,1,1,2,2,2
            ,1,1,1,2,2,2
            ,3,3,3,3,3,3
            ,3,3,3,4,4,-999
            ,3,3,3,4,-999,-999 };
      POSTCOND(ARRAY_SIZE(d)==36);
      com::PathName bil("ecotoop");
      geo::createBil<INT4>(bil,rs,d,-999);
    }
    {
      INT4 d[] = {
           1,1,1,2,2,2
          ,1,1,1,2,2,2
          ,1,1,1,2,2,2
          ,3,3,3,3,3,3
          ,3,3,3,4,4,-999
          ,-999,3,3,4,-999,-999 };
      POSTCOND(ARRAY_SIZE(d)==36);
      com::PathName bil("ecotoop2");
      geo::createBil<INT4>(bil,rs,d,-999);
    }
    {
      REAL4 d[] = {
           0.1,0.1,0.1  ,1,1,1
          ,0.1,0.1,0.1  ,1,1,1
          ,0.1,0.1,0.1  ,0.5,0.5,0.5
          ,0,0,0  ,0.21 ,0.2,0.2
          ,0,0,0  ,0.23 ,0.2,-999
          ,0,0,0  ,0.29 ,-999,-999 };
      POSTCOND(ARRAY_SIZE(d)==36);
      com::PathName bil("habschik");
      geo::createBil<REAL4>(bil,rs,d,-999);
    }
    {
      REAL4 d[] = {
           -0.2 ,1.1  ,0.1,1,1,1
          ,0.1  ,0.1  ,0.1,1,1,1
          ,0.1  ,0.1  ,0.1,0.5,0.5,0.5
          ,0.2 ,0.3 ,0,0.2,0.2,0.2
          ,0.2 ,0.3 ,0,0.2,0.2,-999
          ,0.2 ,0.3 ,0,0.2,-999,-999 };
      POSTCOND(ARRAY_SIZE(d)==36);
      com::PathName bil("habschik2");
      geo::createBil<REAL4>(bil,rs,d,-999);
    }
  }
}

//! tearDown
void calc::WlDelftHabitatTest::tearDown()
{
}

void calc::WlDelftHabitatTest::testXmlParsing()
{
 {
  com::PathName pn("Case1.xml");
  WlDelftHabitat h(pn);
  h.parseXml();
  h.execute();
 }
 { // Stat test, result is checked in epilog
  com::PathName pn("Case8.xml");
  WlDelftHabitat h(pn);
  h.parseXml();
  h.execute();
 }
}


void calc::WlDelftHabitatTest::testStatTable()
{
  {
    com::PathName bil("ST_I4");
    WlTestStat h;
    h.addStatTest(resultName(bil),bil.toString());

    // cross with itself
    h.addStatTest(resultName(bil,bil),
        bil.toString(),
        bil.toString());

    // croos with other
    com::PathName c1("ST_R4");
    h.addStatTest(resultName(bil,c1),
        bil.toString(),
        c1.toString());
    h.execute();
  }
  {
    com::PathName bil("ST_I1");
    WlTestStat h;
    h.addStatTest(resultName(bil),bil.toString());
    h.execute();
  }
  {
    com::PathName bil("ST_R4");
    WlTestStat h;
    h.addStatTest(resultName(bil),bil.toString());
    h.execute();
  }
}

void calc::WlDelftHabitatTest::testWLStatTests()
{
  //Grenzen subject
  char *gSText[] = { " 0",
               "<0,0.2]",
               "<0.2,0.4]",
               "<0.4,0.6]",
               "<0.6,0.8]",
               "<0.8,1]" };
  typedef std::vector<const com::IntervalF *>  IV;
  IV gS,
     oS, // overlap
     kS; // kS -> klassen
  for(size_t i=0; i < ARRAY_SIZE(gSText); ++i) {
    gS.push_back(
        com::createIntervalFromLookupTableKey<float>(gSText[i]));
    kS.push_back(new com::EqualTo<float>(i));
  }
  oS.push_back(new com::GreaterThan<float>(0.25));
  oS.push_back(new com::GreaterThan<float>(0.3));

  struct KaartInterval {
    const char *name;
    IV         *iv;
  };
  enum TYPE { S=0, C=1};
  struct TestCase {
    KaartInterval k[2];
    const char *tableName;
    const KaartInterval& operator[](int i) const { return k[i]; };
  };

TestCase tests[] = {
{ { { "habschik", &gS },{ "habschik2", 0 }}, "kruis3Tbl.txt" },
{ { { "habschik2", &oS },{ "habschik", 0 }}, "kruis3_GrensOverlapTbl.txt" },
{ { { "habschik2",0   },{ "habschik",  &gS }}, "kruis3_R_Tbl.txt" },
{ { { "ecotoop", 0   }, { 0, 0 }          }, "type1_geenKlassenTbl.txt" },
{ { { "ecotoop", &kS }, { 0, 0 }          }, "type1_metKlassenTbl.txt" },
{ { { "habschik", 0 },  { 0, 0 }          }, "type2Tbl.txt" },
{ { { "ecotoop", 0   }, { "ecotoop2", 0 } }, "kruis1_geenKlassenTbl.txt" },
{ { { "ecotoop", &kS }, { "ecotoop2", 0 } }, "kruis1_metRijKlassenTbl.txt" },
{ { { "ecotoop", 0   }, { "habschik", 0 } }, "kruis2_geenKlassenTbl.txt" },
{ { { "habschik", 0  }, { "ecotoop", 0  } }, "kruis2_R_geenKlassenTbl.txt" },
{ { { "ecotoop", &kS }, { "habschik", 0 } }, "kruis2_metKlassenTbl.txt" },
{ { { "ecotoop", &gS }, { "habschik", 0 } }, "negeerGrenzenTbl.txt" },
{ { { "habschik", 0 },  { "habschik2", 0 } }, "type2DubbelTbl.txt" },
{ { { "habschik",&gS }, { 0, 0          } }, "kruis3_rijIsKolTbl.txt" },
{ { { "habschik",&gS }, { "habschik", 0 } }, "kruis3_rijIsKol2Tbl.txt" },
{ { { "ecotoop", 0   },  { "habschik", &gS}},"KGTbl.txt" },
{ { { "habschik", &gS }, { "ecotoop", 0  }}, "KG_R_Tbl.txt" },
{ { { "ecotoop", 0   }, { "habschik2", &gS }}, "KGBuitenGrenzenTbl.txt" },
{ { { "ecotoop", 0   }, { "habschik2", &oS }}, "KGOverlapGrenzenTbl.txt" },
{ { { "habschik",&gS }, { "habschik2", &gS }}, "GGTbl.txt" },
{ { { "habschik",&gS }, { "habschik2", &oS }}, "GGOverlapGrenzenTbl.txt" },
{ { { "habschik2",&oS },{ "habschik", &gS  }}, "GG2OverlapGrenzenTbl.txt" },
{ { { "habschik2",0   },{ "habschik",  0   }}, "type2_2XTbl.txt" }

};

  for(size_t i=0; i < ARRAY_SIZE(tests); ++i) {
    const TestCase& t(tests[i]);
    PRECOND(t[S].name);
    // std::cout << "starting " << t.tableName << "\n";

    WlTestStat h;
    StatTable *s=
     h.addStatTest(t.tableName,t[S].name);
    s->setSubjectName(t[S].name);
    if (t[S].iv)
      s->setSubjectIntervals(*(t[S].iv));
    if (t[C].name) {
       s->setCross(h.addFieldExpr(t[C].name));
       s->setCrossName(t[C].name);
       if (t[C].iv)
        s->setCrossIntervals(*(t[C].iv));
    }
    h.execute();
    // std::cout << "finished " << t.tableName << "\n";
  }

  com::clearClone(gS);
  com::clearClone(kS);
  com::clearClone(oS);
}
