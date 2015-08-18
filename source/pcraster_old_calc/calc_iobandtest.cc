#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CALC_IOBANDTEST
#include "calc_iobandtest.h"
#define INCLUDED_CALC_IOBANDTEST
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
#ifndef INCLUDED_COM_PATHINFO
#include "com_pathinfo.h"
#define INCLUDED_COM_PATHINFO
#endif
#ifndef INCLUDED_COM_PATHNAME
#include "com_pathname.h"
#define INCLUDED_COM_PATHNAME
#endif
#ifndef INCLUDED_COM_ALGORITHM
#include "com_algorithm.h"
#define INCLUDED_COM_ALGORITHM
#endif
#ifndef INCLUDED_COM_FILE
#include "com_file.h"
#define INCLUDED_COM_FILE
#endif
#ifndef INCLUDED_GEO_BANDMAP
#include "geo_bandmap.h"
#define INCLUDED_GEO_BANDMAP
#endif
#ifndef INCLUDED_GEO_RASTERSPACE
#include "geo_rasterspace.h"
#define INCLUDED_GEO_RASTERSPACE
#endif
// Module headers.
#ifndef INCLUDED_APPARGS
#include "appargs.h" // APP_IO_STRATEGY
#define INCLUDED_APPARGS
#endif

#ifndef INCLUDED_PCRCALC
#include "pcrcalc.h"
#define INCLUDED_PCRCALC
#endif


/*!
  \file
  This file contains the implementation of the IoBandTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC IOBAND MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*calc::IoBandTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<IoBandTest> instance(new IoBandTest());

  suite->add(BOOST_CLASS_TEST_CASE(&IoBandTest::test1, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoBandTest::test2, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoBandTest::test3, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&IoBandTest::testTypeChecking, instance));

  return suite;
}

static void execBand(PcrScript *s)
{
 appIOstrategy=APP_IO_BANDMAP;
 pcr_ScriptExecute(s);
}


//------------------------------------------------------------------------------
// DEFINITION OF IOBAND MEMBERS
//------------------------------------------------------------------------------

//! ctor
calc::IoBandTest::IoBandTest()
{
}

#define NR_CELLS 20

//! setUp
void calc::IoBandTest::setUp()
{
 appIOstrategy=APP_IO_BANDMAP;
 { // create a  4*5 UINT1 map as clone
   com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\nYDIM 0.5",
   com::PathName("clone.hdr"));
   UINT1 buf[NR_CELLS];
   std::generate_n(buf,NR_CELLS,com::SeqInc<UINT1>());
   com::write(buf,NR_CELLS,com::PathName("clone.bil"));
 }
 { // create a  4*5 REAL4 map as data
   com::write("NROWS 4\nNCOLS 5\nXDIM 0.5\nYDIM 0.5\nNBITS 32",
   com::PathName("v1_32b.hdr"));
   REAL4 buf[NR_CELLS];
   std::generate_n(buf,NR_CELLS,com::SeqInc<REAL4>());
   com::write(buf,NR_CELLS,com::PathName("v1_32b.bil"));
 }
}

//! tearDown
void calc::IoBandTest::tearDown()
{
}



void calc::IoBandTest::test1()
{
  { // execute correctly
    com::write("dummy = boolean(clone); report piet = uniqueid(1);","script.mod");
    PcrScript *s=pcr_createScript("script.mod");
    BOOST_CHECK(s);
    appIOstrategy=APP_IO_BANDMAP;
    execBand(s);
    if(pcr_ScriptError(s)) {
      std::cerr << "HECK " << pcr_ScriptErrorMessage(s) << "\n";
    }
    BOOST_CHECK(!pcr_ScriptError(s));
    pcr_destroyScript(s);
    // what should not be created
    BOOST_CHECK(!com::pathExists("dummy"));
    BOOST_CHECK(!com::pathExists("dummy.bil"));
    BOOST_CHECK(!com::pathExists("dummy.hdr"));
    BOOST_CHECK(!com::pathExists("piet"));

    // what must be created
    geo::BandMap rm("piet");
    BOOST_CHECK(rm.rasterSpace() == geo::BandMap("clone").rasterSpace());
    BOOST_CHECK(rm.cellRepr() == CR_REAL4);
    REAL4 buf[NR_CELLS];
    rm.getCellsAsREAL4(buf);
    BOOST_CHECK(buf[0]==1);
    BOOST_CHECK(buf[19]==20);
  }
}

void calc::IoBandTest::test2()
{ // dunno output type
    com::write("dummy = boolean(clone); report piet = 1;","script.mod");
    PcrScript *s=pcr_createScript("script.mod");
    BOOST_CHECK(s);
    execBand(s);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK(pcr_ScriptErrorMessage(s).find("conversion") != std::string::npos);
    pcr_destroyScript(s);
}

void calc::IoBandTest::test3()
{
  { // execute correctly, reading UINT1 as REAL4
    com::write("report piet = clone*1+0.5;","script.mod");
    PcrScript *s=pcr_createScript("script.mod");
    BOOST_CHECK(s);
    execBand(s);
    BOOST_CHECK(pcr_ScriptError(s));
    const char *msg="left operand of operator '*': type is one of (nominal,ordinal,boolean), legal type is scalar";
    BOOST_CHECK(pcr_ScriptErrorMessage(s).find(msg) != std::string::npos);
    pcr_destroyScript(s);

    /* NOT CREATED
    // what must be created
    geo::BandMap rm("piet");
    BOOST_CHECK(rm.rasterSpace() == geo::BandMap("clone").rasterSpace());
    BOOST_CHECK(rm.cellRepr() == CR_REAL4);
    REAL4 buf[NR_CELLS];
    rm.getCellsAsREAL4(buf);
    BOOST_CHECK(buf[0]==0.5);
    BOOST_CHECK(buf[19]==19.5);
    */
  }
}

void calc::IoBandTest::testTypeChecking()
{
  { // execute correctly, reading UINT1 as REAL4
    com::write("report piet = areatotal(spatial(3),v1_32b)","script.mod");
    PcrScript *s=pcr_createScript("script.mod");
    BOOST_CHECK(s);
    execBand(s);
    BOOST_CHECK(pcr_ScriptError(s));
    BOOST_CHECK(pcr_ScriptErrorMessage(s).find("legal type") != std::string::npos);
    pcr_destroyScript(s);
  }
}

  // DO remove extensions?
  //  in stuff like piet.map = dadasdsadasdas, in bandmap mode
  // Whoops in current interface not possible to set clone
