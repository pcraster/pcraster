#define BOOST_TEST_MODULE pcraster geo csf_stack_name
#include <boost/test/unit_test.hpp>
#include "com_pathname.h"
#include "com_exception.h"
#include "geo_csfstackname.h"


BOOST_AUTO_TEST_CASE(base_name)
{
  using namespace geo;

  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.baseName() == "soil.csf");

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soil0000");

  pn = "soilsoil.010+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soilsoil");

  pn = "soilsoil.s10+100";
  sn = pn;
  BOOST_CHECK(sn.baseName() == "soilsoil.s");

  {
    // Test functionality that based on the last timestep the digits for the
    // first time step are determined. In this case, 9700 cannot be part of the
    // timestep and thus is part of the basename.
    pn = "lisw9700.001+999";
    sn = pn;
    BOOST_CHECK(sn.baseName() == "lisw9700");
  }
}


BOOST_AUTO_TEST_CASE(filename)
{
  using namespace geo;

 {
  // Static stack in current dir.
  com::PathName pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.fileName(50) == "soil.csf");
 } 
 // pcrcalc has a trick that if the extension
 // is given each timestep is written to the
 // same file
 {
  // CW wanted to use this one  in pcrcalc
  com::PathName pn = "tmp.res";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(!sn.isDynamic());
  BOOST_CHECK(sn.fileName(1) == "tmp.res");
  BOOST_CHECK(sn.fileName(300) == "tmp.res");
 }
 {
  // and not this one in pcrcalc
  com::PathName pn = "tmp.res";
  geo::CSFStackName sn(pn,1,1000);
  BOOST_CHECK(sn.isDynamic());
  BOOST_CHECK(sn.fileName(1) == "tmp.res");
  BOOST_CHECK(sn.fileName(300) == "tmp.res");
 }
 {
  // Dynamic stack in current dir.
  com::PathName pn = "soil0000.010+100";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.fileName(50) == "soil0000.050");
 }

  {
    com::PathName pn = "soilsoil.010+100";
    geo::CSFStackName sn(pn);
    BOOST_CHECK(sn.fileName(50) == "soilsoil.050");
  }

  {
    com::PathName pn = "soilsoil.s10+100";
    geo::CSFStackName sn(pn);
    BOOST_CHECK(sn.fileName(50) == "soilsoil.s50");
  }
}


BOOST_AUTO_TEST_CASE(nr_layers)
{
  using namespace geo;

  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(sn.nrLayers() == 1);

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.nrLayers() == 0); // Stack doesn't exist in current dir.
}


BOOST_AUTO_TEST_CASE(constructor)
{
  using namespace geo;

  com::PathName pn;

  // Static stack in current dir.
  pn = "soil.csf";
  geo::CSFStackName sn(pn);
  BOOST_CHECK(!sn.isDynamic());

  // Dynamic stack in current dir.
  pn = "soil0000.010+100";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

  // Dynamic stack in current dir.
  pn = "XXXeight.001+20000";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

  // baseName ends with numeric digits
  pn = "XXXX970.001+30";
  sn = pn;
  BOOST_CHECK(sn.scanned());
  BOOST_CHECK(sn.isDynamic());

}


BOOST_AUTO_TEST_CASE(bad_formats)
{
  using namespace geo;

  bool catchWrongFormat;
  
  // stupid error
  catchWrongFormat=false;
  try {
       com::PathName pn("+");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  // no last timestep
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpnts00.001+");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);
  
  // last timestep is not numeric
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpnts00.001+XXX");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  // no first timestep
  catchWrongFormat=false;
  try {
       com::PathName pn("rnpntsxx.xxx+431");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
   catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

  catchWrongFormat=false;
  // first timestep larger than second
  try {
       com::PathName pn("XXXX970.009+3");
       geo::CSFStackName sn(pn);
  } catch (const com::Exception& ) {
    catchWrongFormat=true;
  }
  BOOST_CHECK(catchWrongFormat);

}


BOOST_AUTO_TEST_CASE(as_aguila_argument)
{
  using namespace geo;

  std::string r(CSFStackName::asAguilaArgument("prefix",1,100));
  BOOST_CHECK(r == "prefix00.001+100");
}
