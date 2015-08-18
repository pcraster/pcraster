#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACETEST
#include "fieldapi_interfacetest.h"
#define INCLUDED_FIELDAPI_INTERFACETEST
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
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif
#ifndef INCLUDED_GEO_CELLLOCVISITOR
#include "geo_celllocvisitor.h"
#define INCLUDED_GEO_CELLLOCVISITOR
#endif

// Module headers.
#ifndef INCLUDED_FIELDAPI_INTERFACE
#include "fieldapi_interface.h"
#define INCLUDED_FIELDAPI_INTERFACE
#endif
#ifndef INCLUDED_FIELDAPI_TESTFIELD
#include "fieldapi_testfield.h"
#define INCLUDED_FIELDAPI_TESTFIELD
#endif



/*!
  \file
  This file contains the implementation of the InterfaceTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC INTERFACE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*fieldapi::InterfaceTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<InterfaceTest> instance(new InterfaceTest());

  suite->add(BOOST_CLASS_TEST_CASE(&InterfaceTest::testNonMV, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF INTERFACE MEMBERS
//------------------------------------------------------------------------------

//! ctor
fieldapi::InterfaceTest::InterfaceTest()
{
}


void fieldapi::InterfaceTest::testNonMV()
{
  // 1st row all nonMV
  // 2nd row allways some MV
     UINT1  l1[ ] = { 0, 1, 2 , 3, MV_UINT1, 5 };
     UINT1  l2[ ] = { 0, 1, 2 , 3, MV_UINT1, MV_UINT1 };
     REAL4  l3[ ] = { 0, 1, 2 , 3, 70,       80 };
     pcr::setMV(l3[3]);

     TestField<UINT1,2,3> d1(l1);
     TestField<UINT1,2,3> d2(l2);
     TestField<REAL4,2,3> d3(l3);

     ReadWriteData<UINT1,UINT1> t1(TEST_FIELD_INIT(d1));
     ReadWriteData<UINT1,UINT1> t2(TEST_FIELD_INIT(d2));
     ReadWriteData<REAL8,REAL4> t3(TEST_FIELD_INIT(d3));
     ReadOnlyNonSpatial<INT4>   t4(123,t1.nrRows(),t1.nrCols());

     std::vector<const Common*> all;
     all.push_back(&t1);
     all.push_back(&t2);
     all.push_back(&t3);
     all.push_back(&t4);
     for(geo::CellLocVisitor v(t1.nrRows(),t1.nrCols());
          v.valid(); ++v) {
        if ((*v).row() == 0)
          BOOST_CHECK(nonMV(all,*v));
        else
          BOOST_CHECK(!nonMV(all,*v));
     }
}
