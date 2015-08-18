#include "stddefx.h"



#ifndef INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#include <boost/test/included/unit_test.hpp>
#define INCLUDED_BOOST_TEST_INCLUDED_UNIT_TEST
#endif
/*!
 \namespace fieldapi
 \brief api for fields, in support of calc library

  Used in the implemtation of global operators.
  Currently only in the new stuff such as dynamicwave and inversedistance.
  Someday it must replace the Ansi-C MAP_* interface from api.h

  <ul>
   <li>improve maybe to have 'scalar only types' that
       have REAL4 in and out. In general, if we manage to have pcrcalc/oplist.tcl
       to generate ALL interfaces, we can deduce if we need a polomorphic interface,
       as implemented now, are a straigth single type
    <li>implement a check func that checks for MV without getting the value.
       Can be done on higher level, so we can make generic array of maps to
        check indepent of map type
    <li>in the long run, drop MAP_* interface totally, redesign
        C/Pascal/Com interface on top of new api
  </ul>

  \todo
    CW do we still have float/double promotion problem (spread/view or whatever)?
  \todo
    put of UINT1 had in Ansi-C a unsigned int value type
    check if needed here
*/


#ifndef INCLUDED_FIELDAPI_READONLYNONSPATIALTEST
#include "fieldapi_readonlynonspatialtest.h"
#define INCLUDED_FIELDAPI_READONLYNONSPATIALTEST
#endif

#ifndef INCLUDED_FIELDAPI_READONLYSPATIALTEST
#include "fieldapi_readonlyspatialtest.h"
#define INCLUDED_FIELDAPI_READONLYSPATIALTEST
#endif

#ifndef INCLUDED_FIELDAPI_READWRITEDATATEST
#include "fieldapi_readwritedatatest.h"
#define INCLUDED_FIELDAPI_READWRITEDATATEST
#endif

#ifndef INCLUDED_FIELDAPI_INTERFACETEST
#include "fieldapi_interfacetest.h"
#define INCLUDED_FIELDAPI_INTERFACETEST
#endif


boost::unit_test::test_suite* init_unit_test_suite(int /*argc*/, char ** const /*argv */)
{
  boost::unit_test::test_suite* test = BOOST_TEST_SUITE(__FILE__);

  test->add(fieldapi::ReadOnlyNonSpatialTest().suite());
  test->add(fieldapi::ReadOnlySpatialTest().suite());
  test->add(fieldapi::ReadWriteDataTest().suite());
  test->add(fieldapi::InterfaceTest().suite());

  return test;
}

