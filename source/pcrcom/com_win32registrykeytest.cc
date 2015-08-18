#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef WIN32
#error this file should only be included for WIN32 platforms
#endif

#ifndef INCLUDED_COM_WIN32REGISTRYKEY
#include "com_win32registrykey.h"
#define INCLUDED_COM_WIN32REGISTRYKEY
#endif

#ifndef INCLUDED_COM_WIN32REGISTRYKEYTEST
#include "com_win32registrykeytest.h"
#define INCLUDED_COM_WIN32REGISTRYKEYTEST
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

// Module headers.



/*!
  \file
  This file contains the implementation of the Win32RegistryKeyTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC WIN32REGISTRYKEY MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::Win32RegistryKeyTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<Win32RegistryKeyTest> instance(new Win32RegistryKeyTest());

  suite->add(BOOST_CLASS_TEST_CASE(&Win32RegistryKeyTest::testWIN32setting, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&Win32RegistryKeyTest::testCycle, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF WIN32REGISTRYKEY MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::Win32RegistryKeyTest::Win32RegistryKeyTest()
{
}



//! setUp
void com::Win32RegistryKeyTest::setUp()
{
}

//! tearDown
void com::Win32RegistryKeyTest::tearDown()
{
}

void com::Win32RegistryKeyTest::testWIN32setting()
{
  // make sure WIN32 is set for _MSC_VER
  bool WIN32_defineSet=false;
#ifdef _MSC_VER
#ifdef WIN32
  WIN32_defineSet=true;
#endif
#endif
  BOOST_CHECK(WIN32_defineSet);
}


//! whole roundtrip
void com::Win32RegistryKeyTest::testCycle()
{

  Win32RegistryKey k(Win32RegistryKey::CurrentUser,
                     "Software\\PCRasterDevelopment\\Win32RegistryKeyTest");
  // just in case
  k.remove();

  // the cycle
  BOOST_CHECK(!k.exists());
  BOOST_CHECK(k.value() == "");
  BOOST_CHECK(k.set("test"));
  BOOST_CHECK(k.exists());
  BOOST_CHECK(k.value() == "test");
  BOOST_CHECK(k.remove());

  BOOST_CHECK(k.set("fuck"));
}
