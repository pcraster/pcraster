#define BOOST_TEST_MODULE pcraster com win32_registry_key
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#ifndef WIN32
#error this file should only be included for WIN32 platforms
#endif
#include "com_win32registrykey.h"

BOOST_AUTO_TEST_CASE(win32_setting)
{
  using namespace com;

  // make sure WIN32 is set for _MSC_VER
  bool WIN32_defineSet = false;
#ifdef _MSC_VER
#ifdef WIN32
  WIN32_defineSet = true;
#endif
#endif
  BOOST_TEST(WIN32_defineSet);
}

//! whole roundtrip
BOOST_AUTO_TEST_CASE(cycle)
{
  using namespace com;

  Win32RegistryKey k(Win32RegistryKey::CurrentUser,
                     "Software\\PCRasterDevelopment\\Win32RegistryKeyTest");
  // just in case
  k.remove();

  // the cycle
  BOOST_TEST(!k.exists());
  BOOST_TEST(k.value() == "");
  BOOST_TEST(k.set("test"));
  BOOST_TEST(k.exists());
  BOOST_TEST(k.value() == "test");
  BOOST_TEST(k.remove());

  BOOST_TEST(k.set("fuck"));
}
