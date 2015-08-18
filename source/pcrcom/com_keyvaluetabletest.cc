#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_KEYVALUETABLETEST
#include "com_keyvaluetabletest.h"
#define INCLUDED_COM_KEYVALUETABLETEST
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
#ifndef INCLUDED_COM_KEYVALUETABLE
#include "com_keyvaluetable.h"
#define INCLUDED_COM_KEYVALUETABLE
#endif
#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the KeyValueTableTest class.
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC KEYVALUETABLE MEMBERS
//------------------------------------------------------------------------------

//! suite
boost::unit_test::test_suite*com::KeyValueTableTest::suite()
{
  boost::unit_test::test_suite* suite = BOOST_TEST_SUITE(__FILE__);
  boost::shared_ptr<KeyValueTableTest> instance(new KeyValueTableTest());

  suite->add(BOOST_CLASS_TEST_CASE(&KeyValueTableTest::testAdd, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&KeyValueTableTest::testEnum, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&KeyValueTableTest::testInteger, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&KeyValueTableTest::testDouble, instance));
  suite->add(BOOST_CLASS_TEST_CASE(&KeyValueTableTest::testRequired, instance));

  return suite;
}



//------------------------------------------------------------------------------
// DEFINITION OF KEYVALUETABLE MEMBERS
//------------------------------------------------------------------------------

//! ctor
com::KeyValueTableTest::KeyValueTableTest()
{
}

//! setUp
void com::KeyValueTableTest::setUp()
{
}

//! tearDown
void com::KeyValueTableTest::tearDown()
{

}

void com::KeyValueTableTest::testRequired()
{
  KeyValueDouble kvd("key1");

  KeyValueTable kvt;
  kvt.insertKey(kvd,true);

  bool catched=false;
  try {
    kvt.checkRequired();
  } catch (KeyValueTable::MissingKey &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // Insert
  bool succes=true;
  try {
    kvt.add("key1","2.2");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(kvd.value(kvt) == 2.2);

  // OK
  succes=true;
  try {
    kvt.checkRequired();
  } catch (...) {
    succes=false;
  }
  BOOST_CHECK(succes);
}

void com::KeyValueTableTest::testDouble()
{
  KeyValueDouble kvd("key1");

  KeyValueTable kvt;
  kvt.insertKey(kvd);

  bool catched=false;
  try {
    kvt.add("key1","notAnumber");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // Ok
  bool succes=true;
  try {
    kvt.add("key1","2.2");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(kvd.value(kvt) == 2.2);

  com::GreaterThan<>  gt4(4);
  KeyValueDouble kvgt4("keyGt4",&gt4);
  kvt.insertKey(kvgt4);

  // out of range
  catched=false;
  try {
    kvt.add("keyGt4","3");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // ok
  succes=true;
  try {
    kvt.add("keyGt4"," 45 ");
  } catch (...) {
    succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(kvgt4.value(kvt) == 45);
}

void com::KeyValueTableTest::testInteger()
{
  KeyValueInteger kvi("key1");
  KeyValueInteger kvNotSet("keyNotSet");

  KeyValueTable kvt;
  kvt.insertKey(kvi);
  kvt.insertKey(kvNotSet);

  int keep=999;
  kvNotSet.setConditional(keep, kvt);
  BOOST_CHECK(keep == 999);

  bool catched=false;
  try {
    kvt.add("key1","notAnumber");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  catched=false;
  try {
    kvt.add("key1","");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // real
  catched=false;
  try {
    kvt.add("key1","3.34");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);


  bool succes=true;
  try {
    kvt.add("key1"," 45 ");
  } catch (...) {
    succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(kvi.value(kvt) == 45);

  int i45=999;
  kvi.setConditional(i45, kvt);
  BOOST_CHECK(i45==45);

  KeyValueInteger kvgt4("keyGt4");
  kvgt4.setInterval(com::GreaterThan<>(4));
  kvt.insertKey(kvgt4);

  // out of range
  catched=false;
  try {
    kvt.add("keyGt4","3");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // ok
  succes=true;
  try {
    kvt.add("keyGt4"," 45 ");
  } catch (...) {
    succes=false;
  }
  BOOST_CHECK(succes);
  BOOST_CHECK(kvgt4.value(kvt) == 45);
}

void com::KeyValueTableTest::testEnum()
{
  KeyValueEnum kve1("key1");
  kve1.insert("e1");
  kve1.insert("e2");

  KeyValueTable kvt;
  kvt.insertKey(kve1);

  bool catched=false;
  try {
    kvt.add("key1","e3");
  } catch (KeyValueTable::IllegalValue &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  bool succes=true;
  try {
    kvt.add("key1","E1");
  } catch (...) {
    succes=false;
  }
  BOOST_CHECK(succes);

  BOOST_CHECK(kve1.value(kvt) == "E1");
  BOOST_CHECK(kve1.configValue(kvt) == "e1");
}


void com::KeyValueTableTest::testAdd()
{
  KeyValueString kvs1("key1");
  KeyValueString kvs2("key2");
  KeyValueTable kvt;

  kvt.insertKey(kvs1);
  kvt.insertKey(kvs2);

  bool catched=false;
  try {
    kvt.add("unknown","");
  } catch (KeyValueTable::UnknownKey &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  BOOST_CHECK(!kvt.isSet(kvs1));
  kvt.add("key1","value1");
  BOOST_CHECK(kvt.isSet(kvs1));
  BOOST_CHECK(kvs1.value(kvt)=="value1");

  // case insensitive
  BOOST_CHECK(!kvt.isSet(kvs2));
  kvt.add("KEY2","value2");
  BOOST_CHECK(kvt.isSet(kvs2));
  BOOST_CHECK(kvs2.value(kvt)=="value2");

  // catch double
  catched=false;
  try {
    kvt.add("key1","again");
  } catch (KeyValueTable::DuplicateKey &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // catch double other case
  catched=false;
  try {
    kvt.add("KEY1","again");
  } catch (KeyValueTable::DuplicateKey &/*v*/) {
    catched=true;
  }
  BOOST_CHECK(catched);

  // test discard
  kvt.setDiscardUnknownKeys(true);

  bool ignored=true;
  try {
    kvt.add("unknown","");
  } catch (...) {
    ignored=false;
  }
  BOOST_CHECK(ignored);

  bool succes=true;
  try {
   kvt.checkRequired();
  } catch(...) {
   succes=false;
  }
  BOOST_CHECK(succes);
}
