#define BOOST_TEST_MODULE pcraster com key_value_table
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_keyvaluetable.h"
#include "com_intervaltypes.h"

BOOST_AUTO_TEST_CASE(required)
{
  using namespace com;

  KeyValueDouble const kvd("key1");

  KeyValueTable kvt;
  kvt.insertKey(kvd, true);

  bool catched = false;
  try {
    kvt.checkRequired();
  } catch (KeyValueTable::MissingKey & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // Insert
  bool succes = true;
  try {
    kvt.add("key1", "2.2");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    succes = false;
  }
  BOOST_TEST(succes);
  BOOST_TEST(kvd.value(kvt) == 2.2);

  // OK
  succes = true;
  try {
    kvt.checkRequired();
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);
}

BOOST_AUTO_TEST_CASE(double_)
{
  using namespace com;

  KeyValueDouble const kvd("key1");

  KeyValueTable kvt;
  kvt.insertKey(kvd);

  bool catched = false;
  try {
    kvt.add("key1", "notAnumber");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // Ok
  bool succes = true;
  try {
    kvt.add("key1", "2.2");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    succes = false;
  }
  BOOST_TEST(succes);
  BOOST_TEST(kvd.value(kvt) == 2.2);

  com::GreaterThan<> gt4(4);
  KeyValueDouble const kvgt4("keyGt4", &gt4);
  kvt.insertKey(kvgt4);

  // out of range
  catched = false;
  try {
    kvt.add("keyGt4", "3");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // ok
  succes = true;
  try {
    kvt.add("keyGt4", " 45 ");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);
  BOOST_TEST(kvgt4.value(kvt) == 45);
}

BOOST_AUTO_TEST_CASE(integer_)
{
  using namespace com;

  KeyValueInteger const kvi("key1");
  KeyValueInteger const kvNotSet("keyNotSet");

  KeyValueTable kvt;
  kvt.insertKey(kvi);
  kvt.insertKey(kvNotSet);

  int keep = 999;
  kvNotSet.setConditional(keep, kvt);
  BOOST_TEST(keep == 999);

  bool catched = false;
  try {
    kvt.add("key1", "notAnumber");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  catched = false;
  try {
    kvt.add("key1", "");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // real
  catched = false;
  try {
    kvt.add("key1", "3.34");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);


  bool succes = true;
  try {
    kvt.add("key1", " 45 ");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);
  BOOST_TEST(kvi.value(kvt) == 45);

  int i45 = 999;
  kvi.setConditional(i45, kvt);
  BOOST_TEST(i45 == 45);

  KeyValueInteger kvgt4("keyGt4");
  kvgt4.setInterval(com::GreaterThan<>(4));
  kvt.insertKey(kvgt4);

  // out of range
  catched = false;
  try {
    kvt.add("keyGt4", "3");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // ok
  succes = true;
  try {
    kvt.add("keyGt4", " 45 ");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);
  BOOST_TEST(kvgt4.value(kvt) == 45);
}

BOOST_AUTO_TEST_CASE(enum_)
{
  using namespace com;

  KeyValueEnum kve1("key1");
  kve1.insert("e1");
  kve1.insert("e2");

  KeyValueTable kvt;
  kvt.insertKey(kve1);

  bool catched = false;
  try {
    kvt.add("key1", "e3");
  } catch (KeyValueTable::IllegalValue & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  bool succes = true;
  try {
    kvt.add("key1", "E1");
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);

  BOOST_TEST(kve1.value(kvt) == "E1");
  BOOST_TEST(kve1.configValue(kvt) == "e1");
}

BOOST_AUTO_TEST_CASE(add)
{
  using namespace com;

  KeyValueString const kvs1("key1");
  KeyValueString const kvs2("key2");
  KeyValueTable kvt;

  kvt.insertKey(kvs1);
  kvt.insertKey(kvs2);

  bool catched = false;
  try {
    kvt.add("unknown", "");
  } catch (KeyValueTable::UnknownKey & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  BOOST_TEST(!kvt.isSet(kvs1));
  kvt.add("key1", "value1");
  BOOST_TEST(kvt.isSet(kvs1));
  BOOST_TEST(kvs1.value(kvt) == "value1");

  // case insensitive
  BOOST_TEST(!kvt.isSet(kvs2));
  kvt.add("KEY2", "value2");
  BOOST_TEST(kvt.isSet(kvs2));
  BOOST_TEST(kvs2.value(kvt) == "value2");

  // catch double
  catched = false;
  try {
    kvt.add("key1", "again");
  } catch (KeyValueTable::DuplicateKey & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // catch double other case
  catched = false;
  try {
    kvt.add("KEY1", "again");
  } catch (KeyValueTable::DuplicateKey & /*v*/) {
    catched = true;
  }
  BOOST_TEST(catched);

  // test discard
  kvt.setDiscardUnknownKeys(true);

  bool ignored = true;
  try {
    kvt.add("unknown", "");
  } catch (...) {
    ignored = false;
  }
  BOOST_TEST(ignored);

  bool succes = true;
  try {
    kvt.checkRequired();
  } catch (...) {
    succes = false;
  }
  BOOST_TEST(succes);
}
