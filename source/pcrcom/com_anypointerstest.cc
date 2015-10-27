#define BOOST_TEST_MODULE pcraster com algorithm
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include "com_anypointers.h"


struct Base {
};
struct A : public Base {
};
struct B : public Base {
};


BOOST_AUTO_TEST_CASE(test_)
{
  using namespace com;

  AnyPointers pointers;

  BOOST_CHECK(pointers.empty());
  BOOST_CHECK(pointers.size() == 0);

  typedef std::vector<double> Doubles;

  Doubles doubles;
  BOOST_CHECK(pointers.size<Doubles>() == 0);
  size_t idDoubles = pointers.insert(&doubles);
  BOOST_CHECK(!pointers.empty());
  BOOST_CHECK(pointers.size() == 1);
  BOOST_CHECK(pointers.size<Doubles>() == 1);

  Doubles* doublesPointer = 0;

  try {
    doublesPointer = pointers.pointer<Doubles>(idDoubles);
  }
  catch(boost::bad_any_cast& ) {
    bool bad_any_cast = false;
    BOOST_CHECK(bad_any_cast);
  }

  BOOST_CHECK(doublesPointer);
  BOOST_CHECK(doublesPointer->empty());

  // Change doubles collections through pointer.
  doublesPointer->push_back(5.5);

  // Make sure the doubles collection in pointers is changed too.
  try {
    doublesPointer = pointers.pointer<Doubles>(idDoubles);
  }
  catch(boost::bad_any_cast& ) {
    bool bad_any_cast = false;
    BOOST_CHECK(bad_any_cast);
  }

  BOOST_CHECK(doublesPointer);
  BOOST_CHECK(!doublesPointer->empty());
  BOOST_CHECK((*doublesPointer)[0] = 5.5);

  typedef std::vector<int> Integers;
  Integers integers;
  BOOST_CHECK(pointers.size<Integers>() == 0);
  size_t idIntegers = pointers.insert(&integers);
  BOOST_CHECK(pointers.size<Integers>() == 1);

  BOOST_CHECK(idDoubles != idIntegers);

  pointers.erase<Integers>(idIntegers);
  BOOST_CHECK(pointers.size<Integers>() == 0);

  // Check that the id of the previously stored version is returned.
  size_t idIntegers2 = pointers.insert(&integers);
  BOOST_CHECK(idIntegers2 = idIntegers);

  // Add a data set with an id of our choice and make sure it succeeded.
  {
    Integers integers2;
    idIntegers2 = 5;
    pointers.insert(&integers2, 5);

    Integers* integersPointer = 0;

    try {
      integersPointer = pointers.pointer<Integers>(idIntegers2);
    }
    catch(boost::bad_any_cast& ) {
      bool bad_any_cast = false;
      BOOST_CHECK(bad_any_cast);
    }

    BOOST_CHECK(integersPointer);
    BOOST_CHECK(integersPointer == &integers2);
  }

  {
    A a;
    B b;
    AnyPointers pointers;
    pointers.insert<A>(&a);
    pointers.insert<B>(&b);

    BOOST_CHECK(pointers.size<A>() == 1);
    BOOST_CHECK(pointers.size<B>() == 1);
    BOOST_CHECK(pointers.size<Base>() == 0);
    BOOST_CHECK(pointers.size() == 2);
  }

  {
    A a;
    B b;
    AnyPointers pointers;
    pointers.insert(static_cast<Base*>(&a));
    pointers.insert(static_cast<Base*>(&b));

    BOOST_CHECK(pointers.size<A>() == 0);
    BOOST_CHECK(pointers.size<B>() == 0);
    BOOST_CHECK(pointers.size<Base>() == 2);
  }

  {
    AnyPointers pointers;
    A a;
    size_t id = pointers.insert(&a);
    BOOST_CHECK(pointers.size<A>() == 1);

    try {
      /* A const* pointer = */ ((AnyPointers const&)pointers).pointer<A>(id);
    }
    catch(boost::bad_any_cast&) {
      bool bad_any_cast = false;
      BOOST_CHECK(bad_any_cast);
    }

  }
}
