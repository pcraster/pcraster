#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <memory>
#include "com_intervaltypes.h"


namespace com {

  typedef Interval<double> IntervalD;
  static IntervalD *create(const char *msg)
  {
    return com::createIntervalFromLookupTableKey<double>(std::string(msg));
  }

  typedef std::auto_ptr< com::Interval<double> > IVap;
}


BOOST_AUTO_TEST_CASE(from_lookup_table_key_correct_format)
{
  using namespace com;

 {
  IVap a(create("[ ,]"));
  BOOST_CHECK(a->min() == a->minLimit());
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == a->maxLimit());
  BOOST_CHECK(a->valid(a->max()));
 }
 {
  IVap a(create("6")); // EqualTo a(6);
  BOOST_CHECK(a->min() == 6);
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == 6);
  BOOST_CHECK(a->valid(a->max()));
 }

// THE HALF OPEN LIMITS
 {IVap a(create("[6, >")); // GreaterThanEqualTo a(6);
  BOOST_CHECK(a->min() == 6);
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == a->maxLimit());
  BOOST_CHECK(a->valid(a->max()));
 }
 {IVap a(create("<68, ]")); // GreaterThan a(68);
  BOOST_CHECK(a->min() == 68);
  BOOST_CHECK(!a->valid(a->min()));
  BOOST_CHECK(a->max() == a->maxLimit());
  BOOST_CHECK(a->valid(a->max()));
 }

// THE HALF OPEN LIMITS
 {IVap a(create("<,61]")); // LessThanEqualTo a(61);
  BOOST_CHECK(a->min() == a->minLimit());
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == 61);
  BOOST_CHECK(a->valid(a->max()));
 }
 {IVap a(create("<,6>")); // LessThan a(6);
  BOOST_CHECK(a->min() == a->minLimit());
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == 6);
  BOOST_CHECK(!a->valid(a->max()));
 }

 {
 IVap a(create("[4, 6>"));
  BOOST_CHECK(a->min() == 4);
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == 6);
  BOOST_CHECK(!a->valid(a->max()));
 }
 {
  IVap a(create("<4, 6>"));
  BOOST_CHECK(a->min() == 4);
  BOOST_CHECK(!a->valid(a->min()));
  BOOST_CHECK(a->max() == 6);
  BOOST_CHECK(!a->valid(a->max()));
 }
 {
 IVap a(create("[4, 6]"));
  BOOST_CHECK(a->min() == 4);
  BOOST_CHECK(a->valid(a->min()));
  BOOST_CHECK(a->max() == 6);
  BOOST_CHECK(a->valid(a->max()));
 }
 {
   try {
   // test tabs as whitespace
   // test trailing whitespace (failure introduce from boost 1.33 - 1.34)
   // IVap a(create(" <4, 6	    ]  "));
   IVap a(create(" <4, 6	    ] "));
    BOOST_CHECK(a->min() == 4);
    BOOST_CHECK(!a->valid(a->min()));
    BOOST_CHECK(a->max() == 6);
    BOOST_CHECK(a->valid(a->max()));
   } catch (const com::BadIntervalFormat& /*e*/) {
     // bug67
     bool fixSpiritParserBug67=false;
     BOOST_CHECK(fixSpiritParserBug67);
   }
 }
 {
 IVap a(create("<0,0.2]"));
  BOOST_CHECK(a->min() == 0);
  BOOST_CHECK(!a->valid(a->min()));
  BOOST_CHECK(a->max() == 0.2);
  BOOST_CHECK(a->valid(a->max()));
  BOOST_CHECK(a->valid(0.2));
 }
}


BOOST_AUTO_TEST_CASE(from_lookup_table_key_wrong_format)
{
  using namespace com;

  const char *fmts[] = {
    "[ , ",
    "a6",
    "[6, >d",
    "d<68, ]",
    "=,61]",
    ",,>",
    "<03, 34,>",
    "[ 0 ] ",
    "<2, 1>", // <- low is larger than high
    "<1, 1>", // <- low is larger than high
    "[1, 1>", // <- low is larger than high
    "<1, 1]", // <- low is larger than high
  };
  for (size_t i=0; i< ARRAY_SIZE(fmts); i++) {
    bool catched=false;
    try {
       IVap a(create(fmts[i]));
    } catch (const com::BadIntervalFormat& /*e*/) {
       catched=true;
    }
    // if (!catched)
    //  std::cerr << "expect wrong fmt|" << fmts[i] << "|\n";
    BOOST_CHECK(catched);
  }
}

BOOST_AUTO_TEST_CASE(test_equal_to)
{
  using namespace com;

 {
  IVap a(create("2"));
  BOOST_CHECK(a->equalTo());
 }
 {
  IVap a(create("[2,2]"));
  BOOST_CHECK( a->equalTo());
 }
 {
  IVap a(create("[2,>"));
  BOOST_CHECK(!a->equalTo());
 }
 {
  IVap a(create("[ , >"));
  BOOST_CHECK(!a->equalTo());
 }
}


BOOST_AUTO_TEST_CASE(test_min_max)
{
  using namespace com;

  {// docs of virtual double  min()const=0;
   GreaterThanEqualTo<> ge(0);
   BOOST_CHECK( ge.valid(ge.min()));
   GreaterThan<> gt(0);
   BOOST_CHECK(!gt.valid(gt.min()));
  }
 {
  AnythingInterval<> a;
  BOOST_CHECK(a.min() == a.minLimit());
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == a.maxLimit());
  BOOST_CHECK(a.valid(a.max()));
 }
 {
  EqualTo<> a(6);
  BOOST_CHECK(a.min() == 6);
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(a.valid(a.max()));
 }

// THE HALF OPEN LIMITS
 {GreaterThanEqualTo<> a(6);
  BOOST_CHECK(a.min() == 6);
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == a.maxLimit());
  BOOST_CHECK(a.valid(a.max()));
 }
 {GreaterThan<> a(6);
  BOOST_CHECK(a.min() == 6);
  BOOST_CHECK(!a.valid(a.min()));
  BOOST_CHECK(a.max() == a.maxLimit());
  BOOST_CHECK(a.valid(a.max()));
 }

// THE HALF OPEN LIMITS
 {LessThanEqualTo<> a(6);
  BOOST_CHECK(a.min() == a.minLimit());
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(a.valid(a.max()));
 }
 {LessThan<> a(6);
  BOOST_CHECK(a.min() == a.minLimit());
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(!a.valid(a.max()));
 }

 {
  BetweenLimits<> a(GreaterThanEqualTo<>(4),LessThan<>(6)); // [4, 6>
  BOOST_CHECK(a.min() == 4);
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(!a.valid(a.max()));
 }
 {
  BetweenLimits<> a(GreaterThan<>(4),LessThan<>(6)); // <4, 6>
  BOOST_CHECK(a.min() == 4);
  BOOST_CHECK(!a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(!a.valid(a.max()));
 }
 {
  BetweenLimits<> a(GreaterThanEqualTo<>(4),LessThanEqualTo<>(6)); // [4, 6]
  BOOST_CHECK(a.min() == 4);
  BOOST_CHECK(a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(a.valid(a.max()));
 }
 {
  BetweenLimits<> a(GreaterThan<>(4),LessThanEqualTo<>(6)); // <4, 6]
  BOOST_CHECK(a.min() == 4);
  BOOST_CHECK(!a.valid(a.min()));
  BOOST_CHECK(a.max() == 6);
  BOOST_CHECK(a.valid(a.max()));
 }
}


BOOST_AUTO_TEST_CASE(between_limits)
{
  using namespace com;

  BetweenLimits<> b(GreaterThanEqualTo<>(0),LessThan<>(4));
  BOOST_CHECK(!b.valid(-1));
  BOOST_CHECK( b.valid(0));
  BOOST_CHECK( b.valid(2));
  BOOST_CHECK(!b.valid(4));
  BOOST_CHECK(!b.valid(5));

  // was bug, now works
  BetweenLimits<> copy(b);
  BOOST_CHECK(copy.valid(2));

  BetweenLimits<> assignTo(GreaterThanEqualTo<>(8),LessThan<>(12));
  BOOST_CHECK(!assignTo.valid(2));
  assignTo = b;
  BOOST_CHECK( assignTo.valid(2));
}


BOOST_AUTO_TEST_CASE(less_double_operator)
{
  using namespace com;

// Anything

  BOOST_CHECK(! (AnythingInterval<>()   < 2));
  // DOES NOT COMPILE?
  // BOOST_CHECK(! (AnythingInterval<>() > 2));
  BOOST_CHECK(! AnythingInterval<>().operator>(2));
  AnythingInterval<> a;
  BOOST_CHECK(! (a                     > 2));

// EqualTo

  BOOST_CHECK(! (EqualTo<>(0) < -2));   //   0  < -2
  BOOST_CHECK(! (EqualTo<>(0) <  0));   //   0  <  0
  BOOST_CHECK(  (EqualTo<>(0) <  1));   //   0  <  1
  BOOST_CHECK(  (EqualTo<>(0) > -2));   //   0  > -2
  BOOST_CHECK(! (EqualTo<>(0) >  0));   //   0  >  0
  BOOST_CHECK(! (EqualTo<>(0) >  1));   //   0  >  1


// THE HALF OPEN LIMITS

  BOOST_CHECK(! (GreaterThanEqualTo<>(0) < -2));   //   [0->  < -2
  BOOST_CHECK(! (GreaterThanEqualTo<>(0) <  0));   //   [0->  <  0
  BOOST_CHECK(! (GreaterThanEqualTo<>(0) <  1));   //   [0->  <  1
  BOOST_CHECK(  (GreaterThanEqualTo<>(0) > -2));   //   [0->  > -2
  BOOST_CHECK(! (GreaterThanEqualTo<>(0) >  0));   //   [0->  >  0
  BOOST_CHECK(! (GreaterThanEqualTo<>(0) >  1));   //   [0->  >  1

  BOOST_CHECK(! (GreaterThan<>     (0) < -2));   //   <0->  < -2
  BOOST_CHECK(! (GreaterThan<>     (0) <  0));   //   <0->  <  0
  BOOST_CHECK(! (GreaterThan<>     (0) <  1));   //   <0->  <  1
  BOOST_CHECK(  (GreaterThan<>     (0) > -2));   //   <0->  > -2
  BOOST_CHECK(  (GreaterThan<>     (0) >  0));   //   <0->  >  0
  BOOST_CHECK(! (GreaterThan<>     (0) >  1));   //   <0->  >  1

  BOOST_CHECK(! (LessThanEqualTo<>(0) < -2));   //   <-0]  < -2
  BOOST_CHECK(! (LessThanEqualTo<>(0) <  0));   //   <-0]  <  0
  BOOST_CHECK(  (LessThanEqualTo<>(0) <  1));   //   <-0]  <  1
  BOOST_CHECK(! (LessThanEqualTo<>(0) > -2));   //   <-0]  > -2
  BOOST_CHECK(! (LessThanEqualTo<>(0) >  0));   //   <-0]  >  0
  BOOST_CHECK(! (LessThanEqualTo<>(0) >  1));   //   <-0]  >  1

  BOOST_CHECK(! (LessThan<>     (0) < -2));   //   <-0>  < -2
  BOOST_CHECK(  (LessThan<>     (0) <  0));   //   <-0>  <  0
  BOOST_CHECK(  (LessThan<>     (0) <  1));   //   <-0>  <  1
  BOOST_CHECK(! (LessThan<>     (0) > -2));   //   <-0>  > -2
  BOOST_CHECK(! (LessThan<>     (0) >  0));   //   <-0>  >  0
  BOOST_CHECK(! (LessThan<>     (0) >  1));   //   <-0>  >  1

  {
  BetweenLimits<> b(GreaterThanEqualTo<>(0),LessThan<>(4));
  BOOST_CHECK(! (b < -2));   //   [0,4> < -2
  BOOST_CHECK(! (b <  0));   //   [0,4> <  0
  BOOST_CHECK(! (b <  1));   //   [0,4> <  1
  BOOST_CHECK(  (b <  4));   //   [0,4> <  4
  BOOST_CHECK(  (b <  6));   //   [0,4> <  6
  BOOST_CHECK(  (b > -2));   //   [0,4> > -2
  BOOST_CHECK(! (b >  0));   //   [0,4> >  0
  BOOST_CHECK(! (b >  1));   //   [0,4> >  1
  BOOST_CHECK(! (b >  4));   //   [0,4> >  4
  BOOST_CHECK(! (b >  6));   //   [0,4> >  6
  }

  {
  BetweenLimits<> b(GreaterThan<>(0),LessThan<>(4));
  BOOST_CHECK(! (b < -2));   //    <0,4> < -2
  BOOST_CHECK(! (b <  0));   //    <0,4> <  0
  BOOST_CHECK(! (b <  1));   //    <0,4> <  1
  BOOST_CHECK(  (b <  4));   //    <0,4> <  4
  BOOST_CHECK(  (b <  6));   //    <0,4> <  6
  BOOST_CHECK(  (b > -2));   //    <0,4> > -2
  BOOST_CHECK(  (b >  0));   //    <0,4> >  0
  BOOST_CHECK(! (b >  1));   //    <0,4> >  1
  BOOST_CHECK(! (b >  4));   //    <0,4> >  4
  BOOST_CHECK(! (b >  6));   //    <0,4> >  6
  }

  {
  BetweenLimits<> b(GreaterThanEqualTo<>(0),LessThanEqualTo<>(4));
  BOOST_CHECK(! (b < -2));   //   [0,4] < -2
  BOOST_CHECK(! (b <  0));   //   [0,4] <  0
  BOOST_CHECK(! (b <  1));   //   [0,4] <  1
  BOOST_CHECK(! (b <  4));   //   [0,4] <  4
  BOOST_CHECK(  (b <  6));   //   [0,4] <  6
  BOOST_CHECK(  (b > -2));   //   [0,4] > -2
  BOOST_CHECK(! (b >  0));   //   [0,4] >  0
  BOOST_CHECK(! (b >  1));   //   [0,4] >  1
  BOOST_CHECK(! (b >  4));   //   [0,4] >  4
  BOOST_CHECK(! (b >  6));   //   [0,4] >  6
  }

  {
  BetweenLimits<> b(GreaterThan<>(0),LessThanEqualTo<>(4));
  BOOST_CHECK(! (b < -2));   //    <0,4] < -2
  BOOST_CHECK(! (b <  0));   //    <0,4] <  0
  BOOST_CHECK(! (b <  1));   //    <0,4] <  1
  BOOST_CHECK(! (b <  4));   //    <0,4] <  4
  BOOST_CHECK(  (b <  6));   //    <0,4] <  6
  BOOST_CHECK(  (b > -2));   //    <0,4] > -2
  BOOST_CHECK(  (b >  0));   //    <0,4] >  0
  BOOST_CHECK(! (b >  1));   //    <0,4] >  1
  BOOST_CHECK(! (b >  4));   //    <0,4] >  4
  BOOST_CHECK(! (b >  6));   //    <0,4] >  6
  }

}


BOOST_AUTO_TEST_CASE(less_operator)
{
  using namespace com;

  BOOST_CHECK(!(GreaterThan<>(0).less(LessThanEqualTo<>(4))));

  // < 0   vs.  >= 4
  BOOST_CHECK( (LessThan<>(0).less(GreaterThanEqualTo<>(4))));

  // <= 4   vs.  >= 4
  BOOST_CHECK(!(LessThanEqualTo<>(4).less(GreaterThanEqualTo<>(4))));
  BOOST_CHECK( (LessThanEqualTo<>(4).less(GreaterThan<>(4))));
  BOOST_CHECK( (LessThan<>(4).less(GreaterThanEqualTo<>(4))));

  BOOST_CHECK(!(LessThanEqualTo<>(4).less(AnythingInterval<>())));
  BOOST_CHECK(!(AnythingInterval<>().less(AnythingInterval<>())));

  BOOST_CHECK( (EqualTo<>(4).less(GreaterThan<>(4))));
  BOOST_CHECK(!(EqualTo<>(4).less(GreaterThanEqualTo<>(4))));

  BOOST_CHECK( (EqualTo<>(3).less(GreaterThan<>(4))));
  BOOST_CHECK( (EqualTo<>(3).less(GreaterThanEqualTo<>(4))));

  BOOST_CHECK(!GreaterThan<>(4).less(GreaterThan<>(5)));
}


BOOST_AUTO_TEST_CASE(eq_operator)
{
  using namespace com;

  BOOST_CHECK((GreaterThan<>(0)!= (LessThanEqualTo<>(4))));
  BOOST_CHECK(GreaterThan<>(0) == GreaterThan<>(0));
  BOOST_CHECK(LessThanEqualTo<>(4) != AnythingInterval<>());
  BOOST_CHECK(AnythingInterval<>() == AnythingInterval<>());
  BOOST_CHECK(EqualTo<>(4)         == EqualTo<>(4));
  BOOST_CHECK(EqualTo<>(4)         != EqualTo<>(10));
}


BOOST_AUTO_TEST_CASE(round_error)
{
  using namespace com;

  double v(0.2);
  float  vf(0.2F);
  // assumptions:
  // ! v in < 0.2, 0.4]
  //   v in < 0  , 0.2]
  BOOST_CHECK(!
    BetweenLimits<>(GreaterThan<>(0.2),LessThanEqualTo<>(0.4)).valid(v));
  BOOST_CHECK(
    BetweenLimits<>(GreaterThan<>(0),LessThanEqualTo<>(0.2)).valid(v));
  {
  typedef float CT; // choose template
  BOOST_CHECK(!
    BetweenLimits<CT>(GreaterThan<CT>(0.2F),LessThanEqualTo<CT>(0.4F)).valid(vf));
  BOOST_CHECK(
    BetweenLimits<CT>(GreaterThan<CT>(0.0F),LessThanEqualTo<CT>(0.2F)).valid(vf));
  }
  {
    // ROUNDING ERROR comparing float(0.2) ?= double(0.2)
  typedef double CT; // choose template
  BOOST_CHECK(   // <-- this is not what we expect
    BetweenLimits<CT>(GreaterThan<CT>(0.2),LessThanEqualTo<CT>(0.4)).valid(vf));
  BOOST_CHECK(! // <-- this is not what we expect
    BetweenLimits<CT>(GreaterThan<CT>(0.0),LessThanEqualTo<CT>(0.2)).valid(vf));
  }
  { // OK float instantation
  typedef double CT; // choose template
  BOOST_CHECK(!
    BetweenLimits<CT>(GreaterThan<CT>(0.2f),LessThanEqualTo<CT>(0.4f)).valid(vf));
  BOOST_CHECK(
    BetweenLimits<CT>(GreaterThan<CT>(0.0f),LessThanEqualTo<CT>(0.2f)).valid(vf));
  }
  {
    // the solution, ctor argument must be a float if the valid argument is a float
  typedef double CT; // choose template
  BOOST_CHECK(!
    BetweenLimits<CT>(GreaterThan<CT>(vf),LessThanEqualTo<CT>(0.4)).valid(vf));
  BOOST_CHECK(
    BetweenLimits<CT>(GreaterThan<CT>(0),LessThanEqualTo<CT>(vf)).valid(vf));
  }
}


BOOST_AUTO_TEST_CASE(limit)
{
  using namespace com;

  {
   typedef Interval<double> D; // choose template
   BOOST_CHECK(D::maxLimit()+D::minLimit()==0);
  }
  {
   typedef Interval<float> F; // choose template
   BOOST_CHECK(F::maxLimit()+F::minLimit()==0);
  }
}
