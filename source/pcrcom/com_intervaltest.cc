#define BOOST_TEST_MODULE pcraster com clone
#include <boost/test/unit_test.hpp>
#include "stddefx.h"
#include <memory>
#include "com_intervaltypes.h"

namespace com
{

typedef Interval<double> IntervalD;

static IntervalD *create(const char *msg)
{
  return com::createIntervalFromLookupTableKey<double>(std::string(msg));
}

typedef std::unique_ptr<com::Interval<double>> IVap;
}  // namespace com

BOOST_AUTO_TEST_CASE(from_lookup_table_key_correct_format)
{
  using namespace com;

  {
    IVap a(create("[ ,]"));
    BOOST_TEST(a->min() == a->minLimit());
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == a->maxLimit());
    BOOST_TEST(a->valid(a->max()));
  }
  {
    IVap a(create("6"));  // EqualTo a(6);
    BOOST_TEST(a->min() == 6);
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == 6);
    BOOST_TEST(a->valid(a->max()));
  }

  // THE HALF OPEN LIMITS
  {
    IVap a(create("[6, >"));  // GreaterThanEqualTo a(6);
    BOOST_TEST(a->min() == 6);
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == a->maxLimit());
    BOOST_TEST(a->valid(a->max()));
  }
  {
    IVap a(create("<68, ]"));  // GreaterThan a(68);
    BOOST_TEST(a->min() == 68);
    BOOST_TEST(!a->valid(a->min()));
    BOOST_TEST(a->max() == a->maxLimit());
    BOOST_TEST(a->valid(a->max()));
  }

  // THE HALF OPEN LIMITS
  {
    IVap a(create("<,61]"));  // LessThanEqualTo a(61);
    BOOST_TEST(a->min() == a->minLimit());
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == 61);
    BOOST_TEST(a->valid(a->max()));
  }
  {
    IVap a(create("<,6>"));  // LessThan a(6);
    BOOST_TEST(a->min() == a->minLimit());
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == 6);
    BOOST_TEST(!a->valid(a->max()));
  }

  {
    IVap a(create("[4, 6>"));
    BOOST_TEST(a->min() == 4);
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == 6);
    BOOST_TEST(!a->valid(a->max()));
  }
  {
    IVap a(create("<4, 6>"));
    BOOST_TEST(a->min() == 4);
    BOOST_TEST(!a->valid(a->min()));
    BOOST_TEST(a->max() == 6);
    BOOST_TEST(!a->valid(a->max()));
  }
  {
    IVap a(create("[4, 6]"));
    BOOST_TEST(a->min() == 4);
    BOOST_TEST(a->valid(a->min()));
    BOOST_TEST(a->max() == 6);
    BOOST_TEST(a->valid(a->max()));
  }
  {
    try {
      // test tabs as whitespace
      // test trailing whitespace (failure introduce from boost 1.33 - 1.34)
      // IVap a(create(" <4, 6	    ]  "));
      IVap a(create(" <4, 6	    ] "));
      BOOST_TEST(a->min() == 4);
      BOOST_TEST(!a->valid(a->min()));
      BOOST_TEST(a->max() == 6);
      BOOST_TEST(a->valid(a->max()));
    } catch (const com::BadIntervalFormat & /*e*/) {
      // bug67
      bool const fixSpiritParserBug67 = false;
      BOOST_TEST(fixSpiritParserBug67);
    }
  }
  {
    IVap a(create("<0,0.2]"));
    BOOST_TEST(a->min() == 0);
    BOOST_TEST(!a->valid(a->min()));
    BOOST_TEST(a->max() == 0.2);
    BOOST_TEST(a->valid(a->max()));
    BOOST_TEST(a->valid(0.2));
  }
}

BOOST_AUTO_TEST_CASE(from_lookup_table_key_wrong_format)
{
  using namespace com;

  const char *fmts[] = {
      "[ , ",   "a6", "[6, >d", "d<68, ]", "=,61]", ",,>", "<03, 34,>", "[ 0 ] ",
      "<2, 1>",  // <- low is larger than high
      "<1, 1>",  // <- low is larger than high
      "[1, 1>",  // <- low is larger than high
      "<1, 1]",  // <- low is larger than high
  };
  for (auto &fmt : fmts) {
    bool catched = false;
    try {
      IVap const a(create(fmt));
    } catch (const com::BadIntervalFormat & /*e*/) {
      catched = true;
    }
    // if (!catched)
    //  std::cerr << "expect wrong fmt|" << fmts[i] << "|\n";
    BOOST_TEST(catched);
  }
}

BOOST_AUTO_TEST_CASE(test_equal_to)
{
  using namespace com;

  {
    IVap a(create("2"));
    BOOST_TEST(a->equalTo());
  }
  {
    IVap a(create("[2,2]"));
    BOOST_TEST(a->equalTo());
  }
  {
    IVap a(create("[2,>"));
    BOOST_TEST(!a->equalTo());
  }
  {
    IVap a(create("[ , >"));
    BOOST_TEST(!a->equalTo());
  }
}

BOOST_AUTO_TEST_CASE(test_min_max)
{
  using namespace com;

  {  // docs of virtual double  min()const=0;
    GreaterThanEqualTo<> const ge(0);
    BOOST_TEST(ge.valid(ge.min()));
    GreaterThan<> const gt(0);
    BOOST_TEST(!gt.valid(gt.min()));
  }
  {
    AnythingInterval<> const a;
    BOOST_TEST(a.min() == a.minLimit());
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == a.maxLimit());
    BOOST_TEST(a.valid(a.max()));
  }
  {
    EqualTo<> const a(6);
    BOOST_TEST(a.min() == 6);
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(a.valid(a.max()));
  }

  // THE HALF OPEN LIMITS
  {
    GreaterThanEqualTo<> const a(6);
    BOOST_TEST(a.min() == 6);
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == a.maxLimit());
    BOOST_TEST(a.valid(a.max()));
  }
  {
    GreaterThan<> const a(6);
    BOOST_TEST(a.min() == 6);
    BOOST_TEST(!a.valid(a.min()));
    BOOST_TEST(a.max() == a.maxLimit());
    BOOST_TEST(a.valid(a.max()));
  }

  // THE HALF OPEN LIMITS
  {
    LessThanEqualTo<> const a(6);
    BOOST_TEST(a.min() == a.minLimit());
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(a.valid(a.max()));
  }
  {
    LessThan<> const a(6);
    BOOST_TEST(a.min() == a.minLimit());
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(!a.valid(a.max()));
  }

  {
    BetweenLimits<> const a(GreaterThanEqualTo<>(4), LessThan<>(6));  // [4, 6>
    BOOST_TEST(a.min() == 4);
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(!a.valid(a.max()));
  }
  {
    BetweenLimits<> const a(GreaterThan<>(4), LessThan<>(6));  // <4, 6>
    BOOST_TEST(a.min() == 4);
    BOOST_TEST(!a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(!a.valid(a.max()));
  }
  {
    BetweenLimits<> const a(GreaterThanEqualTo<>(4), LessThanEqualTo<>(6));  // [4, 6]
    BOOST_TEST(a.min() == 4);
    BOOST_TEST(a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(a.valid(a.max()));
  }
  {
    BetweenLimits<> const a(GreaterThan<>(4), LessThanEqualTo<>(6));  // <4, 6]
    BOOST_TEST(a.min() == 4);
    BOOST_TEST(!a.valid(a.min()));
    BOOST_TEST(a.max() == 6);
    BOOST_TEST(a.valid(a.max()));
  }
}

BOOST_AUTO_TEST_CASE(between_limits)
{
  using namespace com;

  BetweenLimits<> const b(GreaterThanEqualTo<>(0), LessThan<>(4));
  BOOST_TEST(!b.valid(-1));
  BOOST_TEST(b.valid(0));
  BOOST_TEST(b.valid(2));
  BOOST_TEST(!b.valid(4));
  BOOST_TEST(!b.valid(5));

  // was bug, now works
  const BetweenLimits<> &copy(b);
  BOOST_TEST(copy.valid(2));

  BetweenLimits<> assignTo(GreaterThanEqualTo<>(8), LessThan<>(12));
  BOOST_TEST(!assignTo.valid(2));
  assignTo = b;
  BOOST_TEST(assignTo.valid(2));
}

BOOST_AUTO_TEST_CASE(less_double_operator)
{
  using namespace com;

  // Anything

  BOOST_TEST(!(AnythingInterval<>() < 2));
  // DOES NOT COMPILE?
  // BOOST_TEST(! (AnythingInterval<>() > 2));
  BOOST_TEST(!AnythingInterval<>().operator>(2));
  AnythingInterval<> const a;
  BOOST_TEST(!(a > 2));

  // EqualTo

  BOOST_TEST(!(EqualTo<>(0) < -2));  //   0  < -2
  BOOST_TEST(!(EqualTo<>(0) < 0));   //   0  <  0
  BOOST_TEST((EqualTo<>(0) < 1));    //   0  <  1
  BOOST_TEST((EqualTo<>(0) > -2));   //   0  > -2
  BOOST_TEST(!(EqualTo<>(0) > 0));   //   0  >  0
  BOOST_TEST(!(EqualTo<>(0) > 1));   //   0  >  1


  // THE HALF OPEN LIMITS

  BOOST_TEST(!(GreaterThanEqualTo<>(0) < -2));  //   [0->  < -2
  BOOST_TEST(!(GreaterThanEqualTo<>(0) < 0));   //   [0->  <  0
  BOOST_TEST(!(GreaterThanEqualTo<>(0) < 1));   //   [0->  <  1
  BOOST_TEST((GreaterThanEqualTo<>(0) > -2));   //   [0->  > -2
  BOOST_TEST(!(GreaterThanEqualTo<>(0) > 0));   //   [0->  >  0
  BOOST_TEST(!(GreaterThanEqualTo<>(0) > 1));   //   [0->  >  1

  BOOST_TEST(!(GreaterThan<>(0) < -2));  //   <0->  < -2
  BOOST_TEST(!(GreaterThan<>(0) < 0));   //   <0->  <  0
  BOOST_TEST(!(GreaterThan<>(0) < 1));   //   <0->  <  1
  BOOST_TEST((GreaterThan<>(0) > -2));   //   <0->  > -2
  BOOST_TEST((GreaterThan<>(0) > 0));    //   <0->  >  0
  BOOST_TEST(!(GreaterThan<>(0) > 1));   //   <0->  >  1

  BOOST_TEST(!(LessThanEqualTo<>(0) < -2));  //   <-0]  < -2
  BOOST_TEST(!(LessThanEqualTo<>(0) < 0));   //   <-0]  <  0
  BOOST_TEST((LessThanEqualTo<>(0) < 1));    //   <-0]  <  1
  BOOST_TEST(!(LessThanEqualTo<>(0) > -2));  //   <-0]  > -2
  BOOST_TEST(!(LessThanEqualTo<>(0) > 0));   //   <-0]  >  0
  BOOST_TEST(!(LessThanEqualTo<>(0) > 1));   //   <-0]  >  1

  BOOST_TEST(!(LessThan<>(0) < -2));  //   <-0>  < -2
  BOOST_TEST((LessThan<>(0) < 0));    //   <-0>  <  0
  BOOST_TEST((LessThan<>(0) < 1));    //   <-0>  <  1
  BOOST_TEST(!(LessThan<>(0) > -2));  //   <-0>  > -2
  BOOST_TEST(!(LessThan<>(0) > 0));   //   <-0>  >  0
  BOOST_TEST(!(LessThan<>(0) > 1));   //   <-0>  >  1

  {
    BetweenLimits<> const b(GreaterThanEqualTo<>(0), LessThan<>(4));
    BOOST_TEST(!(b < -2));  //   [0,4> < -2
    BOOST_TEST(!(b < 0));   //   [0,4> <  0
    BOOST_TEST(!(b < 1));   //   [0,4> <  1
    BOOST_TEST((b < 4));    //   [0,4> <  4
    BOOST_TEST((b < 6));    //   [0,4> <  6
    BOOST_TEST((b > -2));   //   [0,4> > -2
    BOOST_TEST(!(b > 0));   //   [0,4> >  0
    BOOST_TEST(!(b > 1));   //   [0,4> >  1
    BOOST_TEST(!(b > 4));   //   [0,4> >  4
    BOOST_TEST(!(b > 6));   //   [0,4> >  6
  }

  {
    BetweenLimits<> const b(GreaterThan<>(0), LessThan<>(4));
    BOOST_TEST(!(b < -2));  //    <0,4> < -2
    BOOST_TEST(!(b < 0));   //    <0,4> <  0
    BOOST_TEST(!(b < 1));   //    <0,4> <  1
    BOOST_TEST((b < 4));    //    <0,4> <  4
    BOOST_TEST((b < 6));    //    <0,4> <  6
    BOOST_TEST((b > -2));   //    <0,4> > -2
    BOOST_TEST((b > 0));    //    <0,4> >  0
    BOOST_TEST(!(b > 1));   //    <0,4> >  1
    BOOST_TEST(!(b > 4));   //    <0,4> >  4
    BOOST_TEST(!(b > 6));   //    <0,4> >  6
  }

  {
    BetweenLimits<> const b(GreaterThanEqualTo<>(0), LessThanEqualTo<>(4));
    BOOST_TEST(!(b < -2));  //   [0,4] < -2
    BOOST_TEST(!(b < 0));   //   [0,4] <  0
    BOOST_TEST(!(b < 1));   //   [0,4] <  1
    BOOST_TEST(!(b < 4));   //   [0,4] <  4
    BOOST_TEST((b < 6));    //   [0,4] <  6
    BOOST_TEST((b > -2));   //   [0,4] > -2
    BOOST_TEST(!(b > 0));   //   [0,4] >  0
    BOOST_TEST(!(b > 1));   //   [0,4] >  1
    BOOST_TEST(!(b > 4));   //   [0,4] >  4
    BOOST_TEST(!(b > 6));   //   [0,4] >  6
  }

  {
    BetweenLimits<> const b(GreaterThan<>(0), LessThanEqualTo<>(4));
    BOOST_TEST(!(b < -2));  //    <0,4] < -2
    BOOST_TEST(!(b < 0));   //    <0,4] <  0
    BOOST_TEST(!(b < 1));   //    <0,4] <  1
    BOOST_TEST(!(b < 4));   //    <0,4] <  4
    BOOST_TEST((b < 6));    //    <0,4] <  6
    BOOST_TEST((b > -2));   //    <0,4] > -2
    BOOST_TEST((b > 0));    //    <0,4] >  0
    BOOST_TEST(!(b > 1));   //    <0,4] >  1
    BOOST_TEST(!(b > 4));   //    <0,4] >  4
    BOOST_TEST(!(b > 6));   //    <0,4] >  6
  }
}

BOOST_AUTO_TEST_CASE(less_operator)
{
  using namespace com;

  BOOST_TEST(!(GreaterThan<>(0).less(LessThanEqualTo<>(4))));

  // < 0   vs.  >= 4
  BOOST_TEST((LessThan<>(0).less(GreaterThanEqualTo<>(4))));

  // <= 4   vs.  >= 4
  BOOST_TEST(!(LessThanEqualTo<>(4).less(GreaterThanEqualTo<>(4))));
  BOOST_TEST((LessThanEqualTo<>(4).less(GreaterThan<>(4))));
  BOOST_TEST((LessThan<>(4).less(GreaterThanEqualTo<>(4))));

  BOOST_TEST(!(LessThanEqualTo<>(4).less(AnythingInterval<>())));
  BOOST_TEST(!(AnythingInterval<>().less(AnythingInterval<>())));

  BOOST_TEST((EqualTo<>(4).less(GreaterThan<>(4))));
  BOOST_TEST(!(EqualTo<>(4).less(GreaterThanEqualTo<>(4))));

  BOOST_TEST((EqualTo<>(3).less(GreaterThan<>(4))));
  BOOST_TEST((EqualTo<>(3).less(GreaterThanEqualTo<>(4))));

  BOOST_TEST(!GreaterThan<>(4).less(GreaterThan<>(5)));
}

BOOST_AUTO_TEST_CASE(eq_operator)
{
  using namespace com;

  BOOST_TEST((GreaterThan<>(0) != (LessThanEqualTo<>(4))));
  BOOST_TEST(GreaterThan<>(0) == GreaterThan<>(0));
  BOOST_TEST(LessThanEqualTo<>(4) != AnythingInterval<>());
  BOOST_TEST(AnythingInterval<>() == AnythingInterval<>());
  BOOST_TEST(EqualTo<>(4) == EqualTo<>(4));
  BOOST_TEST(EqualTo<>(4) != EqualTo<>(10));
}

BOOST_AUTO_TEST_CASE(round_error)
{
  using namespace com;

  double const v(0.2);
  float const vf(0.2F);
  // assumptions:
  // ! v in < 0.2, 0.4]
  //   v in < 0  , 0.2]
  BOOST_TEST(!BetweenLimits<>(GreaterThan<>(0.2), LessThanEqualTo<>(0.4)).valid(v));
  BOOST_TEST(BetweenLimits<>(GreaterThan<>(0), LessThanEqualTo<>(0.2)).valid(v));
  {
    typedef float CT;  // choose template
    BOOST_TEST(!BetweenLimits<CT>(GreaterThan<CT>(0.2F), LessThanEqualTo<CT>(0.4F)).valid(vf));
    BOOST_TEST(BetweenLimits<CT>(GreaterThan<CT>(0.0F), LessThanEqualTo<CT>(0.2F)).valid(vf));
  }
  {
    // ROUNDING ERROR comparing float(0.2) ?= double(0.2)
    typedef double CT;  // choose template
    BOOST_TEST(        // <-- this is not what we expect
        BetweenLimits<CT>(GreaterThan<CT>(0.2), LessThanEqualTo<CT>(0.4)).valid(vf));
    BOOST_TEST(!  // <-- this is not what we expect
                BetweenLimits<CT>(GreaterThan<CT>(0.0), LessThanEqualTo<CT>(0.2)).valid(vf));
  }
  {                     // OK float instantation
    typedef double CT;  // choose template
    BOOST_TEST(!BetweenLimits<CT>(GreaterThan<CT>(0.2f), LessThanEqualTo<CT>(0.4f)).valid(vf));
    BOOST_TEST(BetweenLimits<CT>(GreaterThan<CT>(0.0f), LessThanEqualTo<CT>(0.2f)).valid(vf));
  }
  {
    // the solution, ctor argument must be a float if the valid argument is a float
    typedef double CT;  // choose template
    BOOST_TEST(!BetweenLimits<CT>(GreaterThan<CT>(vf), LessThanEqualTo<CT>(0.4)).valid(vf));
    BOOST_TEST(BetweenLimits<CT>(GreaterThan<CT>(0), LessThanEqualTo<CT>(vf)).valid(vf));
  }
}

BOOST_AUTO_TEST_CASE(limit)
{
  using namespace com;

  {
    typedef Interval<double> D;  // choose template
    BOOST_TEST(D::maxLimit() + D::minLimit() == 0);
  }
  {
    typedef Interval<float> F;  // choose template
    BOOST_TEST(F::maxLimit() + F::minLimit() == 0);
  }
}
