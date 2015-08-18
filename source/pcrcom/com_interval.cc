#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_COM_INTERVALTYPES
#include "com_intervaltypes.h"
#define INCLUDED_COM_INTERVALTYPES
#endif

// Library headers.
#ifndef INCLUDED_SSTREAM
#include <sstream>
#define INCLUDED_SSTREAM
#endif
#ifndef INCLUDED_COM_MATH
#include "com_math.h"
#define INCLUDED_COM_MATH
#endif
#include <boost/spirit/include/classic_core.hpp>
#ifndef INCLUDED_BOOST_ALGORITHM_STRING_TRIM
#include <boost/algorithm/string/trim.hpp>
#define INCLUDED_BOOST_ALGORITHM_STRING_TRIM
#endif
#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of all classes
  derived from Interval
*/



//------------------------------------------------------------------------------
// DEFINITION OF INTERVAL MEMBERS
//------------------------------------------------------------------------------

template<typename R>
com::Interval<R>::Interval()
{
}

template<typename R>
com::Interval<R>::~Interval()
{
}

//! sorting criteria for Interval objects
/*! this Interval object is smaller than \a rhs if
 *  all values represented by this interval are smaller
 *  than all values represented in \a rhs.
 *  In other words they do not overlap
 */
template<typename R>
bool com::Interval<R>::less(const Interval<R>& rhs) const
{
  if (max()==rhs.min()) {
    // really < if one side is exclusive
    return (!valid(max())) || (!rhs.valid(rhs.min()));
  }
  return max() < rhs.min();
}

template<typename R>
bool com::Interval<R>::operator==(const Interval& rhs)const
{
  return
   min()==rhs.min()&&
   max()==rhs.max()&&
   valid(min())==rhs.valid(rhs.min())&&
   valid(max())==rhs.valid(rhs.max());
}


//! lowest value representable
template<typename R>
R com::Interval<R>::minLimit()
{
    return NumericLimits<R>::minValue();
}

//! highest value representable
template<typename R>
R com::Interval<R>::maxLimit()
{
  return NumericLimits<R>::maxValue();
}

template<typename R>
R com::Interval<R>::centre() const
{
  return (min()+max())/2;
}

template<typename R>
void com::Interval<R>::print(std::ostream& stream) const
{
  if (min()==max()) {
     // single value
     stream << min();
     return;
  }
  stream << std::string(valid(min()) ? "[" : "<");
  if (min()!=minLimit())
    stream<<min();
  stream << ",";
  if (max()!=maxLimit())
    stream<<max();
  stream << std::string(valid(max()) ? "]" : ">");
}

template<typename R>
std::ostream& com::operator<<(
    std::ostream& stream,
    const Interval<R>& i)
{
  i.print(stream);
  return stream;
}

template<typename R>
com::GreaterThan<R> *com::GreaterThan<R>::createClone() const
{
  return new GreaterThan(*this);
}

template<typename R>
std::string com::GreaterThan<R>::msg() const
{
  std::ostringstream str;
  str << "greater than " << this->d_minV << " (>"<<this->d_minV<<")";
  return str.str();
}


template<typename R>
std::string com::GreaterThanEqualTo<R>::msg() const
{
  std::ostringstream str;
  str << "greater than or equal to " << this->d_minV << " (>="<<this->d_minV<<")";
  return str.str();
}

template<typename R>
com::GreaterThanEqualTo<R> *com::GreaterThanEqualTo<R>::createClone() const
{
  return new GreaterThanEqualTo(*this);
}

template<typename R>
std::string com::LessThan<R>::msg() const
{
  std::ostringstream str;
  str << "less than " << this->d_maxV << " (<"<<this->d_maxV<<")";
  return str.str();
}

template<typename R>
com::LessThan<R> *com::LessThan<R>::createClone() const
{
  return new LessThan(*this);
}

template<typename R>
std::string com::LessThanEqualTo<R>::msg() const
{
  std::ostringstream str;
  str << "less than or equal to " << this->d_maxV << " (<="<<this->d_maxV<<")";
  return str.str();
}

template<typename R>
com::LessThanEqualTo<R> *com::LessThanEqualTo<R>::createClone() const
{
  return new LessThanEqualTo(*this);
}

template<typename R>
std::string com::EqualTo<R>::msg() const
{
  std::ostringstream str;
  str << "equal to " << d_thisValue << " (="<<d_thisValue<<")";
  return str.str();
}

template<typename R>
com::EqualTo<R> *com::EqualTo<R>::createClone() const
{
  return new EqualTo(*this);
}

template<typename R>
std::string com::AnythingInterval<R>::msg() const
{
  return "anything";
}

template<typename R>
com::AnythingInterval<R> *com::AnythingInterval<R>::createClone() const
{
  return new AnythingInterval(*this);
}

//! ctor
/*!
 * \throws
 *   com::BadIntervalFormat if:
 *   <ol>
 *   <li>lower value is higher than high value: [2,1] </li>
 *   <li>lower value is equal to high value but conflicting or empty like
 *       conflict:[2,2> empty:<2,2> ok:[2,2] </li>
 *   </ol>
 */
template<typename R>
com::BetweenLimits<R>::BetweenLimits(
   const LowerLimit<R>&  lowerLimit,
   const UpperLimit<R>&  upperLimit):
    d_lowerLimit(lowerLimit.createClone()),
    d_upperLimit(upperLimit.createClone())
{
  // normal expected case:
  R min(d_lowerLimit->min());
  R max(d_upperLimit->max());

  if (min < max)
    return;

  // most certain errors, except the case [2,2]
  if ( min==max && valid(min) && valid(max) )
    return;

  clean();
  throw com::BadIntervalFormat(
        "lower value is higher than high value");
}

//! dtor
template<typename R>
com::BetweenLimits<R>::~BetweenLimits()
{
  clean();
}

template<typename R>
void com::BetweenLimits<R>::clean()
{
  delete d_lowerLimit;
  d_lowerLimit=0;
  delete d_upperLimit;
  d_upperLimit=0;
}

template<typename R>
com::BetweenLimits<R>::BetweenLimits(const BetweenLimits& rhs)
  : Interval<R>(rhs),
    d_lowerLimit(rhs.d_lowerLimit->createClone()),
    d_upperLimit(rhs.d_upperLimit->createClone())
{
}

template<typename R>
com::BetweenLimits<R> *com::BetweenLimits<R>::createClone() const
{
  return new BetweenLimits(*this);
}


template<typename R>
com::BetweenLimits<R>& com::BetweenLimits<R>::operator=(
                   const BetweenLimits<R>& rhs )
{
  if (this != &rhs) {
    delete d_lowerLimit;
    d_lowerLimit=rhs.d_lowerLimit->createClone();
    delete d_upperLimit;
    d_upperLimit=rhs.d_upperLimit->createClone();
  }
  return *this;
}


template<typename R>
bool com::BetweenLimits<R>::valid(R v) const
{
  return d_lowerLimit->valid(v) && d_upperLimit->valid(v);
}

template<typename R>
bool  com::BetweenLimits<R>::operator<(R v) const
{
  return d_upperLimit->operator<(v);
}

template<typename R>
bool  com::BetweenLimits<R>::operator>(R v) const
{
  return d_lowerLimit->operator>(v);
}

template<typename R>
std::string com::BetweenLimits<R>::msg() const
{
  return d_lowerLimit->msg()+" and "+d_upperLimit->msg();
}

#ifndef INCLUDED_BOOST_OPTIONAL
#include <boost/optional.hpp>
#define INCLUDED_BOOST_OPTIONAL
#endif
#ifndef INCLUDED_BOOST_BIND
#include <boost/bind.hpp>
#define INCLUDED_BOOST_BIND
#endif

namespace com {
   typedef boost::optional<double> OD;

static void spiritParser(
     const std::string& str,
     std::vector<char>& ranges,
      OD& singleValue,
      OD& low,
      OD& high
     )
  {
    // laatste hand parser (in commentaar) zit in revisie 1.14
    using namespace boost::spirit::classic;

   /* normal EBNF
      ( real
        | ( '[' | '<' ) real? ',' real? (']'|'>')
      )
   */
   // TODO model this like spirt's predefined action assign
   //      to elimninate this macro
#define RESET(x) boost::bind(&OD::reset,&x,_1)

   // TODO how te define trailing space correctly in parser?
   std::string copyStr(str);
   boost::algorithm::trim(copyStr);


   bool correct=parse(copyStr.begin(),copyStr.end(),
        //  Begin grammar
          (   real_p[RESET(singleValue)]
              | (
                 ( ch_p('[')[append(ranges)] |
                   ch_p('<')[append(ranges)] )
                    >> (!real_p[RESET(low)]) >> ','
                    >>  !real_p[RESET(high)]
                    >>
                 ( ch_p(']')[append(ranges)] |
                   ch_p('>')[append(ranges)] )
                )
          )
        ,
        //  End grammar
        space_p).full;
  if (!correct)
      throw com::BadIntervalFormat("illegal key format");
 }
}

//! create from a string formatted as a PCRaster lookup key element
/*!
 *  \throws com::BadIntervalFormat in case of error
 */
template<typename R>
com::Interval<R> * com::createIntervalFromLookupTableKey (
    const std::string& str)
{
   std::vector<char>   ranges;
   OD singleValue, low, high;

   //  either float or double
   DEVELOP_PRECOND(!std::numeric_limits<R>::is_integer);

   spiritParser(str,ranges,singleValue,low,high);

   if (!low && !high) {
     if (!singleValue)
       return new com::AnythingInterval<R>(); // infinity
     return new com::EqualTo<R>((R)*singleValue);
   }

   PRECOND(ranges.size() == 2);
   char lowRange  = ranges[0];
   char highRange = ranges[1];

   com::UpperLimit<R>* h(0);
   com::LowerLimit<R>* l(0);

   if (high) {
    if (highRange == ']')
     h = new com::LessThanEqualTo<R>((R)*high);  // ... ,h]
    else {
     POSTCOND(highRange == '>');
     h = new com::LessThan<R>((R)*high);         // ... ,h>
    }
    if (!low)
      return h; // < inf, ...
   }

   if (low) {
    if (lowRange == '[')
     l = new com::GreaterThanEqualTo<R>((R)*low); // [l, ...
    else {
     POSTCOND(lowRange == '<');
     l = new com::GreaterThan<R>((R)*low);        // <l, ...
    }
    if (!h)
      return l; // ... , inf >
   }

   PRECOND(l && h);
   com::Interval<R> *v(0);
   try {
    v=new com::BetweenLimits<R>(*l,*h);
   } catch(...) {
    delete l;
    delete h;
    throw;
   }
   delete l;
   delete h;
   return v;
}



namespace com {
  template class Interval<double>;
  template class UpperLimit<double>;
  template class GreaterThanEqualTo<double>;
  template class GreaterThan<double>;
  template class LessThanEqualTo<double>;
  template class LessThan<double>;
  template class LowerLimit<double>;
  template class EqualTo<double>;
  template class AnythingInterval<double>;
  template class BetweenLimits<double>;
  template Interval<double>* createIntervalFromLookupTableKey (const std::string& str);
  Interval<double> * createClone(Interval<double> const* c)
  { return c->createClone(); }
  void deleteClone(Interval<double> const* c) { delete c; }
  template std::ostream& operator<<(std::ostream& stream, const Interval<double>& i);
}
namespace com {
  template class Interval<float>;
  template class UpperLimit<float>;
  template class GreaterThanEqualTo<float>;
  template class GreaterThan<float>;
  template class LessThanEqualTo<float>;
  template class LessThan<float>;
  template class LowerLimit<float>;
  template class EqualTo<float>;
  template class AnythingInterval<float>;
  template class BetweenLimits<float>;
  template Interval<float>* createIntervalFromLookupTableKey (const std::string& str);
  template std::ostream& operator<<(std::ostream& stream, const Interval<float>& i);

  Interval<float> * createClone(Interval<float> const* c)
  { return c->createClone(); }
  void deleteClone(Interval<float> const* c) { delete c; }
}
/*! be sure to implements maxLimit()+minLimit()==0
 *  test in IntervalTest::testLimit
 */
