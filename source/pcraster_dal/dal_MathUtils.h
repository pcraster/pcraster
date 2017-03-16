#ifndef INCLUDED_DAL_MATHUTILS
#define INCLUDED_DAL_MATHUTILS


// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif

#ifndef INCLUDED_BOOST_MATH_COMMON_FACTOR
#include <boost/math/common_factor.hpp>
#define INCLUDED_BOOST_MATH_COMMON_FACTOR
#endif

#ifndef INCLUDED_BOOST_NUMERIC_CONVERSION_CONVERTER
#include <boost/numeric/conversion/converter.hpp>
#define INCLUDED_BOOST_NUMERIC_CONVERSION_CONVERTER
#endif

#ifndef INCLUDED_BOOST_STATIC_ASSERT
#include <boost/static_assert.hpp>
#define INCLUDED_BOOST_STATIC_ASSERT
#endif


#ifndef INCLUDED_BOOST_TYPE_TRAITS
#include <boost/type_traits.hpp>
#define INCLUDED_BOOST_TYPE_TRAITS
#endif

#ifndef INCLUDED_BOOST_TYPE_TRAITS_IS_FLOATING_POINT
#include <boost/type_traits/is_floating_point.hpp>
#define INCLUDED_BOOST_TYPE_TRAITS_IS_FLOATING_POINT
#endif

#ifndef INCLUDED_BOOST_VERSION
#include <boost/version.hpp>
#define INCLUDED_BOOST_VERSION
#endif

#if BOOST_VERSION > 105800
#include <boost/test/tools/floating_point_comparison.hpp>
#else
#include <boost/test/floating_point_comparison.hpp>
#endif

// PCRaster library headers.
#ifndef INCLUDED_PCRTYPES
#include "pcrtypes.h"
#define INCLUDED_PCRTYPES
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_ARRAY
#include "dal_Array.h"
#define INCLUDED_DAL_ARRAY
#endif



namespace dal {

//! Rounds \a value to the nearest integer value and returns the result.
/*!
  \param     value Value to round.
  \return    Rounded value.
  \exception std::bad_cast in case the target type cannot represent the rounded
             value, for example when -0.6 is rounded and the target type is
             an unsigned integer type.
  \warning   .
  \sa        http://www.boost.org/libs/numeric/conversion/doc/converter.html
*/
template<typename FloatType, typename IntegerType>
inline IntegerType round(
         FloatType const& value)
{
  // Boost.Numeric.Converter set up with nearest neighbour rounding.
  typedef boost::numeric::conversion_traits<IntegerType, FloatType> Traits;
  typedef typename boost::numeric::converter<IntegerType, FloatType,
         Traits,
         boost::numeric::def_overflow_handler,
         boost::numeric::RoundEven<typename Traits::source_type> > Converter;

  return Converter::convert(value);
}



template<typename T>
inline bool comparable(
         T const& lhs,
         T const& rhs)
{
  BOOST_STATIC_ASSERT((!boost::is_floating_point<T>::value));

  return lhs == rhs;
}


//! Compares two floating point values.
/*!
  \param     lhs First value.
  \param     rhs Second value.
  \return    true or false
  \warning   The tolerance value used by the algorithm is set to 1e-4 percent.

  True is returned when the difference between the two values does not exceed
  the tolerance value.

  For a discussion of the exact algorithm used see the docs of
  "Floating-point comparison algorithms" of the boost test library.

  Since the algorithm uses a percentage to check whether two values are close,
  when testing values close to 0 this function might return false negatives.
  In that case, you might want to add a fixed amount to each side. Typical
  example:

  \code
  assert(fmod(value, 1.0), 0.0);                 // Fails easely.
  assert(fmod(value, 1.0) + 1.0, 0.0 + 1.0);     // Fails in much less cases.
  \endcode
*/
template<>
inline bool comparable(
         REAL4 const& lhs,
         REAL4 const& rhs)
{
#if BOOST_VERSION > 105800
  static boost::math::fpc::close_at_tolerance<REAL4> tester(
         boost::math::fpc::fpc_detail::fraction_tolerance<REAL4>(REAL4(1e-4)),
         boost::math::fpc::FPC_STRONG);
#else
  static boost::test_tools::close_at_tolerance<REAL4> tester(
#if BOOST_VERSION < 103401
         REAL4(1e-4),
#else
         boost::test_tools::fraction_tolerance_t<REAL4>(REAL4(1e-4)),
#endif
         boost::test_tools::FPC_STRONG);
#endif

  return tester(lhs, rhs);
}

template<>
inline bool comparable(
         REAL8 const& lhs,
         REAL8 const& rhs)
{

#if BOOST_VERSION > 105800
  static boost::math::fpc::close_at_tolerance<REAL8> tester(
         boost::math::fpc::fpc_detail::fraction_tolerance<REAL8>(REAL8(1e-6)),
         boost::math::fpc::FPC_STRONG);
#else
  static boost::test_tools::close_at_tolerance<REAL8> tester(
#if BOOST_VERSION < 103401
         REAL8(1e-6),
#else
         boost::test_tools::fraction_tolerance_t<REAL8>(REAL8(1e-6)),
#endif
         boost::test_tools::FPC_STRONG);
#endif

  return tester(lhs, rhs);
}

template <typename Iterator>
inline bool comparable(
         Iterator begin1,
         Iterator end1,
         Iterator begin2)
{
  while(begin1 != end1) {
    if(!comparable(*begin1, *begin2)) {
      return false;
    }

    ++begin1;
    ++begin2;
  }

  return true;
}

template<typename T>
inline bool smallerOrComparable(
         T const& lhs,
         T const& rhs)
{
  return lhs < rhs || comparable(lhs, rhs);
}

template<typename T>
inline bool greaterOrComparable(
         T const& lhs,
         T const& rhs)
{
  return lhs > rhs || comparable(lhs, rhs);
}

// template<typename T, typename Iterator>
// inline bool isRegularIncreasingRange(
//          T& first,
//          T& last,
//          T& interval,
//          Iterator begin,
//          Iterator end)
// {
//   if(begin != end) {
//     first = *begin++;
//
//     if(begin != end && *begin > first) {
//       interval = *begin++ - first;
//
//       if(begin == end) {
//         last = *(begin - 1);
//         return true;
//       }
//
//       while(begin != end - 1) {
//         if(*begin + interval != *(begin + 1)) {
//           return false;
//         }
//
//         ++begin;
//       }
//
//       last = *begin;
//       return true;
//     }
//   }
//
//   return false;
// }

template<typename T, typename ForwardIterator>
inline bool isRegularIncreasingRange(
         T& first,
         T& last,
         T& interval,
         ForwardIterator pos,
         ForwardIterator end)
{
  if(pos == end) {
    return false;
  }

  first = *pos;
  last = *pos;
  ++pos;

  // Return false if there's only one value in the collection.
  if(pos == end) {
    return false;
  }

  // Make sure the collection is sorted.
  assert(*pos > first);

  // The difference between the first two values is taken to be the interval.
  interval = *pos - first;

  while(pos != end) {
    // Return false if the intervals are different.
    if(!comparable<T>(*pos - last, interval)) {
      return false;
    }

    last = *pos;
    ++pos;

    // Make sure the collection is sorted.
    assert(pos == end || *pos > last);
  }

  return true;
}

namespace {

template<typename T>
inline bool valueInRange(
         T const& first,
         T const& last,
         T const& step,
         T const& value)
{
  return value >= first && value <= last && (value - first) % step == 0;
}

template<>
inline bool valueInRange(
         float const& first,
         float const& last,
         float const& step,
         float const& value)
{
  if(greaterOrComparable<float>(value, first) &&
     smallerOrComparable<float>(value, last)) {

    float multiplier = comparable<float>(value, first)
         ? 0.0f : (value - first) / step;

    return comparable<float>(multiplier,
         static_cast<float>(round<float, int>(multiplier)));
  }

  return false;
}

template<>
inline bool valueInRange(
         double const& first,
         double const& last,
         double const& step,
         double const& value)
{
  if(greaterOrComparable<double>(value, first) &&
     smallerOrComparable<double>(value, last)) {
    double multiplier = (value - first) / step;

    return comparable<double>(multiplier,
         static_cast<double>(round<double, int>(multiplier)));
  }

  return false;
}

template<typename T, class ForwardIterator>
inline bool valuesInRange(
         ForwardIterator begin,
         ForwardIterator end,
         T first,
         T last,
         T interval)
{
  if(begin == end) {
    return false;
  }

  for(; begin != end; ++begin) {
    if(!valueInRange(first, last, interval, *begin)) {
      return false;
    }
  }

  return true;
}


template<typename T, typename ForwardIterator, bool IsInteger>
struct DoIsIncreasingRange
{
  static bool calculate(
         T& first,
         T& last,
         T& interval,
         ForwardIterator pos,
         ForwardIterator end);
};

template<typename T, typename ForwardIterator>
struct DoIsIncreasingRange<T, ForwardIterator, false>
{
  // Floats.
  inline static bool calculate(
         T& first,
         T& last,
         T& interval,
         ForwardIterator begin,
         ForwardIterator end)
  {
    if(begin == end || begin + 1 == end) {
      return false;
    }

    // Determine smallest difference between two values.
    ForwardIterator pos = begin;

    assert(*(pos + 1) > *pos);
    interval = *(pos + 1) - *pos;

    while(++pos + 1 != end) {
      assert(*(pos + 1) > *pos);
      interval = std::min(interval, *(pos + 1) - *pos);
    }

    // See whether all values are in the range. Otherwise lower the interval.
    first = *begin;
    last = *pos;

    while(!valuesInRange(begin, end, first, last, interval)) {
      assert(interval > T(0.01));
      interval -= T(0.01);
    }

    return true;
  }
};

template<typename T, typename ForwardIterator>
struct DoIsIncreasingRange<T, ForwardIterator, true>
{
  // Integrals.
  inline static bool calculate(
         T& first,
         T& last,
         T& interval,
         ForwardIterator begin,
         ForwardIterator end)
  {
    if(begin == end || begin + 1 == end) {
      return false;
    }

    // Determine smallest difference between two values.
    ForwardIterator pos = begin;

    assert(*(pos + 1) > *pos);
    interval = *(pos + 1) - *pos;

    while(++pos + 1 != end) {
      assert(*(pos + 1) > *pos);
      interval = std::min(interval, *(pos + 1) - *pos);
    }

    // See whether all values are in the range. Otherwise lower the interval.
    first = *begin;
    last = *pos;

    while(!valuesInRange(begin, end, first, last, interval)) {
      assert(interval > 1);
      --interval;
    }

    return true;
  }
};

} // Anonymous namespace

template<typename T, typename ForwardIterator>
inline bool isIncreasingRange(
         T& first,
         T& last,
         T& interval,
         ForwardIterator pos,
         ForwardIterator end)
{
  return DoIsIncreasingRange<T, ForwardIterator,
         boost::is_integral<T>::value>::calculate(first, last, interval, pos,
              end);
}

template<typename T>
inline bool valueInExtent(
         T const& first,
         T const& last,
         T const& value)
{
  return greaterOrComparable(value, first) && smallerOrComparable(value, last);
}



template<typename T>
inline T gcd(
         T a,
         T b)
{
  return boost::math::gcd<T>(a, b);
}

template<typename T>
T gcdFloat(
         T a,
         T b);

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Find a way to multiply a and b by a computed value instead of the
             1000000 used now.
*/
template<>
inline float gcd(
         float a,
         float b)
{
  // extern float gcdFloat(float a, float b);
#ifdef DEBUG_DEVELOP
  return gcdFloat(1e6f * a, 1e6f * b) / 1e6f;
#else
  return static_cast<float>(round<float, int>((
         gcdFloat(1e4f * a, 1e4f * b)))) / 1e4f;
#endif
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \todo      Find a way to multiply a and b by a computed value instead of the
             1000000 used now.
*/
template<>
inline double gcd(
         double a,
         double b)
{
  return gcdFloat(1e6 * a, 1e6 * b) / 1e6;
}

template<typename T>
void               mergeRanges         (T& first,
                                        T& last,
                                        T& step,
                                        T const& first2,
                                        T const& last2,
                                        T const& step2);

template<typename T>
inline void interpolate(
         Array<T>& array)
{
  size_t index = 0;

  // Skip leading missing values.
  while(index < array.size() && pcr::isMV(array[index])) {
    ++index;
  }

  size_t preNonMVIndex, postNonMVIndex;
  double preWeight, postWeight;

  while(index < array.size()) {

    // Skip leading non missing values.
    while(index < array.size() && !pcr::isMV(array[index])) {
      ++index;
    }

    // Nothing left to do.
    if(index == array.size()) {
      break;
    }

    // Current value is a missing value which needs to be calculated. Find first
    // non missing value after this one.

    preNonMVIndex = index - 1;

    // Skip all missing values which need to be filled.
    while(index < array.size() && pcr::isMV(array[index])) {
      ++index;
    }

    // Nothing left to do.
    if(index == array.size()) {
      break;
    }

    postNonMVIndex = index;

    // All missing values between pre- and postNonMVIndex need to be calculated.
    for(size_t i = preNonMVIndex + 1; i < postNonMVIndex; ++i) {
      preWeight = postNonMVIndex - i;
      postWeight = i - preNonMVIndex;
      array[i] = static_cast<T>((preWeight * array[preNonMVIndex] +
         postWeight * array[postNonMVIndex]) / (preWeight + postWeight));
    }
  }
}

template<typename T>
inline void interpolate(
         T& result,
         T const& value1,
         double distance1,
         T const& value2,
         double distance2)
{
  if(pcr::isMV(value1) || pcr::isMV(value2)) {
    pcr::setMV(result);
  }
  else {
    result = static_cast<T>((distance2 * value1 + distance1 * value2) /
         (distance1 + distance2));
  }
}

template<typename T>
inline void interpolate(
         T* result,
         size_t size,
         T const* array1,
         double distance1,
         T const* array2,
         double distance2)
{
  assert(distance1 > 0.0 && distance2 > 0.0);

  double sumOfDistances = distance1 + distance2;

  for(size_t i = 0; i < size; ++i) {
    if(pcr::isMV(array1[i]) || pcr::isMV(array2[i])) {
      pcr::setMV(result[i]);
    }
    else {
      result[i] = static_cast<T>(
         (distance2 * array1[i] + distance1 * array2[i]) /
         sumOfDistances);
    }
  }
}

template<typename T>
inline void interpolate(
         Array<T>& result,
         Array<T> const& array1,
         double distance1,
         Array<T> const& array2,
         double distance2)
{
  assert(distance1 > 0 && distance2 > 0);

  double sumOfDistances = distance1 + distance2;
  typename Array<T>::iterator it;
  typename Array<T>::const_iterator it1, it2;

  for(it = result.begin(), it1 = array1.begin(), it2 = array2.begin();
         it < result.end(); ++it, ++it1, ++it2) {
    if(pcr::isMV(*it1) || pcr::isMV(*it2)) {
      pcr::setMV(*it);
    }
    else {
      *it = static_cast<T>(
         (distance2 * *it1 + distance1 * *it2) / sumOfDistances);
    }
  }
}

template<typename T>
inline void interpolate(
         Array<T>& result,
         Array<T> const& cells1,
         Array<double> const& distances1,
         Array<T> const& cells2,
         Array<double> const& distances2)
{
  typename Array<T>::iterator result_it;
  typename Array<T>::const_iterator cell1_it, cell2_it;
  typename Array<double>::const_iterator dist1_it, dist2_it;

  for(   result_it = result.begin(),
         cell1_it = cells1.begin(), dist1_it = distances1.begin(),
         cell2_it = cells2.begin(), dist2_it = distances2.begin();
         result_it < result.end();
         ++result_it, ++cell1_it, ++dist1_it, ++cell2_it, ++dist2_it) {
    if(pcr::isMV(*cell1_it) || pcr::isMV(*cell2_it)) {
      pcr::setMV(*result_it);
    }
    else {
      assert(!pcr::isMV(*dist1_it));
      assert(!pcr::isMV(*dist2_it));
      *result_it = (*dist2_it * *cell1_it + *dist1_it * *cell2_it) /
         *dist1_it + *dist2_it;
    }
  }
}

template<typename T>
inline void fillUsingPreviousValue(
         Array<T>& array)
{
  size_t index = 0;

  // Skip leading missing values.
  while(index < array.size() && pcr::isMV(array[index])) {
    ++index;
  }

  while(index < array.size()) {
    T value = array[index];

    ++index;

    while(index < array.size() && pcr::isMV(array[index])) {
      array[index] = value;
      ++index;
    }
  }
}

template<typename T>
inline T clamp(
         std::vector<T> const& values,
         T value)
{
  assert(!values.empty());

  size_t index = 0;

  if(values.front() > value) {
    // Nothing to do.
  }
  else if(values.back() < value) {
    index = values.size() - 1;
  }
  else {
    size_t belowIndex = index;

    while(values[index] < value) {
      belowIndex = index;
      ++index;
    }

    size_t aboveIndex = index;

    // Assumes unique values...
    assert(values[belowIndex] < value ||
         comparable<T>(values[belowIndex], value));
    assert(values[aboveIndex] > value ||
         comparable<T>(values[aboveIndex], value));

    if(comparable<T>(value - values[belowIndex], values[aboveIndex] - value)) {
      index = belowIndex;
    }
    else {
      index = value - values[belowIndex] <
              values[aboveIndex] - value ? belowIndex : aboveIndex;
    }

    /*
    // Assumes unique values...
    assert(values[belowIndex] <= value);
    assert(values[aboveIndex] >= value);

    if(std::abs((value - values[belowIndex]) -
         (values[aboveIndex] - value)) < 0.000001) {
      index = belowIndex;
    }
    else {
      index = value - values[belowIndex] <
              values[aboveIndex] - value ? belowIndex : aboveIndex;
    }
    */
  }

  return values[index];
}

template<typename T>
inline T clamp(
         T first,
         T last,
         T interval,
         T value)
{
  assert((first < last || comparable<T>(first, last)) && interval > T(0));

  T result = first;

  // if(!(value < first)) {
  //   size_t multiplier = 0;

  //   for(;
  //       (result < last - interval ||
  //        comparable<T>(result, last - interval)) &&
  //        result < value;
  //       result = first + ++multiplier * interval) {
  //     if((result < value || comparable<T>(result, value)) &&
  //        result + interval > value) {
  //       if(value - result > result + interval - value) {
  //         result = first + ++multiplier * interval;
  //       }
  //       break;
  //     }
  //   }
  // }

  if(value < first) {
    result = first;
  }
  // else if(greaterOrComparable(value, last)) {
  //   result = last;
  // }
  else {
    size_t multiplier = 0;

    // Find values between which the value lies.
    while(greaterOrComparable(value, first + ((multiplier + 1) * interval))) {
      ++multiplier;
    }

    if(multiplier < (last - first) / interval) {
      // Determine the closest value.
      result = value - (first + (multiplier * interval)) <
               (first + ((multiplier + 1) * interval)) - value
           ? first + (multiplier * interval)
           : first + ((multiplier + 1) * interval);
    }
    else {
      // Outside of interval, pick the last value in the interval.
      result = first + ((last - first) / interval) * interval;
    }
  }

  return result;
}

//   size_t           clamp                (size_t first,
//                                         size_t last,
//                                         size_t interval,
//                                         size_t value);




template<typename T>
inline T average(T const* it, T const* end)
{
  T mv;
  pcr::setMV(mv);

  T result = 0.0;
  size_t count = 0;

  while(it != end) {
    if(!pcr::isMV(*it)) {
      result += *it;
      ++count;
      ++it;
    }
  }

  return count == 0 ? mv : result / count;
}

template<class Raster, typename T>
inline T average(
         Raster const& raster,
         size_t row,
         size_t col,
         size_t length)
{
  T mv;
  pcr::setMV(mv);

  T result = 0.0;
  size_t count = 0;

  for(size_t i = 0; i < length; ++i) {
    for(size_t j = 0; j < length; ++j) {
      if(!pcr::isMV(raster.template cell<T>(row + i, col + j))) {
        result += raster.template cell<T>(row + i, col + j);
        ++count;
      }
    }
  }

  return count == 0 ? mv : result / count;
}

//! Converts an angle in radians to degrees.
/*!
  \tparam    T Floating point type.
  \param     radians Angle in radians ([-pi/2, pi/2]).
  \return    Angle in degrees ([-90, 90]).
*/
template<typename T>
inline T radiansToDegrees(
         T const& radians)
{
  assert(radians >= T(-M_PI / 2.0) && radians <= T(M_PI / 2.0));

  T result = (T(180.0) * radians) / T(M_PI);

  assert(result >= T(-90.0) && result <= T(90.0));

  return result;
}

//! Returns the angle in degrees of the vector \a x, \a y.
/*!
  \tparam    T Floating point type.
  \param     x X magnitude.
  \param     y Y magnitude.
  \return    Angle in degrees ([-90, 90]).
*/
template<typename T>
inline T angleInDegrees(
         T const& x,
         T const& y)
{
  T result;

  if(x == 0.0) {
    result = 90.0;
  }
  else {
    T radians = std::atan(y / x);
    result = radiansToDegrees(radians);
  }

  assert(result >= T(-90.0) && result <= T(90.0));
  return result;
}

//! Returns the clockwise angle of the vector \a x, \a y.
/*!
  \tparam    T Floating point type.
  \param     x X magnitude.
  \param     y Y magnitude.
  \return    Angle in degrees ([0, 360]).
*/
template<typename T>
inline T clockwiseAngle(
         T const& x,
         T const& y)
{
  assert(!pcr::isMV(x) && !pcr::isMV(y));

  T result = angleInDegrees(x, y);

  assert(result >= T(-90.0) && result <= T(90.0));

  if(x >= T(0.0)) {
    if(y >= T(0.0)) {
      result = T(90.0) - result;
    }
    else {
      result = T(90.0) + std::abs(result);
    }
  }
  else {
    if(y >= T(0.0)) {
      result = T(270.0) + std::abs(result);
    }
    else {
      result = T(270.0) - result;
    }
  }

  assert(result >= T(0.0) && result <= T(360.0));

  return result;
}

} // namespace dal

#endif
