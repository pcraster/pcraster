#ifndef INCLUDED_RANGECLASSIFICATION
#include "RangeClassification.h"
#define INCLUDED_RANGECLASSIFICATION
#endif

// External headers.
#ifndef INCLUDED_ALGORITHM
#include <algorithm>
#define INCLUDED_ALGORITHM
#endif

#ifndef INCLUDED_BOOST_FOREACH
#include <boost/foreach.hpp>
#define INCLUDED_BOOST_FOREACH
#endif

#ifndef INCLUDED_QWT_SCALE_ENGINE
#include <qwt_scale_engine.h>
#define INCLUDED_QWT_SCALE_ENGINE
#endif

// Project headers.
#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

// Module headers.



/*!
  \file
  This file contains the implementation of the RangeClassification class.
*/



namespace {

template<typename T, class BackInsertIterator>
void classifyLinear(
         T min,
         T max,
         size_t nrClasses,
         BackInsertIterator it)
{
  assert(dal::smallerOrComparable(min, max));
  assert(nrClasses > 0);

  QwtLinearScaleEngine scaleEngine;
  QwtScaleDiv scaleDivision(scaleEngine.divideScale(min, max, nrClasses, 0));
  QwtValueList const& ticks(scaleDivision.ticks(QwtScaleDiv::MajorTick));

  BOOST_FOREACH(double value, ticks) {
    *it++ = value;
  }
}



template<typename T, class BackInsertIterator>
void classifyLog10(
         T min,
         T max,
         size_t nrClasses,
         BackInsertIterator it)
{
  assert(dal::smallerOrComparable(min, max));
  assert(nrClasses > 0);

  QwtLog10ScaleEngine scaleEngine;
  QwtScaleDiv scaleDivision(scaleEngine.divideScale(min, max, nrClasses, 0));
  QwtValueList const& ticks(scaleDivision.ticks(QwtScaleDiv::MajorTick));

  BOOST_FOREACH(double value, ticks) {
    *it++ = value;
  }
}

} // Anonymous namespace



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC RANGECLASSIFICATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF RANGECLASSIFICATION MEMBERS
//------------------------------------------------------------------------------

template<typename T>
RangeClassification<T>::RangeClassification()

  : Classification<T>(),
    _min(T(0)),
    _max(T(1))

{
  classify(_min, _max, 10, Linear);
}



template<typename T>
RangeClassification<T>::RangeClassification(
         T min,
         T max,
         size_t nrClasses,
         Algorithm algorithm)

  : Classification<T>(),
    _min(min),
    _max(max)

{
  classify(_min, _max, nrClasses, algorithm);
}



template<typename T>
RangeClassification<T>::RangeClassification(
         T min,
         T max,
         T minCutoff,
         T maxCutoff,
         size_t nrClasses,
         Algorithm algorithm)

  : Classification<T>(),
    _min(min),
    _max(max)

{
  classify(minCutoff, maxCutoff, nrClasses, algorithm);
}



//! Destructor.
/*!
*/
template<typename T>
RangeClassification<T>::~RangeClassification()
{
}



template<typename T>
void RangeClassification<T>::classify(
         T minCutoff,
         T maxCutoff,
         size_t nrClasses,
         Algorithm algorithm)
{
  this->clear();

  switch(algorithm) {
    case Linear: {
      classifyLinear(minCutoff, maxCutoff, nrClasses,
         std::back_insert_iterator<RangeClassification>(*this));
      break;
    }
    case Log10: {
      // For some reason the upper border is duplicated in some cases.
      // Therefore we postprocess the calculated borders to remove duplicates.
      std::vector<T> borders;
      classifyLog10(minCutoff, maxCutoff, nrClasses,
         std::back_insert_iterator< std::vector<T> >(borders));
      this->insert(this->end(), borders.begin(),
         std::unique(borders.begin(), borders.end()));
      break;
    }
    default: {
      assert(false);
      classify(minCutoff, maxCutoff, nrClasses, Linear);
      break;
    }
  }

  assert(std::unique(this->begin(), this->end()) == this->end());

  _algorithm = algorithm;
}



template<typename T>
size_t RangeClassification<T>::index(
         T value) const
{
  assert(!this->empty());
  assert(dal::greaterOrComparable(value, this->front()));
  assert(dal::smallerOrComparable(value, this->back()));

  size_t i = 1;

  for(i = 0; i < this->size() &&
         dal::smallerOrComparable(this->operator[](i), value); ++i) { }

  return --i;
}



template<typename T>
T RangeClassification<T>::min() const
{
  return _min;
}



template<typename T>
T RangeClassification<T>::max() const
{
  return _max;
}



template<typename T>
T RangeClassification<T>::minCutoff() const
{
  assert(!this->empty());

  return this->front();
}



template<typename T>
T RangeClassification<T>::maxCutoff() const
{
  assert(!this->empty());

  return this->back();
}



template class RangeClassification<float>;
template class RangeClassification<double>;

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

