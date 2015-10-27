#ifndef INCLUDED_DAL_DIMENSION
#define INCLUDED_DAL_DIMENSION



// Library headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

#ifndef INCLUDED_SET
#include <set>
#define INCLUDED_SET
#endif

#ifndef INCLUDED_VECTOR
#include <vector>
#define INCLUDED_VECTOR
#endif

#ifndef INCLUDED_BOOST_ANY
#include <boost/any.hpp>
#define INCLUDED_BOOST_ANY
#endif

#ifndef INCLUDED_BOOST_STATIC_ASSERT
#include <boost/static_assert.hpp>
#define INCLUDED_BOOST_STATIC_ASSERT
#endif

#ifndef INCLUDED_BOOST_TYPE_TRAITS
#include <boost/type_traits.hpp>
#define INCLUDED_BOOST_TYPE_TRAITS
#endif

// PCRaster library headers.
#ifndef INCLUDED_DEV_COMPILER
#include "dev_Compiler.h"
#define INCLUDED_DEV_COMPILER
#endif

// Module headers.
#ifndef INCLUDED_DAL_CONFIGURE
#include "dal_Configure.h"
#define INCLUDED_DAL_CONFIGURE
#endif

#ifndef INCLUDED_DAL_DEF
#include "dal_Def.h"
#define INCLUDED_DAL_DEF
#endif

#ifndef INCLUDED_DAL_MATHUTILS
#include "dal_MathUtils.h"
#define INCLUDED_DAL_MATHUTILS
#endif

#ifndef INCLUDED_DAL_UTILS
#include "dal_Utils.h"
#define INCLUDED_DAL_UTILS
#endif



namespace dal {
  // Dimension declarations.
}



namespace dal {




//! Class for describing the dimensions of a data space.
/*!
  A dimension provides information about one dimension in a dataset. A set of
  one or more dimensions can describe the dimensional properties of the whole
  dataset.

  For each dimension settings are stored: coordinate type, meaning,
  discretisation and values. Depending on the discretisation the values
  represent start, end and interval of a range of values, or they represent a
  set of individual values along the dimension.

  Examples of Dimension settings for different datasets.

  A raster has one dimension with the folowing settings:
  - coordinate type: NumericalCoordinates (rows, columns, world coordinates)
  - meaning: Space
  - discretisation: RegularDiscretisation

  A vector has one dimension with the folowing settings:
  - coordinate type: NumericalCoordinates (world coordinates)
  - meaning: Space
  - discretisation: BorderedDiscretisation

  Dynamic stack has three dimensions, the two above and one with settings:
  - coordinate type: NumericalCoordinates (time steps)
  - meaning: Time
  - discretisation: RegularDiscretisation

  A set of probabalistic distribution function rasters has the two dimensions
  of rasters mentioned above including one dimension with settings:
  - coordinate type: NumericalCoordinates
  - meaning: CumulativeProbabilities
  - discretisation: RegularDiscretisation (most probably percentiles or deciles)

  Results (rasters) of a Monte Carlo analysis have the two dimensions of
  rasters mentioned above including one dimension with settings:
  - coordinate type: NumericalCoordinates (sample numbers)
  - meaning: Samples
  - discretisation: RegularDiscretisation

  \sa DataSpace

  \todo Store real world coordinates in the dimension. Get rid of the mappers.
*/
class PCR_DAL_DECL Dimension
{

private:

  static CoordinateType meaningToCoordinateType(Meaning meaning);

  static DiscretisationType meaning2DefaultDiscretisation(Meaning meaning);

  //! CoordinateType of dimension.
  CoordinateType   _type;

  //! Meaning of dimension.
  Meaning          _meaning;

  //! Discretisation of dimension.
  DiscretisationType _discretisation;

  //! Values along dimension, layout dependent on discretisation.
  std::vector<boost::any> _values;

#ifdef DEBUG_DEVELOP
  void             checkConsistency    ();
#endif

  template<typename T>
  void             values              (std::vector<T>& values) const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Dimension           ();

  template<typename T>
                   Dimension           (Meaning meaning,
                                        DiscretisationType discretisation,
                                        T const& value);

  template<typename T>
                   Dimension           (Meaning meaning,
                                        std::set<T> const& values);

  template<typename T>
                   Dimension           (Meaning meaning,
                                        std::vector<T> const& values);

  template<typename T>
                   Dimension           (Meaning meaning,
                                        T first,
                                        T last,
                                        T interval);

  /* virtual */    ~Dimension          ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  Dimension&       operator=           (Dimension const& rhs);

  Dimension&       operator|=          (Dimension const& dimension);

  Dimension&       operator&=          (Dimension const& dimension);

  template<typename T>
  void             setValues           (std::set<T> const& values);

  template<typename T>
  void             setValues           (std::vector<T> const& values);

  template<typename T>
  void             setValue            (T const& value);

  template<typename T>
  void             setValues           (T const& first,
                                        T const& last,
                                        T const& interval);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  CoordinateType   coordinateType      () const;

  Meaning          meaning             () const;

  DiscretisationType discretisation    () const;

  bool             isWide              () const;

  size_t           nrValues            () const;

  size_t           nrCoordinates       () const;

  template<typename T>
  T&               value               (size_t index);

  template<typename T>
  T const&         value               (size_t index) const;

  template<typename T>
  size_t           indexOf             (T const& value) const;

  template<typename T>
  T                coordinate          (size_t index) const;

  template<typename T>
  T                clamp               (T const& value) const;

  bool             isCompatible        (Dimension const& dimension) const;

  bool             equals              (Dimension const& rhs) const;

  bool             contains            (boost::any const& coordinate) const;

  template<typename T>
  bool             containsExactValue  (T const& value) const;

  template<typename T>
  bool             containsValueInRange(T const& value) const;

  template<typename T>
  bool             containsValueInExtent(T const& value) const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \tparam    T Type of coordinates defining the addresses along the dimension.
  \param     meaning How the coordinates should be interpreted.
  \param     discretisation How the dimension is divided/abstracted.
  \param     value Coordinate to store for this dimension.
  \warning   \a value must not be a size_t, float, std::string or a collection
             of these. Choose another constructor if you want to pass these.

  This constructor is meant to be used for space dimensions with an
  RasterDimensions or SpaceDimensions object used as \a value.
*/
template<typename T>
inline Dimension::Dimension(
         Meaning meaning,
         DiscretisationType discretisation,
         T const& value)
  : _type(meaningToCoordinateType(meaning)),
    _meaning(meaning),
    _discretisation(discretisation)

{
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<float> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<size_t> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<std::string> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<boost::any> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::set<std::string> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::set<boost::any> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, size_t>::value) &&
          !(boost::is_same<T, float>::value) &&
          !(boost::is_same<T, std::string>::value));

  _values.push_back(boost::any(value));

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

//! Constructor.
/*!
  \tparam    T Type of coordinates defining the addresses along the dimension.
  \param     meaning How the coordinates should be interpreted.
  \param     values Coordinates to store for this dimension.
  \warning   \a T must be std::string.

  This constructor is meant to be used for scenario dimensions.

  Coordinate type will be set to TextualCoordinates. Meaning will be set to
  Scenarios. Discretisation type will be set to ExactDiscretisation.
*/
template<typename T>
inline Dimension::Dimension(
         Meaning meaning,
         std::set<T> const& values)
  : _type(meaningToCoordinateType(meaning)),
    _meaning(Scenarios),
    _discretisation(ExactDiscretisation)
{
  BOOST_STATIC_ASSERT((boost::is_same<T, std::string>::value));
  assert(_meaning == Scenarios);

  _values.reserve(values.size());

  for(typename std::set<T>::const_iterator it = values.begin();
         it != values.end(); ++it) {
    _values.push_back(boost::any(*it));
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

//! Constructor.
/*!
  \tparam    T Type of coordinates defining the addresses along the dimension.
  \param     meaning How the coordinates should be interpreted.
  \param     values Coordinates to store for this dimension.
  \warning   \a T must be size_t or float.

  This constructor is meant to be used for time and uncertainty dimensions.

  Coordinate type will be set to NumericalCoordinates. Discretisation type
  will be set to RegularDiscretisation.
*/
template<typename T>
inline Dimension::Dimension(
         Meaning meaning,
         std::vector<T> const& values)
  : _type(NumericalCoordinates),
    _meaning(meaning),
    _discretisation(RegularDiscretisation),
    _values(values.size())
{
  BOOST_STATIC_ASSERT((boost::is_same<T, size_t>::value) ||
         (boost::is_same<T, float>::value));
  assert(_meaning == CumulativeProbabilities || _meaning == Time ||
         _meaning == Samples);

  for(size_t i = 0; i < values.size(); ++i) {
    _values[i] = boost::any(values[i]);
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}



//! Constructor.
/*!
  \tparam    T Type of coordinates defining the addresses along the dimension.
  \param     meaning How the coordinates should be interpreted.
  \param     first First value of a range.
  \param     last Last value of a range.
  \param     interval Interval.
  \warning   \a T must be size_t or float.

  This constructor is meant to be used for time and uncertainty dimensions.

  Coordinate type will be set to NumericalCoordinates. Discretisation type
  will be set to RegularDiscretisation.
*/
template<typename T>
Dimension::Dimension(
         Meaning meaning,
         T first,
         T last,
         T interval)
  : _type(NumericalCoordinates),
    _meaning(meaning),
    _discretisation(RegularDiscretisation),
    _values(3)
{
  BOOST_STATIC_ASSERT((boost::is_same<T, size_t>::value) ||
          (boost::is_same<T, float>::value));
  assert(_meaning == CumulativeProbabilities || _meaning == Time ||
         _meaning == Samples);

  _values[0] = boost::any(first);
  _values[1] = boost::any(last);
  _values[2] = boost::any(interval);

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

//! Returns the value at \a index.
/*!
  \param     index Index of value.
  \return    Value
  \warning   \a index must point to an existing value.
  \sa        setValue(size_t, T)
*/
template<typename T>
inline T const& Dimension::value(
         size_t index) const
{
  assert(index < _values.size());
  return boost::any_cast<T const&>(_values[index]);
}

template<typename T>
inline T& Dimension::value(
         size_t index)
{
  assert(index < _values.size());
  return *boost::any_cast<T>(&_values[index]);
}

template<typename T>
inline void Dimension::values(
         std::vector<T>& values) const
{
  BOOST_STATIC_ASSERT((boost::is_same<T, size_t>::value) ||
         (boost::is_same<T, float>::value));

  values.resize(_values.size());

  for(size_t i = 0; i < _values.size(); ++i) {
    values[i] = value<T>(i);
  }
}

template<typename T>
inline void Dimension::setValues(
         std::set<T> const& values)
{
  BOOST_STATIC_ASSERT((boost::is_same<T, std::string>::value));

  _values.resize(values.size());

  size_t i = 0;

  for(typename std::set<T>::const_iterator it = values.begin();
         it != values.end(); ++it, ++i) {
    _values[i] = boost::any(*it);
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

template<typename T>
inline void Dimension::setValues(
         std::vector<T> const& values)
{
  BOOST_STATIC_ASSERT((boost::is_same<T, size_t>::value) ||
          (boost::is_same<T, float>::value));

  _values.resize(values.size());

  for(size_t i = 0; i < values.size(); ++i) {
    _values[i] = boost::any(values[i]);
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

template<typename T>
inline void Dimension::setValue(
         T const& value)
{
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<float> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<size_t> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<std::string> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::vector<boost::any> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::set<std::string> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, std::set<boost::any> >::value));
  BOOST_STATIC_ASSERT(!(boost::is_same<T, size_t>::value) &&
          !(boost::is_same<T, float>::value));

  _values.clear();
  _values.push_back(value);

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

template<typename T>
inline void Dimension::setValues(
         T const& first,
         T const& last,
         T const& interval)
{
  BOOST_STATIC_ASSERT((boost::is_same<T, size_t>::value) ||
         (boost::is_same<T, float>::value));
  assert(_discretisation == RegularDiscretisation);
  assert(_type == NumericalCoordinates);
  assert(_values.size() == 3);

  _values[0] = first;
  _values[1] = last;
  _values[2] = interval;

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif
}

template<typename T>
inline bool Dimension::containsExactValue(
         T const& value) const
{
  for(size_t i = 0; i < nrValues(); ++i) {
    assert(_values[i].type() == typeid(T));
    if(this->template value<T>(i) == value) {
      return true;
    }
  }

  return false;
}

template<typename T>
inline bool Dimension::containsValueInRange(
         T const& value) const
{
  assert(_values[0].type() == typeid(T));
  assert(_values[1].type() == typeid(T));
  assert(_values[2].type() == typeid(T));

  T first = this->template value<T>(0);
  T last = this->template value<T>(1);
  T step = this->template value<T>(2);

  return dal::valueInRange(first, last, step, value);
}

template<typename T>
inline bool Dimension::containsValueInExtent(
         T const& value) const
{
  assert(_values[0].type() == typeid(T));
  assert(_values[1].type() == typeid(T));

  T first = this->template value<T>(0);
  T last = this->template value<T>(1);

  return dal::valueInExtent(first, last, value);
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   Result is undefined if \a value is not a valid coordinate.
  \sa        .
*/
template<typename T>
inline size_t Dimension::indexOf(
         T const& value) const
{
  assert(discretisation() == ExactDiscretisation ||
         discretisation() == RegularDiscretisation);

  size_t result;   // Default return value.

  if(discretisation() == ExactDiscretisation) {
    result = nrValues();

    for(size_t i = 0; i < nrValues(); ++i) {
      if(dal::comparable<T>(this->template value<T>(i), value)) {
        result = i;
        break;
      }
    }
  }
  else {
    T first = this->template value<T>(0);
    T interval = this->template value<T>(2);

    assert(greaterOrComparable<T>(value, first));
    assert(greaterOrComparable<T>(interval, T(0)));

    result = static_cast<size_t>((value - first) / interval);
  }

  return result;
}

template<>
inline size_t Dimension::indexOf(
         float const& value) const
{
  assert(discretisation() == ExactDiscretisation ||
         discretisation() == RegularDiscretisation);

  size_t result;

  if(discretisation() == ExactDiscretisation) {
    result = nrValues();

    for(size_t i = 0; i < nrValues(); ++i) {
      if(dal::comparable<float>(this->value<float>(i), value)) {
        result = i;
        break;
      }
    }
  }
  else {
    float first = this->value<float>(0);
    float interval = this->value<float>(2);
    result = round<float, size_t>((value - first) / interval);
  }

  return result;
}

template<>
inline size_t Dimension::indexOf(
         std::string const& value) const
{
  assert(discretisation() == ExactDiscretisation ||
         discretisation() == RegularDiscretisation);

  size_t result = nrValues();

  assert(discretisation() == ExactDiscretisation);

  for(size_t i = 0; i < nrValues(); ++i) {
    if(this->value<std::string>(i) == value) {
      result = i;
      break;
    }
  }

  return result;
}

template<typename T>
inline T Dimension::coordinate(
         size_t index) const
{
  assert(discretisation() == ExactDiscretisation ||
         discretisation() == RegularDiscretisation);

  T result = T();

  switch(discretisation()) {
    case ExactDiscretisation: {
      result = this->template value<T>(index);
      break;
    }
    case RegularDiscretisation: {
      T first = this->template value<T>(0);
      T interval = this->template value<T>(2);
      result = first + index * interval;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return result;
}

template<>
inline std::string Dimension::coordinate(
         size_t index) const
{
  return this->value<std::string>(index);
}

template<typename T>
inline T Dimension::clamp(
         T const& value) const
{
  assert(discretisation() == ExactDiscretisation ||
         discretisation() == RegularDiscretisation);
  T result;

  if(discretisation() == ExactDiscretisation) {
    std::vector<T> values;
    this->template values<T>(values);
    result = dal::clamp<T>(values, value);
  }
  else {
    T first = this->template value<T>(0);
    T last = this->template value<T>(1);
    T interval = this->template value<T>(2);
    result = dal::clamp<T>(first, last, interval, value);
  }

  return result;
}

//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

bool               operator==          (Dimension const& lhs,
                                        Dimension const& rhs);

bool               operator!=          (Dimension const& lhs,
                                        Dimension const& rhs);



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace dal

#endif
