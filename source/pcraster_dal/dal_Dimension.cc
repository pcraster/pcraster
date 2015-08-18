#ifndef INCLUDED_DAL_DIMENSION
#include "dal_Dimension.h"
#define INCLUDED_DAL_DIMENSION
#endif

// Library headers.
#ifndef INCLUDED_LIMITS
#include <limits>
#define INCLUDED_LIMITS
#endif

#ifndef INCLUDED_STRING
#include <std::string>
#define INCLUDED_STRING
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_DAL_RASTERDIMENSIONS
#include "dal_RasterDimensions.h"
#define INCLUDED_DAL_RASTERDIMENSIONS
#endif

#ifndef INCLUDED_DAL_SPATIALCOORDINATE
#include "dal_SpatialCoordinate.h"
#define INCLUDED_DAL_SPATIALCOORDINATE
#endif



/*!
  \file
  This file contains the implementation of the Dimension class.
*/



namespace dal {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC DIMENSION MEMBERS
//------------------------------------------------------------------------------

//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Default types are:
  - Scenarios: TextualCoordinates
  - Samples: NumericalCoordinates
  - Time: NumericalCoordinates
  - CumulativeProbabilities: NumericalCoordinates
  - Space: NumericalCoordinates
  - AttributeMeaning: NumericalCoordinates
*/
CoordinateType Dimension::meaningToCoordinateType(
         Meaning meaning)
{
  CoordinateType type = NrCoordinateTypes;

  switch(meaning) {
    case Scenarios: {
      type = TextualCoordinates;
      break;
    }
    case CumulativeProbabilities: {
      type = NumericalCoordinates;
      break;
    }
    case Samples: {
      type = NumericalCoordinates;
      break;
    }
    case Time: {
      type = NumericalCoordinates;
      break;
    }
    case Space: {
      type = NumericalCoordinates;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return type;
}



//!
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .

  Default discretisations are:
  - Scenarios: ExactDiscretisation
  - Samples: RegularDiscretisation
  - Time: RegularDiscretisation
  - CumulativeProbabilities: RegularDiscretisation
  - Space: RegularDiscretisation
*/
DiscretisationType Dimension::meaning2DefaultDiscretisation(
         Meaning meaning)
{
  DiscretisationType discretisation = NrDiscretisationTypes;

  switch(meaning) {
    case Scenarios: {
      discretisation = ExactDiscretisation;
      break;
    }
    case CumulativeProbabilities: {
      discretisation = RegularDiscretisation;
      break;
    }
    case Samples: {
      discretisation = RegularDiscretisation;
      break;
    }
    case Time: {
      discretisation = RegularDiscretisation;
      break;
    }
    case Space: {
      discretisation = RegularDiscretisation;
      break;
    }
    default: {
      assert(false);
      break;
    }
  }

  return discretisation;
}



//------------------------------------------------------------------------------
// DEFINITION OF DIMENSION MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Creates an invalid object. Only here to be assigned to.
*/
Dimension::Dimension()

  : _type(NrCoordinateTypes),
    _meaning(NrMeanings),
    _discretisation(NrDiscretisationTypes)

{
}



//! Destructor.
/*!
*/
Dimension::~Dimension()
{
}



//! Checks whether the settings of the dimension are valid.
/*!
  This function is called only in DEBUG_DEVELOP mode and will abort the
  application when the dimension is not valid.
*/
#ifdef DEBUG_DEVELOP
void Dimension::checkConsistency()
{
  switch(meaning()) {
    case Scenarios: {
      assert(coordinateType() == TextualCoordinates);
      assert(discretisation() == ExactDiscretisation);
      assert(!_values.empty());

      for(std::vector<boost::any>::const_iterator it = _values.begin();
               it != _values.end(); ++it) {
        assert((*it).type() == typeid(std::string));
      }

      break;
    }
    case CumulativeProbabilities: {
      assert(coordinateType() == NumericalCoordinates);
      assert(discretisation() == RegularDiscretisation);
      assert(_values.size() == 3);
      assert(_values[0].type() == typeid(float));
      assert(_values[1].type() == typeid(float));
      assert(_values[2].type() == typeid(float));
      float first = boost::any_cast<float>(_values[0]);
      float last = boost::any_cast<float>(_values[1]);
      float interval = boost::any_cast<float>(_values[2]);
      assert(first > float(0.0));
      assert(smallerOrComparable(first, last));
      assert(interval > float(0.0));

      break;
    }
    case Samples: {
      assert(coordinateType() == NumericalCoordinates);
      assert(discretisation() == RegularDiscretisation);
      assert(_values.size() == 3);
      assert(_values[0].type() == typeid(size_t));
      assert(_values[1].type() == typeid(size_t));
      assert(_values[2].type() == typeid(size_t));
      size_t first = boost::any_cast<size_t>(_values[0]);
      size_t last = boost::any_cast<size_t>(_values[1]);
      size_t interval = boost::any_cast<size_t>(_values[2]);
      assert(first > 0);
      assert(first <= last);
      assert(interval > 0);

      break;
    }
    case Time: {
      assert(coordinateType() == NumericalCoordinates);
      assert(discretisation() == RegularDiscretisation);
      assert(_values.size() == 3);
      assert(_values[0].type() == typeid(size_t));
      assert(_values[1].type() == typeid(size_t));
      assert(_values[2].type() == typeid(size_t));
      size_t first = boost::any_cast<size_t>(_values[0]);
      size_t last = boost::any_cast<size_t>(_values[1]);
      size_t interval = boost::any_cast<size_t>(_values[2]);
      assert(first > 0);
      assert(first <= last);
      assert(interval > 0);

      break;
    }
    case Space: {
      assert(coordinateType() == NumericalCoordinates);
      assert(discretisation() == RegularDiscretisation ||
         discretisation() == BorderedDiscretisation);

      switch(discretisation()) {
        case RegularDiscretisation: {
          assert(_values.size() == 1);
          assert(_values[0].type() == typeid(RasterDimensions));
          break;
        }
        case BorderedDiscretisation: {
          assert(_values.size() == 1);
          assert(_values[0].type() == typeid(SpaceDimensions));
          break;
        }
        default: {
          assert(false);
        }
      }

      break;
    }
    case NrMeanings: {
      break;
    }
  }
}
#endif



Dimension& Dimension::operator=(
         Dimension const& rhs)
{
  if(this != &rhs) {
    _type = rhs._type;
    _meaning = rhs._meaning;
    _discretisation = rhs._discretisation;
    _values = rhs._values;
  }

  return *this;
}



//! Merges \a dimension with this dimension.
/*!
  \param     dimension Dimension to merge settings from.
  \return    This dimension.
  \exception .
  \warning   .
  \sa        .
*/
Dimension& Dimension::operator|=(
         Dimension const& dimension)
{
  assert(_type == dimension._type);
  assert(_meaning == dimension._meaning);
  assert(_discretisation == dimension._discretisation); // For now.

  switch(_meaning) {
    case(Scenarios): {
      std::set<std::string> values;

      for(size_t i = 0; i < nrValues(); ++i) {
        values.insert(value<std::string>(i));
      }

      for(size_t i = 0; i < dimension.nrValues(); ++i) {
        values.insert(dimension.value<std::string>(i));
      }

      _values.clear();

      for(std::set<std::string>::const_iterator it = values.begin();
          it != values.end(); ++it) {
        _values.push_back(*it);
      }

      break;
    }
    case(CumulativeProbabilities): {
      float first = value<float>(0);
      float last = value<float>(1);
      float step = value<float>(2);

      mergeRanges(first, last, step,
         dimension.value<float>(0), dimension.value<float>(1),
         dimension.value<float>(2));

      setValues<float>(first, last, step);

      break;
    }
    case(Samples): {
      size_t first = value<size_t>(0);
      size_t last = value<size_t>(1);
      size_t step = value<size_t>(2);

      mergeRanges(first, last, step,
         dimension.value<size_t>(0), dimension.value<size_t>(1),
         dimension.value<size_t>(2));

      setValues<size_t>(first, last, step);

      break;
    }
    case(Time): {
      size_t first = value<size_t>(0);
      size_t last = value<size_t>(1);
      size_t step = value<size_t>(2);

      mergeRanges(first, last, step,
         dimension.value<size_t>(0), dimension.value<size_t>(1),
         dimension.value<size_t>(2));

      setValues<size_t>(first, last, step);

      break;
    }
    case(Space): {
      switch(_discretisation) {
        case(RegularDiscretisation): {
          value<RasterDimensions>(0) |=
              dimension.value<RasterDimensions>(0);
          break;
        }
        case(BorderedDiscretisation): {
          value<SpaceDimensions>(0) |= dimension.value<SpaceDimensions>(0);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      break;
    }
    default: {
      assert(false);
      break;
    }
  }

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  return *this;
}



//! Intersects the dimension with \a dimension.
/*!
  \param     dimension Dimension to intersect with.
  \return    This dimension.
  \exception .
  \warning   Currently not implemented, *this is always returned, \a dimension
             is not considered.
  \sa        .

  The resulting dimension contains coordinates present in both dimensions.
*/
Dimension& Dimension::operator&=(
         Dimension const&
#ifdef DEBUG_DEVELOP
         dimension
#endif
         )
{
  assert(_type == dimension._type);
  assert(_meaning == dimension._meaning);
  assert(_discretisation == dimension._discretisation); // For now.

#ifdef DEBUG_DEVELOP
  checkConsistency();
#endif

  return *this;
}



//! Returns the type.
/*!
  \return    CoordinateType
*/
CoordinateType Dimension::coordinateType() const
{
  return _type;
}



//! Returns the meaning.
/*!
  \return    Meaning
*/
Meaning Dimension::meaning() const
{
  return _meaning;
}



//! Returns the discretisation.
/*!
  \return    DiscretisationType
*/
DiscretisationType Dimension::discretisation() const
{
  return _discretisation;
}



bool Dimension::isWide() const
{
  return nrCoordinates() > 1;
}



//! Returns the number of values stored.
/*!
  \return    Number of values.
*/
size_t Dimension::nrValues() const
{
  return _values.size();
}



size_t Dimension::nrCoordinates() const
{
  size_t result = 0;

  switch(meaning()) {
    case Scenarios: {
      result = nrValues();
      break;
    }
    case CumulativeProbabilities: {
      float first = boost::any_cast<float>(_values[0]);
      float last = boost::any_cast<float>(_values[1]);
      float interval = boost::any_cast<float>(_values[2]);
      result = round<float, size_t>((last - first) / interval + float(1.0));

      break;
    }
    case Samples:
    case Time: {
      size_t first = boost::any_cast<size_t>(_values[0]);
      size_t last = boost::any_cast<size_t>(_values[1]);
      size_t interval = boost::any_cast<size_t>(_values[2]);
      result = (last - first) / interval + 1;

      break;
    }
    case Space: {
      if(discretisation() == RegularDiscretisation) {
        RasterDimensions const& dimensions(value<RasterDimensions>(0));
        result = dimensions.nrCells();
      }
      else if(discretisation() == BorderedDiscretisation) {
        SpaceDimensions const& dimensions(value<SpaceDimensions>(0));

        if(comparable<double>(dimensions.area(), 0.0)) {
          result = 1;
        }
        else {
          // Infinite number of coordinates available along this dimension.
          result = std::numeric_limits<size_t>::max();
        }
      }

      break;
    }
    case NrMeanings: {
      assert(false);
      break;
    }
  }

  return result;
}



//! Returns whether \a dimension is compatible with this dimension.
/*!
  \param     dimension Dimension to check.
  \return    True or false

  Dimensions are compatible when their type, meaning and discretization are
  equal.
*/
bool Dimension::isCompatible(
         Dimension const& dimension) const
{
  return _type == dimension._type &&
         _meaning == dimension._meaning &&
         _discretisation == dimension._discretisation;
}



//! Returns whether \a dimension equals this dimension.
/*!
  \param     dimension Dimension to check.
  \return    True or false
*/
bool Dimension::equals(
         Dimension const& rhs) const
{
  if(    _type == rhs._type &&
         _meaning == rhs._meaning &&
         _discretisation == rhs._discretisation &&
         nrValues() == rhs.nrValues()) {

    try {
      switch(meaning()) {
        case Scenarios: {
          for(size_t i = 0; i < nrValues(); ++i) {
            if(boost::any_cast<std::string>(_values[i]) !=
               boost::any_cast<std::string>(rhs._values[i])) {
              return false;
            }
          }

          break;
        }
        case CumulativeProbabilities: {
          for(size_t i = 0; i < nrValues(); ++i) {
            if(!comparable<float>(boost::any_cast<float>(_values[i]),
                   boost::any_cast<float>(rhs._values[i]))) {
              return false;
            }
          }

          break;
        }
        case Samples: {
          for(size_t i = 0; i < nrValues(); ++i) {
            if(boost::any_cast<size_t>(_values[i]) !=
               boost::any_cast<size_t>(rhs._values[i])) {
              return false;
            }
          }

          break;
        }
        case Time: {
          for(size_t i = 0; i < nrValues(); ++i) {
            if(boost::any_cast<size_t>(_values[i]) !=
               boost::any_cast<size_t>(rhs._values[i])) {
              return false;
            }
          }

          break;
        }
        case Space: {
          switch(_discretisation) {
            case RegularDiscretisation: {
              if(value<RasterDimensions>(0) != rhs.value<RasterDimensions>(0)) {
                return false;
              }

              break;
            }
            case BorderedDiscretisation: {
              if(value<SpaceDimensions>(0) != rhs.value<SpaceDimensions>(0)) {
                return false;
              }
              break;
            }
            default: {
              assert(false);
              break;
            }
          }

          break;
        }
        case NrMeanings: {
          break;
        }
      }

      return true;
    }
    catch(boost::bad_any_cast const&) {
      return false;
    }
  }

  return false;
}



bool Dimension::contains(
         boost::any const& coordinate) const
{
  assert(!coordinate.empty());

  bool result = false;

  switch(meaning()) {
    case Scenarios: {
      result = containsExactValue<std::string>(
         boost::any_cast<std::string>(coordinate));
      break;
    }
    case CumulativeProbabilities: {
      result = containsValueInRange<float>(boost::any_cast<float>(coordinate));
      break;
    }
    case Samples: {
      result = containsValueInRange<size_t>(boost::any_cast<size_t>(
         coordinate));
      break;
    }
    case Time: {
      result = containsValueInRange<size_t>(boost::any_cast<size_t>(
         coordinate));
      break;
    }
    case Space: {
      SpaceDimensions const* dimensions = 0;

      switch(discretisation()) {
        case RegularDiscretisation: {
          dimensions = &value<RasterDimensions>(0);
          break;
        }
        case BorderedDiscretisation: {
          dimensions = &value<SpaceDimensions>(0);
          break;
        }
        default: {
          assert(false);
          break;
        }
      }

      SpatialCoordinate const& spatialCoordinate(
            boost::any_cast<SpatialCoordinate const&>(coordinate));
      result = dimensions->contains(spatialCoordinate);

      break;
    }
    case NrMeanings: {
      assert(false);
      break;
    }
  }

  return result;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs equals \a rhs
/*!
  \param     lhs Dimension to check.
  \param     rhs Dimension to check.
  \return    True or false
*/
bool operator==(
         Dimension const& lhs,
         Dimension const& rhs)
{
  return lhs.equals(rhs);
}



//! Returns whether \a lhs does not equal \a rhs
/*!
  \param     lhs Dimension to check.
  \param     rhs Dimension to check.
  \return    True or false
*/
bool operator!=(
         Dimension const& lhs,
         Dimension const& rhs)
{
  return !lhs.equals(rhs);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------


} // namespace dal

