#ifndef INCLUDED_GEO_POINTVALUE
#define INCLUDED_GEO_POINTVALUE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace geo {
  // PointValue declarations.
}



namespace geo {



//! Class for points with an attribute value.
/*!
  This class layers a point and a value.
*/
template<class Point, class Value>
class PointValue
{

  friend class PointValueTest;

private:

  Point            d_point;

  Value            d_value;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PointValue          ();

                   PointValue          (Point const& point,
                                        Value const& value);

                   PointValue          (PointValue const& pointValue);

  /* virtual */    ~PointValue         ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  PointValue&      operator=           (PointValue const& pointValue);

  void             setPoint            (Point const& point);

  void             setValue            (Value const& value);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  Point const&     point               () const;

  Point&           point               ();

  Value const&     value               () const;

  Value&           value               ();

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  The layered point and value are also default constructed.
*/
template<class Point, class Value>
inline PointValue<Point, Value>::PointValue()
  : d_point(), d_value()
{
}

//! Constructor.
/*!
  \param     point Initial value of layered point.
  \param     value Initial value of layered value.
*/
template<class Point, class Value>
inline PointValue<Point, Value>::PointValue(Point const& point, Value const& value)
  : d_point(point), d_value(value)
{
}

template<class Point, class Value>
inline PointValue<Point, Value>::PointValue(PointValue const& pointValue)
  : d_point(pointValue.d_point), d_value(pointValue.d_value)
{
}

//! Destructor.
/*!
*/
template<class Point, class Value>
inline PointValue<Point, Value>::~PointValue()
{
}

template<class Point, class Value>
inline PointValue<Point, Value>& PointValue<Point, Value>::operator=(PointValue const& rhs)
{
  if(&rhs != this) {
    d_point = rhs.d_point;
    d_value = rhs.d_value;
  }

  return *this;
}

//! Sets the layered point to \a point.
/*!
  \param     point New point.
*/
template<class Point, class Value>
inline void PointValue<Point, Value>::setPoint(Point const& point)
{
  d_point = point;
}

//! Sets the layered value to \a value.
/*!
  \param     value New value.
*/
template<class Point, class Value>
inline void PointValue<Point, Value>::setValue(Value const& value)
{
  d_value = value;
}

//! Returns the layered point.
/*!
  \return    Point
*/
template<class Point, class Value>
inline Point const& PointValue<Point, Value>::point() const
{
  return d_point;
}

//! Returns the layered point.
/*!
  \return    Point
*/
template<class Point, class Value>
inline Point& PointValue<Point, Value>::point()
{
  return d_point;
}

//! Returns the layered value.
/*!
  \return    Value
*/
template<class Point, class Value>
inline Value const& PointValue<Point, Value>::value() const
{
  return d_value;
}

//! Returns the layered value.
/*!
  \return    Value
*/
template<class Point, class Value>
inline Value& PointValue<Point, Value>::value()
{
  return d_value;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo

#endif
