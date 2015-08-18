#ifndef INCLUDED_GEO_POINT
#define INCLUDED_GEO_POINT

#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_CSTRING
#include <cstring>
#define INCLUDED_CSTRING
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_FUNCTIONAL
#include <functional>
#define INCLUDED_FUNCTIONAL
#endif

#ifndef INCLUDED_IOSTREAM
#include <iostream>
#define INCLUDED_IOSTREAM
#endif



namespace geo {
  template<class T, size_t n>
    class Point;
  template<class T, size_t n>
    bool operator==(Point<T, n> const& lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    bool operator!=(Point<T, n> const& lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator+(Point<T, n> const& lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator-(Point<T, n> const& lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator*(Point<T, n> const& lhs, T rhs);
  template<class T, size_t n>
    Point<T, n> operator*(T lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator*(Point<T, n> const& lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator/(Point<T, n> const& lhs, T rhs);
  template<class T, size_t n>
    Point<T, n> operator/(T lhs, Point<T, n> const& rhs);
  template<class T, size_t n>
    Point<T, n> operator/(Point<T, n> const& lhs, Point<T, n> const& rhs);
}



namespace geo {




/*
 * this enum for the indices is also used
 * for the indexDirection, x on bit 0,
 * y on bit 1, etc.
 */
enum { X = 0, Y = 1, Z = 2 };



//! A point in \a n dimensional space with \a T as coordinate type.
/*!
 * \sa Closer
 * \todo
 *  deriving from  boost::operators<> seems logical here
 * \todo
 *  write optimizations/specialization for the 1 and 2 D cases
 */
template<class T, size_t n>
class Point
{
public:

  //----------------------------------------------------------------------------
  // TYPES
  //----------------------------------------------------------------------------
  //! dimension
  enum    { Dim = n };
  typedef T CoordinateType;

private:

  //! Coordinates in d_n dimensions.
  T                d_coords[n];

public:
  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Point               ();

                   Point               (T c0,
                                        T c1);

                   Point               (Point const& point);

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //! Assignment operator.
  Point&           operator=           (Point const& point);

  T&               operator[]          (size_t i);

  T const&         operator[]          (size_t i) const;

  Point&           operator+=          (T distance);

  Point&           operator+=          (Point const& point);

  Point&           operator-=          (T distance);

  Point&           operator-=          (Point const& point);

  Point&           operator*=          (T d);

  Point&           operator*=          (Point const& point);

  Point&           operator/=          (T d);

  Point&           operator/=          (Point const& point);

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  T                x                   () const;

  T                y                   () const;

  T                z                   () const;

  T                distance            (Point const& point) const;

  T                squaredDistance     (Point const& point) const;

  //! index direction
  size_t           indexDirection      (Point const& centre) const;

  //----------------------------------------------------------------------------
  // FRIENDS
  //----------------------------------------------------------------------------

  friend bool      operator==<>        (Point const& lhs,
                                        Point const& rhs);

  friend bool      operator!=<>        (Point<T, n> const& lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator+<>       (Point<T, n> const& lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator-<>       (Point<T, n> const& lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator*<>       (Point<T, n> const& lhs,
                                        T rhs);

  friend Point<T, n> operator*<>       (T lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator*<>       (Point<T, n> const& lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator/<>       (Point<T, n> const& lhs,
                                        T rhs);

  friend Point<T, n> operator/<>       (T lhs,
                                        Point<T, n> const& rhs);

  friend Point<T, n> operator/<>       (Point<T, n> const& lhs,
                                        Point<T, n> const& rhs);

};



//! predicate to test closest distance
/*!
 * \sa Point
 */
template<class P>
class Closer : public std::binary_function<P,P,bool> {
  P d_closerTo;
 public:
  Closer(const P& closerTo):
    d_closerTo(closerTo)
    {};
  bool operator()(const P& p1, const P& p2) const {
    return p1.squaredDistance(d_closerTo) < p2.squaredDistance(d_closerTo);
  };
};

//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Coordinates will be initialised with 0.
*/
template<class T, size_t n>
inline Point<T, n>::Point()
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] = static_cast<T>(0.0);
  }
}

//! Constructor.
/*!
  \param     c0 Value of first coordinate.
  \param     c1 Value of second coordinate.
  \todo      I guess n should have a fixed value of 2 here.
             Point<T, 5>::Point(3, 4) for example doesn't seem usefull.
*/
template<class T, size_t n>
inline Point<T, n>::Point(T c0, T c1)
{
  d_coords[0] = c0;
  d_coords[1] = c1;
}

//! Copy constructor.
/*!
  \param     point Point to copy.
*/
template<class T, size_t n>
inline Point<T, n>::Point(Point const& point)
{
  std::memcpy(static_cast<void*>(d_coords),
         static_cast<void const*>(point.d_coords), n * sizeof(T));
}

//! Assignment operator.
/*!
  \param     point Point to copy.
  \return    Reference to this.
  \exception .
  \warning   .
  \sa        .
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator=(Point const& point)
{
  if(this != &point) {
    std::memcpy(static_cast<void*>(d_coords),
           static_cast<void const*>(point.d_coords), n * sizeof(T));
  }

  return *this;
}

//! Returns x-coordinate.
/*!
  \return    Coordinate.
  \warning   Number of dimensions must be > 0.
  \sa        y(), z(), operator[](size_t)

  This function assumes that the first coordinate of the point is the
  x-coordinate.
*/
template<class T, size_t n>
inline T Point<T, n>::x() const
{
  DEVELOP_PRECOND(n > 0);
  return d_coords[0];
}

//! Returns y-coordinate.
/*!
  \return    Coordinate.
  \warning   Number of dimensions must be > 1.
  \sa        x(), z(), operator[](size_t)

  This function assumes that the second coordinate of the point is the
  y-coordinate.
*/
template<class T, size_t n>
inline T Point<T, n>::y() const
{
  DEVELOP_PRECOND(n > 1);
  return d_coords[1];
}

//! Returns z-coordinate.
/*!
  \return    Coordinate.
  \warning   Number of dimensions must be > 2.
  \sa        x(), y(), operator[](size_t)

  This function assumes that the third coordinate of the point is the
  z-coordinate.
*/
template<class T, size_t n>
inline T Point<T, n>::z() const
{
  DEVELOP_PRECOND(n > 2);
  return d_coords[2];
}

//! Returns the i-coordinate.
/*!
  \param     i Index of coordinate to return.
  \return    Coordinate.
  \warning   Number of dimensions must be >= i.
  \sa        x(), y(), z()
*/
template<class T, size_t n>
inline T &Point<T, n>::operator[](size_t i)
{
  DEVELOP_PRECOND(i < n);
  return d_coords[i];
}

//! Returns the i-coordinate.
/*!
  \param     i Index of coordinate to return.
  \return    Coordinate.
  \warning   Number of dimensions must be >= i.
  \sa        x(), y(), z()
*/
template<class T, size_t n>
inline const T &Point<T, n>::operator[](size_t i) const
{
  DEVELOP_PRECOND(i < n);
  return d_coords[i];
}

//! Translates point by \a distance.
/*!
  \param     distance Distance to add.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator+=(T distance)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] += distance;
  }

  return *this;
}

//! Translates point by \a point.
/*!
  \param     point Distances to add.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator+=(Point const& point)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] += point.d_coords[i];
  }

  return *this;
}

//! Translates point by \a distance.
/*!
  \param     distance Distance to substract.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator-=(T distance)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] -= distance;
  }

  return *this;
}

//! Translates point by \a point.
/*!
  \param     point Distances to substract.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator-=(Point const& point)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] -= point.d_coords[i];
  }

  return *this;
}

//! Scales the point by \a factor.
/*!
  \param     factor Amount to multiply by.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator*=(T factor)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] *= factor;
  }

  return *this;
}

//! Scales the point by \a point.
/*!
  \param     point Amounts to multiply by.
  \return    this
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator*=(Point const& point)
{
  for(size_t i = 0; i < n; ++i) {
    d_coords[i] *= point.d_coords[i];
  }

  return *this;
}

//! Scales the point by \a factor.
/*!
  \param     factor Amount to divide by.
  \return    this
  \warning   \a factor != 0
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator/=(T factor)
{
  for(size_t i = 0; i < n; ++i) {
    DEVELOP_PRECOND(factor != T(0));
    d_coords[i] /= factor;
  }

  return *this;
}

//! Scales the point by \a point.
/*!
  \param     point Amounts to divide by.
  \return    this
  \warning   Coordinates in \a point != 0
*/
template<class T, size_t n>
inline Point<T, n> &Point<T, n>::operator/=(Point const& point)
{
  for(size_t i = 0; i < n; ++i) {
    DEVELOP_PRECOND(point.d_coords[i] != T(0));
    d_coords[i] /= point.d_coords[i];
  }

  return *this;
}

//! Returns the distance between this and \a point.
/*!
  \param     point Point to determine distance to.
  \return    distance
  \todo      return type being T is not correct, maybe a trait here
             T=float will have Distance=float, but T=int also may have
             Distance=float
  \sa        squaredDistance(Point const&)
*/
template<class T, size_t n>
T Point<T, n>::distance(Point const& point) const
{
  return static_cast<T>(std::sqrt(squaredDistance(point)));
}

//! Returns the squared distance between this and \a point.
/*!
  \param     point Point to determine distance to.
  \return    distance
  \sa        distance(Point const&)
*/
template<class T, size_t n>
T Point<T, n>::squaredDistance(Point const& point) const
{
  T result(0);

  for(size_t i = 0; i < n; ++i) {
    T distance(d_coords[i] - point.d_coords[i]);
    result += distance * distance;
  }

  return result;
}

/*!
 * see tests
 * <pre>
 *
 *  <  1 -c- >= 0
 *
 *  2D:
 *  NW 1  | NE 0
 *        0
 *  1-----C-------0
 *  SW 3  | SE 2
 *        2
 *
 * </pre>
 */
template<class T, size_t n>
size_t Point<T, n>::indexDirection(Point const& centre) const
{
  size_t d(0);
  for(size_t i = 0; i < n; ++i)
    d |= (d_coords[i] < centre[i])<<i;
  return d;
}



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------

//! Returns whether \a lhs equals \a rhs.
/*!
  \param     lhs Point.
  \param     rhs Point.
  \return    true or false
*/
template<class T, size_t n>
bool operator==(const Point<T, n> &lhs, const Point<T, n> &rhs)
{
  for(size_t i = 0; i < n; ++i) {
    if(lhs.d_coords[i] != rhs.d_coords[i]) {
      return false;
    }
  }

  return true;
}

//! Returns whether \a lhs does not equal \a rhs.
/*!
  \param     lhs Point.
  \param     rhs Point.
  \return    true or false
*/
template<class T, size_t n>
bool operator!=(const Point<T, n> &lhs, const Point<T, n> &rhs)
{
  for(size_t i = 0; i < n; ++i) {
    if(lhs.d_coords[i] != rhs.d_coords[i]) {
      return true;
    }
  }

  return false;
}

//! Adds \a rhs to \a lhs.
/*!
  \param     lhs Point.
  \param     rhs Point.
  \return    Point
*/
template<class T, size_t n>
Point<T, n> operator+(const Point<T, n> &lhs, const Point<T, n> &rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] += rhs.d_coords[i];
  }

  return result;
}

//! Substracts \a rhs from \a lhs.
/*!
  \param     lhs Point.
  \param     rhs Point.
  \return    Point
*/
template<class T, size_t n>
Point<T, n> operator-(const Point<T, n> &lhs, const Point<T, n> &rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] -= rhs.d_coords[i];
  }

  return result;
}

//! Scales \a lhs by rhs.
/*!
  \param     lhs Point.
  \param     rhs Factor.
  \return    Point
*/
template<class T, size_t n>
Point<T, n> operator*(const Point<T, n> &lhs, T rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] *= rhs;
  }

  return result;
}

//! Scales \a lhs by \a rhs.
/*!
  \param     lhs Factor.
  \param     rhs Point.
  \return    Point
*/
template<class T, size_t n>
Point<T, n> operator*(T lhs, const Point<T, n> &rhs)
{
  Point<T, n> result(rhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] *= lhs;
  }

  return result;
}

//! Scales \a lhs by \a rhs.
/*!
  \param     lhs Point.
  \param     rhs Point.
  \return    Point
*/
template<class T, size_t n>
Point<T, n> operator*(const Point<T, n> &lhs, const Point<T, n> &rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] *= rhs.d_coords[i];
  }

  return result;
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<class T, size_t n>
Point<T, n> operator/(const Point<T, n> &lhs, T rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] /= rhs;
  }

  return result;
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<class T, size_t n>
Point<T, n> operator/(T lhs, const Point<T, n> &rhs)
{
  Point<T, n> result(rhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] /= lhs;
  }

  return result;
}

//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
template<class T, size_t n>
Point<T, n> operator/(const Point<T, n> &lhs,
                      const Point<T, n> &rhs)
{
  Point<T, n> result(lhs);

  for(size_t i = 0; i < n; ++i) {
    result[i] /= rhs.d_coords[i];
  }

  return result;
}



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace geo



template<class T, size_t n>
std::ostream &operator<<(std::ostream &stream, const geo::Point<T, n> &point)
{
  for(size_t i = 0; i < n - 1; ++i) {
    stream << point[i] << ' ';
  }

  stream << point[n - 1];

  return stream;
}



#endif
