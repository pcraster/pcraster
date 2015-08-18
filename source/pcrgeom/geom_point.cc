#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_POINT
#include "geom_point.h"
#define INCLUDED_GEOM_POINT
#endif



/*!
  Default constructor
*/
geom_Point::geom_Point()

  : d_x(0), d_y(0)

{
}



/*!
  Constructor takes an x- and y-coordinate.
*/
geom_Point::geom_Point(int x, int y)

  : d_x(x), d_y(y)

{
}



/*!
  Destructor.
*/
geom_Point::~geom_Point()
{
}



/*!
  This function sets the x-coordinate to \a x.
*/
void geom_Point::setX(int x)
{
  d_x = x;
}



/*!
  This function sets the y-coordinate to \a y.
*/
void geom_Point::setY(int y)
{
  d_y = y;
}



/*!
  This function sets the coordinates of the point to \a x, \a y.
*/
void geom_Point::setPoint(int x, int y)
{
  d_x = x;
  d_y = y;
}



/*!
  This function adds i to *this.
*/
geom_Point &geom_Point::operator+=(int i)
{
  d_x += i;
  d_y += i;
  return *this;
}



/*!
  This function moves the point by \a dx, \a dy.
*/
void geom_Point::moveBy(int dx, int dy)
{
  d_x += dx;
  d_y += dy;
}



/*!
  This function returns the x-coordinate.
*/
int geom_Point::getX() const
{
  return d_x;
}



/*!
  This function returns the y-coordinate.
*/
int geom_Point::getY() const
{
  return d_y;
}



bool geom_Point::equals(const geom_Point &point) const
{
  return ((d_x == point.getX()) && (d_y == point.getY()));
}

//------------------------------------------------------------------------------

/*!
  \relates geom_Point

  This function adds i to point and returns the result.
*/
geom_Point operator+(const geom_Point &point, int i)
{
  geom_Point result = point;
  result += i;
  return result;
}

/*!
  \relates geom_Point

  This function checks if \a point1 equals \a point2.
*/
bool operator==(const geom_Point &lhs, const geom_Point &rhs)
{
  return lhs.equals(rhs);
}



bool operator!=(const geom_Point &lhs, const geom_Point &rhs)
{
  return !lhs.equals(rhs);
}
