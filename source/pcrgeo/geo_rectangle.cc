#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEO_RECTANGLE
#include "geo_rectangle.h"
#define INCLUDED_GEO_RECTANGLE
#endif

#ifndef INCLUDED_CMATH
#include <cmath>
#define INCLUDED_CMATH
#endif

#ifndef INCLUDED_GEO_CONST
#include "geo_const.h"
#define INCLUDED_GEO_CONST
#endif

#ifndef INCLUDED_GEO_POINT
#include "geo_point.h"
#define INCLUDED_GEO_POINT
#endif



/*!
  Default constructor.
*/
geo_Rectangle::geo_Rectangle()

  : d_upperLeft(), d_lowerRight()

{
}



/*!
  Copy constructor.
*/
geo_Rectangle::geo_Rectangle(const geo_Rectangle &rectangle)

  : d_upperLeft(rectangle.getUpperLeft()),
    d_lowerRight(rectangle.getLowerRight())

{
}



/*!
  Constructor takes upper left and lower right coordinates.
*/
geo_Rectangle::geo_Rectangle(REAL8 ulx, REAL8 uly,
                             REAL8 lrx, REAL8 lry)
{
  d_upperLeft[geo::X] = ulx;
  d_upperLeft[geo::Y] = uly;
  d_lowerRight[geo::X] = lrx;
  d_lowerRight[geo::Y] = lry;
}



/*!
  Constructor takes a width and a height.
*/
geo_Rectangle::geo_Rectangle(REAL8 width, REAL8 height)
{
  d_lowerRight[geo::X] = width;
  d_lowerRight[geo::Y] = height;
}



/*!
  Constructor takes upper left and lower right coordinates.
*/
geo_Rectangle::geo_Rectangle(const geo::Point<REAL8, 2> &upperLeft,
                             const geo::Point<REAL8, 2> &lowerRight)

  : d_upperLeft(upperLeft), d_lowerRight(lowerRight)

{
}



/*!
  Set upper left coordinate.
*/
void geo_Rectangle::setUpperLeft(REAL8 x, REAL8 y)
{
  d_upperLeft[geo::X] = x;
  d_upperLeft[geo::Y] = y;
}



/*!
  Set upper left coordinate.
*/
void geo_Rectangle::setUpperLeft(const geo::Point<REAL8, 2> &upperLeft)
{
  d_upperLeft = upperLeft;
}



/*!
  This function moves the rectangle. The upper left coordinate of the rectangle
  will be positioned at \a x, \a y.
*/
void geo_Rectangle::moveUpperLeft(REAL8 x, REAL8 y)
{
  d_lowerRight[geo::X] = d_lowerRight[geo::X] - (d_upperLeft[geo::X] - x);
  d_lowerRight[geo::Y] = d_lowerRight[geo::Y] - (d_upperLeft[geo::Y] - y);
  d_upperLeft[geo::X]  = x;
  d_upperLeft[geo::Y]  = y;
}



/*!
  Set lower right coordinate.
*/
void geo_Rectangle::setLowerRight(REAL8 x, REAL8 y)
{
  d_lowerRight[geo::X] = x;
  d_lowerRight[geo::Y] = y;
}



/*!
  Set lower right coordinate.
*/
void geo_Rectangle::setLowerRight(const geo::Point<REAL8, 2> &lowerRight)
{
  d_lowerRight = lowerRight;
}



/*!
  This function sets the width of the rectangle to \a width. It adjusts the
  x-coordinate of the lower right corner of the rectangle.
*/
void geo_Rectangle::setWidth(REAL8 width)
{
  d_lowerRight[geo::X] = d_upperLeft[geo::X] + width;
}



/*!
  This function sets the height of the rectangle to \a width. It adjusts the
  y-coordinate of the lower right corner of the rectangle.
*/
void geo_Rectangle::setHeight(REAL8 height)
{
  d_lowerRight[geo::Y] = d_upperLeft[geo::Y] + height;
}



/*!
  This function sets the size of the rectangle to \a width and \a height. It
  adjusts the lower right corner of the rectangle.
*/
void geo_Rectangle::setSize(REAL8 width, REAL8 height)
{
  d_lowerRight[geo::X] = d_upperLeft[geo::X] + width;
  d_lowerRight[geo::Y] = d_upperLeft[geo::Y] + height;
}



/*!
  This function sets the coordinates of the rectangle to \a x, \a y, \a x +
  \a width, \a y + \a height.
*/
void geo_Rectangle::setRectangle(REAL8 x, REAL8 y,
                                 REAL8 width, REAL8 height)
{
  moveUpperLeft(x, y);
  setSize(width, height);
}



/*!
  Multiply x- and y-coordinates of upper left and lower right coordinates of
  the rectangle by i.
*/
geo_Rectangle &geo_Rectangle::operator*=(int i)
{
  d_upperLeft  *= i;
  d_lowerRight *= i;
  return *this;
}



/*!
  Multiply x- and y-coordinates of upper left and lower right coordinates of
  the rectangle by i.
*/
geo_Rectangle &geo_Rectangle::operator*=(REAL8 i)
{
  d_upperLeft  *= i;
  d_lowerRight *= i;
  return *this;
}



/*!
  Devide x- and y-coordinates of upper left and lower right coordinates of
  the rectangle by i.
*/
geo_Rectangle &geo_Rectangle::operator/=(int i)
{
  d_upperLeft  /= i;
  d_lowerRight /= i;
  return *this;
}



/*!
  Devide x- and y-coordinates of upper left and lower right coordinates of
  the rectangle by i.
*/
geo_Rectangle &geo_Rectangle::operator/=(REAL8 i)
{
  d_upperLeft  /= i;
  d_lowerRight /= i;
  return *this;
}



/*!
  Add i to the x- and y-coordinates of upper left and lower right coordinates
  of the rectangle.
*/
geo_Rectangle &geo_Rectangle::operator+=(int i)
{
  d_upperLeft  += i;
  d_lowerRight += i;
  return *this;
}



/*!
  Add i to the x- and y-coordinates of upper left and lower right coordinates
  of the rectangle.
*/
geo_Rectangle &geo_Rectangle::operator+=(REAL8 i)
{
  d_upperLeft  += i;
  d_lowerRight += i;
  return *this;
}



/*!
  Substract i from the x- and y-coordinates of upper left and lower right
  coordinates of the rectangle.
*/
geo_Rectangle &geo_Rectangle::operator-=(int i)
{
  d_upperLeft  -= i;
  d_lowerRight -= i;
  return *this;
}



/*!
  Substract i from the x- and y-coordinates of upper left and lower right
  coordinates of the rectangle.
*/
geo_Rectangle &geo_Rectangle::operator-=(REAL8 i)
{
  d_upperLeft  -= i;
  d_lowerRight -= i;
  return *this;
}



/*!
  Get the upper left coordinate of the rectangle.
*/
const geo::Point<REAL8, 2> &geo_Rectangle::getUpperLeft() const
{
  return d_upperLeft;
}



/*!
  Get the lower right coordinate of the rectangle.
*/
const geo::Point<REAL8, 2> &geo_Rectangle::getLowerRight() const
{
  return d_lowerRight;
}



/*!
  Returns x-coordinate of left side.
*/
REAL8 geo_Rectangle::getLeft() const
{
  return d_upperLeft[geo::X];
}



/*!
  Returns y-coordinate of top side.
*/
REAL8 geo_Rectangle::getTop() const
{
  return d_upperLeft[geo::Y];
}



/*!
  Returns x-coordinate of right side.
*/
REAL8 geo_Rectangle::getRight() const
{
  return d_lowerRight[geo::X];
}



/*!
  Returns y-coordinate of bottom side.
*/
REAL8 geo_Rectangle::getBottom() const
{
  return d_lowerRight[geo::Y];
}



REAL8 geo_Rectangle::left() const
{
  return d_upperLeft[geo::X];
}



REAL8 geo_Rectangle::top() const
{
  return d_upperLeft[geo::Y];
}



REAL8 geo_Rectangle::right() const
{
  return d_lowerRight[geo::X];
}



REAL8 geo_Rectangle::bottom() const
{
  return d_lowerRight[geo::Y];
}



/*!
  Get the width of the rectangle.
*/
REAL8 geo_Rectangle::getWidth() const
{
  return ABS(d_lowerRight[geo::X] - d_upperLeft[geo::X]);
}



/*!
  Get the height of the rectangle.
*/
REAL8 geo_Rectangle::getHeight() const
{
  return ABS(d_upperLeft[geo::Y] - d_lowerRight[geo::Y]);
}



/*!
  Get the area of the rectangle.
*/
REAL8 geo_Rectangle::getArea() const
{
  return getWidth() * getHeight();
}

//------------------------------------------------------------------------------

/*!
  \relates geo_Rectangle
  Multiply the rectangle by i and return the resulting point.
*/
geo_Rectangle operator*(const geo_Rectangle &rectangle, int i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle *= i;
}



/*!
  \relates geo_Rectangle
  Multiply the rectangle by i and return the resulting point.
*/
geo_Rectangle operator*(const geo_Rectangle &rectangle, REAL8 i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle *= i;
}



/*!
  \relates geo_Rectangle
  Devide the rectangle by i and return the resulting point.
*/
geo_Rectangle operator/(const geo_Rectangle &rectangle, int i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle /= i;
}



/*!
  \relates geo_Rectangle
  Devide the rectangle by i and return the resulting point.
*/
geo_Rectangle operator/(const geo_Rectangle &rectangle, REAL8 i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle /= i;
}



/*!
  \relates geo_Rectangle
  Add i to the rectangle and return the resulting point.
*/
geo_Rectangle operator+(const geo_Rectangle &rectangle, int i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle += i;
}



/*!
  \relates geo_Rectangle
  Add i to the rectangle and return the resulting point.
*/
geo_Rectangle operator+(const geo_Rectangle &rectangle, REAL8 i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle += i;
}



/*!
  \relates geo_Rectangle
  Substract i from the rectangle and return the resulting point.
*/
geo_Rectangle operator-(const geo_Rectangle &rectangle, int i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle -= i;
}



/*!
  \relates geo_Rectangle
  Substract i from the rectangle and return the resulting point.
*/
geo_Rectangle operator-(const geo_Rectangle &rectangle, REAL8 i)
{
  geo_Rectangle newRectangle = rectangle;
  return newRectangle -= i;
}

//------------------------------------------------------------------------------

/*!
  \relates geo_Rectangle

  This function calculates the bounding rectangle of \a rect1 and \a rect2
  taking into account the \a projection.
*/
geo_Rectangle bounds(const geo_Rectangle &rect1, const geo_Rectangle &rect2,
                     int projection)
{
  REAL8 ulX, lrX;
  REAL8 ulY = 0.0;
  REAL8 lrY = 0.0;

  ulX = rect1.getUpperLeft()[geo::X] < rect2.getUpperLeft()[geo::X]
          ? rect1.getUpperLeft()[geo::X]
          : rect2.getUpperLeft()[geo::X];
  lrX = rect1.getLowerRight()[geo::X] > rect2.getLowerRight()[geo::X]
          ? rect1.getLowerRight()[geo::X]
          : rect2.getLowerRight()[geo::X];

  if(projection == geo_Const::YINCRB2T)
  {
    ulY = rect1.getUpperLeft()[geo::Y] > rect2.getUpperLeft()[geo::Y]
            ? rect1.getUpperLeft()[geo::Y]
            : rect2.getUpperLeft()[geo::Y];
    lrY = rect1.getLowerRight()[geo::Y] < rect2.getLowerRight()[geo::Y]
            ? rect1.getLowerRight()[geo::Y]
            : rect2.getLowerRight()[geo::Y];
  }
  else if(projection == geo_Const::YINCRT2B)
  {
    ulY = rect1.getUpperLeft()[geo::Y] < rect2.getUpperLeft()[geo::Y]
            ? rect1.getUpperLeft()[geo::Y]
            : rect2.getUpperLeft()[geo::Y];
    lrY = rect1.getLowerRight()[geo::Y] > rect2.getLowerRight()[geo::Y]
            ? rect1.getLowerRight()[geo::Y]
            : rect2.getLowerRight()[geo::Y];
  }

  return geo_Rectangle(ulX, ulY, lrX, lrY);
}



/*!
  \relates geo_Rectangle

  This function returns if \a rect1 intersects with rect2.
*/
bool intersect(const geo_Rectangle &rect1, const geo_Rectangle &rect2,
               int projection)
{
  REAL8 maxX, minX, maxY, minY;

  maxX = MAX(rect1.getUpperLeft()[geo::X], rect2.getUpperLeft()[geo::X]);
  minX = MIN(rect1.getLowerRight()[geo::X], rect2.getLowerRight()[geo::X]);

  if(projection == geo_Const::YINCRB2T)
  {
    maxY = MIN(rect1.getLowerRight()[geo::Y], rect2.getLowerRight()[geo::Y]);
    minY = MAX(rect1.getUpperLeft()[geo::Y], rect2.getUpperLeft()[geo::Y]);
  }
  else
  {
    maxY = MIN(rect1.getUpperLeft()[geo::Y], rect2.getUpperLeft()[geo::Y]);
    minY = MAX(rect1.getLowerRight()[geo::Y], rect2.getLowerRight()[geo::Y]);
  }

  return (maxX <= minX) && (maxY <= minY);
}



/*!
  \relates geo_Rectangle

  This function returns the intersection between \a rect1 and \a rect2.
*/
geo_Rectangle intersection(const geo_Rectangle &rect1,
                           const geo_Rectangle &rect2,
                           int projection)
{
  REAL8 ulX, lrX, ulY, lrY;

  ulX = MAX(rect1.getUpperLeft()[geo::X], rect2.getUpperLeft()[geo::X]);
  lrX = MIN(rect1.getLowerRight()[geo::X], rect2.getLowerRight()[geo::X]);

  if(projection == geo_Const::YINCRB2T)
  {
    ulY = MIN(rect1.getUpperLeft()[geo::Y], rect2.getUpperLeft()[geo::Y]);
    lrY = MAX(rect1.getLowerRight()[geo::Y], rect2.getLowerRight()[geo::Y]);
  }
  else
  {
    ulY = MAX(rect1.getUpperLeft()[geo::Y], rect2.getUpperLeft()[geo::Y]);
    lrY = MIN(rect1.getLowerRight()[geo::Y], rect2.getLowerRight()[geo::Y]);
  }

  return geo_Rectangle(ulX, ulY, lrX, lrY);
}


