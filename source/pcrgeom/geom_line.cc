#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_GEOM_LINE
#include "geom_line.h"
#define INCLUDED_GEOM_LINE
#endif



/*!
  Default constructor.
*/
geom_Line::geom_Line()

  : d_xStart(0), d_yStart(0), d_xEnd(0), d_yEnd(0)

{
}



/*!
  Constructor takes the x- and y-coordinates of the begin- and end-point of
  the line.
*/
geom_Line::geom_Line(int xStart, int yStart, int xEnd, int yEnd)

  : d_xStart(xStart), d_yStart(yStart), d_xEnd(xEnd), d_yEnd(yEnd)

{
}



/*!
  Destructor.
*/
geom_Line::~geom_Line()
{
}



/*!
  This function sets the x-coordinate of the start-point.
*/
void geom_Line::setXStart(int xStart)
{
  d_xStart = xStart;
}



/*!
  This function sets the y-coordinate of the start-point.
*/
void geom_Line::setYStart(int yStart)
{
  d_yStart = yStart;
}



/*!
  This function sets the x-coordinate of the end-point.
*/
void geom_Line::setXEnd(int xEnd)
{
  d_xEnd = xEnd;
}



/*!
  This function sets the y-coordinate of the end-point.
*/
void geom_Line::setYEnd(int yEnd)
{
  d_yEnd = yEnd;
}


/*!
  This function sets the coordinates of the line.
*/
void geom_Line::setLine(int xStart, int yStart, int xEnd, int yEnd)
{
  d_xStart = xStart;
  d_yStart = yStart;
  d_xEnd   = xEnd;
  d_yEnd   = yEnd;
}



/*!
  This function move the line by \a dx, \a dy.
*/
void geom_Line::moveBy(int dx, int dy)
{
  d_xStart += dx;
  d_yStart += dy;
  d_xEnd   += dx;
  d_yEnd   += dy;
}



/*!
  This function returns the x-coordinate of the start-point.
*/
int geom_Line::getXStart() const
{
  return d_xStart;
}



/*!
  This function returns the y-coordinate of the start-point.
*/
int geom_Line::getYStart() const
{
  return d_yStart;
}



/*!
  This function returns the x-coordinate of the end-point.
*/
int geom_Line::getXEnd() const
{
  return d_xEnd;
}



/*!
  This function returns the y-coordinate of the end-point.
*/
int geom_Line::getYEnd() const
{
  return d_yEnd;
}


