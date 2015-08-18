#ifndef INCLUDED_DRAWPROPERTIES
#include "DrawProperties.h"
#define INCLUDED_DRAWPROPERTIES
#endif

// External headers.
#ifndef INCLUDED_CASSERT
#include <cassert>
#define INCLUDED_CASSERT
#endif

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the DrawProperties class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DRAWPROPERTIES MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF DRAWPROPERTIES MEMBERS
//------------------------------------------------------------------------------

//! Default constructor.
/*!
  Constructs a DrawProperties object. Default values are:
  - pen: black solid line pen with 0 width.
  - attributePen: black solid line pen with 0 width.
  - brush: black brush with the style Qt::NoBrush (i.e. this brush will not
    fill shapes).
  - palette: empty palette.
*/
DrawProperties::DrawProperties()

  : _pen(),
    _attributePen(),
    _brush(),
    _palette()

{
}



//! Default constructor.
/*!
  \tparam    .
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
DrawProperties::DrawProperties(
         QPen const& pen,
         QPen const& attributePen,
         QBrush const& brush,
         Palette const& palette)

  : _pen(pen),
    _attributePen(attributePen),
    _brush(brush),
    _palette(palette)

{
}



//! Destructor.
/*!
*/
DrawProperties::~DrawProperties()
{
}



void DrawProperties::setPen(
         QPen const& pen)
{
  _pen = pen;
}



void DrawProperties::setAttributePen(
         QPen const& pen)
{
  _attributePen = pen;
}



void DrawProperties::setBrush(
         QBrush const& brush)
{
  _brush = brush;
}



void DrawProperties::setPalette(
         Palette const& palette)
{
  _palette = palette;
}



QPen const& DrawProperties::pen() const
{
  return _pen;
}



QPen const& DrawProperties::attributePen() const
{
  return _attributePen;
}



QBrush const& DrawProperties::brush() const
{
  return _brush;
}



Palette const& DrawProperties::palette() const
{
  return _palette;
}



QColor const& DrawProperties::color(
         size_t i) const
{
  assert(i < _palette.size());

  return _palette[i];
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

