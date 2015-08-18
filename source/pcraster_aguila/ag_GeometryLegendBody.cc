#include "ag_GeometryLegendBody.h"

// External headers.

// Project headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the GeometryLegendBody class.
*/



namespace ag {

// Code that is private to this module.
namespace detail {

} // namespace detail



//------------------------------------------------------------------------------
// DEFINITION OF STATIC GEOMETRYLEGENDBODY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GEOMETRYLEGENDBODY MEMBERS
//------------------------------------------------------------------------------

GeometryLegendBody::GeometryLegendBody(
         DataObject const& /* object */,
         DataGuide const& /* guide */,
         ViewerType type,
         QWidget* parent)

  : LegendBody(type, parent)

{
}



GeometryLegendBody::~GeometryLegendBody()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

