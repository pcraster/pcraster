#include "ag_Map.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the Map class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC MAP MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF MAP MEMBERS
//------------------------------------------------------------------------------

Map::Map(
         DataObject* object,
         std::string const& visualisationName,
         QWidget* parent)

  : Visualisation<>(object, visualisationName, parent)

{
}



Map::~Map()
{
}



QSize Map::sizeHint() const
{
  return QSize(500, 300);
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag


