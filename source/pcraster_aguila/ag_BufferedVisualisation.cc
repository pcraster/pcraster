#include "ag_BufferedVisualisation.h"

// Library headers.

// PCRaster library headers.

// Module headers.



/*!
  \file
  This file contains the implementation of the BufferedVisualisation class.
*/



//------------------------------------------------------------------------------

/*
namespace ag {

class BufferedVisualisationPrivate
{
public:

  BufferedVisualisationPrivate()
  {
  }

  ~BufferedVisualisationPrivate()
  {
  }

};

} // namespace ag
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC BUFFEREDVISUALISATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF BUFFEREDVISUALISATION MEMBERS
//------------------------------------------------------------------------------

ag::BufferedVisualisation::BufferedVisualisation(
         DataObject* object,
         std::string const& visualisationName,
         Alignment alignment,
         QWidget* parent,
         Qt::WindowFlags flags)

  : BufferedWidget(alignment, parent, flags),
    IVisualisation(object, visualisationName)

{
}



ag::BufferedVisualisation::~BufferedVisualisation()
{
}



bool ag::BufferedVisualisation::close()
{
  return BufferedWidget::close();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



