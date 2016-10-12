#include "ag_Visualisation.h"

// Library headers.
#include <QTableWidget>

// PCRaster library headers.

// Module headers.
// #ifndef INCLUDED_AG_DATAOBJECT
// #include "ag_DataObject.h"
// #define INCLUDED_AG_DATAOBJECT
// #endif
// 
// #ifndef INCLUDED_AG_VISENGINE
// #include "ag_VisEngine.h"
// #define INCLUDED_AG_VISENGINE
// #endif



/*!
  \file
  This file contains the implementation of the Visualisation class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC VISUALISATION MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF VISUALISATION MEMBERS
//------------------------------------------------------------------------------

template<class Widget>
Visualisation<Widget>::Visualisation(
         DataObject* object,
         const std::string& visualisationName,
         QWidget* parent,
         Qt::WindowFlags flags)

  : Widget(parent),
    IVisualisation(object, visualisationName)

{
  Widget::setWindowFlags(flags);
  Widget::setAttribute(Qt::WA_DeleteOnClose);
}



template<class Widget>
Visualisation<Widget>::~Visualisation()
{
}



template<class Widget>
bool Visualisation<Widget>::close()
{
  return Widget::close();
}



// template<class Widget>
// void Visualisation<Widget>::process()
// {
//   std::cout << "Visualisation process" << std::endl;
//   if(visualisationEngine().change() & VisEngine::BACKGROUND_COLOUR) {
//     if(!dataObject().backgroundColour().isValid()) {
//       std::cout << "set default palette" << std::endl;
//       Widget::setPalette(QPalette());
//     }
//     else {
//       std::cout << "set new palette" << std::endl;
//       QPalette palette;
//       palette.setColor(Widget::backgroundRole(),
//          dataObject().backgroundColour());
//       Widget::setPalette(palette);
//     }
//   }
// }
// 
// 
// 
// template<class Widget>
// void Visualisation<Widget>::visualise()
// {
// }



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



template class Visualisation<QWidget>;
template class Visualisation<QTableWidget>;

} // namespace
