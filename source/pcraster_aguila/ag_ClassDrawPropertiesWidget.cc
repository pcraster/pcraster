#include "ag_ClassDrawPropertiesWidget.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"



/*!
  \file
  This file contains the implementation of the ClassDrawPropertiesWidget class.
*/



//------------------------------------------------------------------------------

namespace ag {

class ClassDrawPropertiesWidgetPrivate
{
public:

  ClassDrawPropertiesWidgetPrivate()
  {
  }

  ~ClassDrawPropertiesWidgetPrivate()
  {
  }

};

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF STATIC CLASSDRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF CLASSDRAWPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------

ag::ClassDrawPropertiesWidget::ClassDrawPropertiesWidget(
         DataObject& dataObject, const DataGuide& dataGuide,
         QWidget* parent)

  : DrawPropertiesWidget(dataObject, dataGuide, parent),
    d_data(new ClassDrawPropertiesWidgetPrivate)

{
  createInterface();
}



ag::ClassDrawPropertiesWidget::~ClassDrawPropertiesWidget()
{
}



void ag::ClassDrawPropertiesWidget::createInterface()
{
  createPaletteInterface();
}



void ag::ClassDrawPropertiesWidget::rescan()
{
  DrawPropertiesWidget::rescan();
}



void ag::ClassDrawPropertiesWidget::apply()
{
  DrawPropertiesWidget::apply();
  dataObject().notify();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



