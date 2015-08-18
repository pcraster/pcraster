#include "ag_PropertiesWidget.h"

// Library headers.
#include <QGroupBox>
#include <QBoxLayout>

// PCRaster library headers.
#include "qt_Const.h"

// Module headers.
#include "ag_DataGuide.h"
#include "ag_DataObject.h"



/*!
  \file
  This file contains the implementation of the PropertiesWidget class.
*/



//------------------------------------------------------------------------------

namespace ag {

class PropertiesWidgetPrivate
{
public:

  DataObject&      d_dataObject;
  const DataGuide& d_dataGuide;
  QGroupBox*       d_groupBox;
  QVBoxLayout*     d_groupBoxLayout;

  PropertiesWidgetPrivate(DataObject& dataObject, const DataGuide& dataGuide)
    : d_dataObject(dataObject), d_dataGuide(dataGuide), d_groupBox(0),
      d_groupBoxLayout(0)

  {
    assert(d_dataGuide.isValid());
    assert(d_dataObject.isValid(d_dataGuide));
  }

  ~PropertiesWidgetPrivate()
  {
  }

};

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------

ag::PropertiesWidget::PropertiesWidget(const std::string& title,
         DataObject& dataObject, const DataGuide& dataGuide,
         QWidget* parent)

  : qt::PropertiesWidget(parent),
    d_data(new PropertiesWidgetPrivate(dataObject, dataGuide))

{
  createInterface(title);
}



ag::PropertiesWidget::~PropertiesWidget()
{
}



void ag::PropertiesWidget::createInterface(
         std::string const& title)
{
  // Layout group box within the properties widget.
  QBoxLayout* layout = new QVBoxLayout(this);
  d_data->d_groupBox = new QGroupBox(QString(title.c_str()), this);
  layout->addWidget(d_data->d_groupBox);

  // Add a layout to the group box for sub classes to use.
  d_data->d_groupBoxLayout = new QVBoxLayout(d_data->d_groupBox);
}




QGroupBox* ag::PropertiesWidget::groupBox() const
{
  return d_data->d_groupBox;
}


QVBoxLayout* ag::PropertiesWidget::groupBoxLayout() const
{
  return d_data->d_groupBoxLayout;
}



ag::DataObject& ag::PropertiesWidget::dataObject() const
{
  return d_data->d_dataObject;
}



const ag::DataGuide& ag::PropertiesWidget::dataGuide() const
{
  return d_data->d_dataGuide;
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



