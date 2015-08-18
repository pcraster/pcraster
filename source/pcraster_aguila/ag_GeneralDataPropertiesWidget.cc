#include "ag_GeneralDataPropertiesWidget.h"

// Library headers.
#include <QCheckBox>
#include <QGroupBox>
#include <QLayout>
#include <QToolTip>

// PCRaster library headers.

// Module headers.
#include "ag_DataGuide.h"
#include "ag_DataObject.h"



/*!
  \file
  This file contains the implementation of the GeneralDataPropertiesWidget class.
*/



//------------------------------------------------------------------------------

namespace ag {

class GeneralDataPropertiesWidgetPrivate
{
public:

  QCheckBox*       d_show;

  GeneralDataPropertiesWidgetPrivate()

    : d_show(0)

  {
  }

  ~GeneralDataPropertiesWidgetPrivate()
  {
  }

};

} // namespace ag

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GENERALDATAPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GENERALDATAPROPERTIESWIDGET MEMBERS
//------------------------------------------------------------------------------

ag::GeneralDataPropertiesWidget::GeneralDataPropertiesWidget(
         DataObject& dataObject, const DataGuide& dataGuide,
         QWidget* parent)

  : ag::PropertiesWidget("General data properties",
    dataObject, dataGuide, parent),
    d_data(new GeneralDataPropertiesWidgetPrivate())

{
  createInterface();
  configureInterface();
}



ag::GeneralDataPropertiesWidget::~GeneralDataPropertiesWidget()
{
}



void ag::GeneralDataPropertiesWidget::createInterface()
{
  //------------------------------------------------------------------------      // Create and layout the widgets for selecting the colour palette.
  d_data->d_show = new QCheckBox("Show layer", groupBox());
  groupBoxLayout()->addWidget(d_data->d_show);
  d_data->d_show->setToolTip(
         "Select whether the data should be visualised");
}



void ag::GeneralDataPropertiesWidget::configureInterface()
{
  d_data->d_show->setChecked(dataObject().isEnabled(dataGuide()));
}



void ag::GeneralDataPropertiesWidget::rescan()
{
  if(d_data->d_show->isChecked() != dataObject().isEnabled(dataGuide())) {
    d_data->d_show->setChecked(dataObject().isEnabled(dataGuide()));
  }
}



void ag::GeneralDataPropertiesWidget::apply()
{
  // Read status of widgets and apply settings.
  dataObject().setEnabled(dataGuide(), d_data->d_show->isChecked());
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



