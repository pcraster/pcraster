#include "ag_GeneralPreferencesWidget.h"

// Library headers.
#include <QColorDialog>
#include <QPushButton>

// PCRaster library headers.

// Module headers.
#include "ag_DataObject.h"



/*!
  \file
  This file contains the implementation of the GeneralPreferencesWidget class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC GENERALPREFERENCESWIDGET MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF GENERALPREFERENCESWIDGET MEMBERS
//------------------------------------------------------------------------------

GeneralPreferencesWidget::GeneralPreferencesWidget(
         DataObject* dataObject,
         QWidget* parent)

  : QWidget(parent),
    d_dataObject(dataObject),
    d_backgroundColour(dataObject->backgroundColour())

{
  d_ui.setupUi(this);

  connect(d_ui.d_changeBackgroundColourButton, SIGNAL(clicked()),
         this, SLOT(changeBackgroundColour()));
  connect(d_ui.d_resetButton, SIGNAL(clicked()),
         this, SLOT(resetBackgroundColour()));

  updateInterface();
}



/* NOT IMPLEMENTED
//! Copy constructor.
GeneralPreferencesWidget::GeneralPreferencesWidget(
         GeneralPreferencesWidget const& rhs)

  : Base(rhs)

{
}
*/



GeneralPreferencesWidget::~GeneralPreferencesWidget()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
GeneralPreferencesWidget& GeneralPreferencesWidget::operator=(
         GeneralPreferencesWidget const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void GeneralPreferencesWidget::updateInterface()
{
  if(!d_backgroundColour.isValid()) {
    d_ui.d_changeBackgroundColourButton->setPalette(QPalette());
  }
  else {
    QPalette palette(d_ui.d_changeBackgroundColourButton->palette());
    palette.setColor(d_ui.d_changeBackgroundColourButton->backgroundRole(),
         d_backgroundColour);
    d_ui.d_changeBackgroundColourButton->setPalette(palette);
  }
}



void GeneralPreferencesWidget::changeBackgroundColour()
{
  d_backgroundColour = QColorDialog::getColor(d_backgroundColour, this);
  updateInterface();
}



void GeneralPreferencesWidget::resetBackgroundColour()
{
  d_backgroundColour = QColor();
  updateInterface();
}



void GeneralPreferencesWidget::apply()
{
  d_dataObject->setBackgroundColour(d_backgroundColour, false);

  d_dataObject->notify();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

