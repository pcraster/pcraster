#include "ag_DataPropertiesDialog.h"

// Library headers.
#include <QBoxLayout>
#include <QMenuBar>
#include <QPushButton>
#include <QTabWidget>

// PCRaster library headers.
#include "qt_Accel.h"
#include "qt_Const.h"

// Module headers.
#include "ag_ClassDrawPropertiesWidget.h"
#include "ag_DataGuide.h"
#include "ag_DataObject.h"
#include "ag_GeneralDataPropertiesWidget.h"
#include "ag_RangeDrawPropertiesWidget.h"



/*!
  \file
  This file contains the implementation of the DataPropertiesDialog class.
*/



//------------------------------------------------------------------------------

namespace ag {

class DataPropertiesDialogPrivate
{
public:

  DataGuide        d_dataGuide;        // Guide to data to edit.

  GeneralDataPropertiesWidget* d_generalProperties;
  DrawPropertiesWidget* d_drawProperties;

  DataPropertiesDialogPrivate()
    : d_generalProperties(0), d_drawProperties(0)
  {
  }

  ~DataPropertiesDialogPrivate()
  {
  }

};

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF STATIC DATAPROPERTIESEDITOR MEMBERS
//------------------------------------------------------------------------------

ag::DataPropertiesDialog* ag::DataPropertiesDialog::instance(
         DataObject* object,
         DataGuide const& guide)
{
  DataPropertiesDialog* dialog =
         VisualisationDialog<DataGuide, DataPropertiesDialog>::instance(
         object, guide);

  if(dialog) {
    dialog->raise();
  }
  else {
    // Create and add instance.
    dialog = new DataPropertiesDialog(object, guide);
    addInstance(object, guide, dialog);
  }

  assert(dialog);

  return dialog;
}



//------------------------------------------------------------------------------
// DEFINITION OF DATAPROPERTIESEDITOR MEMBERS
//------------------------------------------------------------------------------

ag::DataPropertiesDialog::DataPropertiesDialog(
         DataObject* object,
         DataGuide const& dataGuide)

  : VisualisationDialog<DataGuide, DataPropertiesDialog>(
         object, "Data Properties Dialog"),
    d_data(new DataPropertiesDialogPrivate())

{
  assert(object->isValid(dataGuide));

  d_data->d_dataGuide = dataGuide;

  createInterface();
}



ag::DataPropertiesDialog::~DataPropertiesDialog()
{
}



//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
  \bug       Set fixed size, see AnimationControl::createInterface().
*/
void ag::DataPropertiesDialog::createInterface()
{
  // ag::VisualisationWindow::createInterface();

  QBoxLayout *box, *top;

  QWidget* widget = new QWidget(this);
  top = new QVBoxLayout(widget);

  QTabWidget* tabWidget = new QTabWidget(widget);
  top->addWidget(tabWidget);

  d_data->d_generalProperties = new GeneralDataPropertiesWidget(
                   dataObject(), d_data->d_dataGuide, tabWidget);

  if(d_data->d_dataGuide.valueScale() == VS_BOOLEAN ||
     d_data->d_dataGuide.valueScale() == VS_NOMINAL ||
     d_data->d_dataGuide.valueScale() == VS_ORDINAL ||
     d_data->d_dataGuide.valueScale() == VS_LDD) {
    d_data->d_drawProperties = new ClassDrawPropertiesWidget(dataObject(),
         d_data->d_dataGuide, tabWidget);
  }
  else if(d_data->d_dataGuide.valueScale() == VS_SCALAR ||
          d_data->d_dataGuide.valueScale() == VS_DIRECTION) {
    d_data->d_drawProperties = new RangeDrawPropertiesWidget(dataObject(),
         d_data->d_dataGuide, tabWidget);
  }
  else {
    assert(false);
  }

  tabWidget->addTab(d_data->d_generalProperties, "&General");
  tabWidget->addTab(d_data->d_drawProperties, "&Draw");

  //----------------------------------------------------------------------------
  // Create and layout the buttons.
  //----------------------------------------------------------------------------

  QPushButton* ok;
  ok = new QPushButton("OK", widget);
  ok->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);

  QPushButton* cancel;
  cancel = new QPushButton("Cancel", widget);
  cancel->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);

  QPushButton* apply;
  apply = new QPushButton("Apply", widget);
  apply->setDefault(true);
  apply->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);

  box = new QHBoxLayout();
  top->addLayout(box);
  box->addStretch(1);
  box->addWidget(ok);
  box->addSpacing(qt::SPACING);
  box->addWidget(cancel);
  box->addSpacing(qt::SPACING);
  box->addWidget(apply);
  box->addStretch(1);

  // setCentralWidget(widget);
  box = new QVBoxLayout(this);
  box->setMargin(0);
  box->addWidget(widget);

  connect(ok, SIGNAL(clicked()), SLOT(accept()));
  connect(cancel, SIGNAL(clicked()), SLOT(reject()));
  connect(apply, SIGNAL(clicked()), SLOT(apply()));
}



void ag::DataPropertiesDialog::rescan()
{
  // VisualisationWindow::rescan();
  d_data->d_generalProperties->rescan();
  d_data->d_drawProperties->rescan();
}



void ag::DataPropertiesDialog::accept()
{
  apply();
  done(0);
}



void ag::DataPropertiesDialog::reject()
{
  done(0);
}



void ag::DataPropertiesDialog::apply()
{
  d_data->d_generalProperties->apply();
  d_data->d_drawProperties->apply();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



