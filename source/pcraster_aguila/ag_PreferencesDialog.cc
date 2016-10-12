#include "ag_PreferencesDialog.h"

// Library headers.
#include <cassert>
#include <QTabWidget>

// PCRaster library headers.

// Module headers.
#include "ag_GeneralPreferencesWidget.h"



/*!
  \file
  This file contains the implementation of the PreferencesDialog class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC PREFERENCESDIALOG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PREFERENCESDIALOG MEMBERS
//------------------------------------------------------------------------------

PreferencesDialog::PreferencesDialog   (
         DataObject* dataObject,
         QWidget* parent,
         bool modal,
         Qt::WindowFlags flags)

  : QDialog(parent, flags),
    d_dataObject(dataObject),
    d_generalPreferences(new GeneralPreferencesWidget(dataObject, this))

{
  assert(d_dataObject);

  setModal(modal);
  d_ui.setupUi(this);

  // connections.
  connect(d_ui.d_cancelButton, SIGNAL(clicked()),
         this, SLOT(reject()));
  connect(d_ui.d_okButton, SIGNAL(clicked()),
         this, SLOT(accept()));
  connect(d_ui.d_applyButton, SIGNAL(clicked()),
         this, SLOT(apply()));

  d_ui.d_preferencesTab->removeTab(d_ui.d_preferencesTab->indexOf(
         d_ui.d_preferencesTab->currentWidget()));
  d_ui.d_preferencesTab->addTab(d_generalPreferences, "General");
}



/* NOT IMPLEMENTED
//! Copy constructor.
PreferencesDialog::PreferencesDialog(
         PreferencesDialog const& rhs)

  : Base(rhs)

{
}
*/



PreferencesDialog::~PreferencesDialog()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
PreferencesDialog& PreferencesDialog::operator=(
         PreferencesDialog const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void PreferencesDialog::accept()
{
  apply();
  done(0);
}



void PreferencesDialog::apply()
{
  d_generalPreferences->apply();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

