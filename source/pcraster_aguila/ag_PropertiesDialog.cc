#include "ag_PropertiesDialog.h"

// Library headers.

// PCRaster library headers.

// Module headers.
#include "ag_PropertiesWidget.h"



/*!
  \file
  This file contains the implementation of the PropertiesDialog class.
*/



//------------------------------------------------------------------------------

namespace ag {

class PropertiesDialogPrivate
{
public:

  PropertiesDialogPrivate()
  {
  }

  ~PropertiesDialogPrivate()
  {
  }

};

} // namespace ag



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROPERTIESDIALOG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROPERTIESDIALOG MEMBERS
//------------------------------------------------------------------------------

ag::PropertiesDialog::PropertiesDialog(PropertiesWidget* widget,
                   QWidget* parent)

  : qt::PropertiesDialog(widget, parent, true),
    d_data(new PropertiesDialogPrivate())

{
}



ag::PropertiesDialog::~PropertiesDialog()
{
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



