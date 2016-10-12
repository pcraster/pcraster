#include "qt_PropertiesDialog.h"

// Library headers.
#include <cassert>
#include <QLayout>
#include <QPushButton>

// PCRaster library headers.

// Module headers.
#include "qt_Const.h"
#include "qt_PropertiesWidget.h"



/*!
  \file
  This file contains the implementation of the PropertiesDialog class.
*/



//------------------------------------------------------------------------------

namespace qt {

class PropertiesDialogPrivate
{
public:

  PropertiesWidget* d_widget;

  PropertiesDialogPrivate()
  {
  }

  ~PropertiesDialogPrivate()
  {
  }

};

} // namespace qt



//------------------------------------------------------------------------------
// DEFINITION OF STATIC PROPERTIESDIALOG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF PROPERTIESDIALOG MEMBERS
//------------------------------------------------------------------------------

//! Constructor.
/*!
  \param     widget Widget with the PropertiesWidget interface.
  \warning   \a widget will be reparented. Since you have to provide \a widget
             to the constructor of this dialog, you can't make it a child of
             this dialog ('this' is undefined yet). Just create \a widget with
             parent 0 and we'll reparent it to this dialog.
*/
qt::PropertiesDialog::PropertiesDialog(PropertiesWidget* widget,
                   QWidget* parent, bool modal, Qt::WindowFlags flags)

  : QDialog(parent, flags),
    d_data(new PropertiesDialogPrivate())

{
  assert(widget);

  setModal(modal);
  d_data->d_widget = widget;
  d_data->d_widget->setParent(this);

  createInterface();
}



qt::PropertiesDialog::~PropertiesDialog()
{
}



void qt::PropertiesDialog::createInterface()
{
  QBoxLayout* box, *top;

  top = new QVBoxLayout(this);

  // Layout widget as the central widget.
  top->addWidget(d_data->d_widget);

  // Create and layout buttons.
  QPushButton* ok;
  ok = new QPushButton("OK", this);
  ok->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);

  QPushButton* cancel;
  cancel = new QPushButton("Cancel", this);
  cancel->setFixedSize(qt::BUTTONWIDTH, qt::BUTTONHEIGHT);

  QPushButton* apply;
  apply = new QPushButton("Apply", this);
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

  // Connect buttons to slots.
  connect(ok, SIGNAL(clicked()), SLOT(accept()));
  connect(cancel, SIGNAL(clicked()), SLOT(reject()));
  connect(apply, SIGNAL(clicked()), SLOT(apply()));
}



void qt::PropertiesDialog::accept()
{
  d_data->d_widget->apply();
  done(1);
}



void qt::PropertiesDialog::reject()
{
  done(0);
}



void qt::PropertiesDialog::apply()
{
  d_data->d_widget->apply();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



