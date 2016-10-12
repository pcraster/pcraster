#include "ag_SaveDataAsDialog.h"

// Library headers.
#include <boost/filesystem/path.hpp>
#include <QComboBox>
#include <QLineEdit>
#include <QPushButton>

// PCRaster library headers.
#include "dal_FilesystemUtils.h"
#include "qt_Util.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the SaveDataAsDialog class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SAVEDATAASDIALOG MEMBERS
//------------------------------------------------------------------------------

bool SaveDataAsDialog::nameIsValid(
         QString const& name)
{
  return !name.isEmpty();
}



//------------------------------------------------------------------------------
// DEFINITION OF SAVEDATAASDIALOG MEMBERS
//------------------------------------------------------------------------------

SaveDataAsDialog::SaveDataAsDialog(
         dal::Formats const& formats,
         QWidget* parent,
         bool modal,
         Qt::WindowFlags flags)

  : QDialog(parent, flags),
    d_formats(formats)

{
  setModal(modal);
  d_ui.setupUi(this);
  connect(d_ui.d_cancelButton, SIGNAL(clicked()),
         this, SLOT(reject()));
  connect(d_ui.d_saveButton, SIGNAL(clicked()),
         this, SLOT(accept()));
  connect(d_ui.d_browseButton, SIGNAL(clicked()),
         this, SLOT(browse()));
  connect(d_ui.d_nameEdit, SIGNAL(textChanged(QString)),
         this, SLOT(nameChanged(QString)));

  assert(!d_formats.empty());

  for(size_t i = 0; i < d_formats.size(); ++i) {
    d_ui.d_formatCombo->insertItem(i, d_formats[i].description().c_str());
  }
}



/* NOT IMPLEMENTED
//! Copy constructor.
SaveDataAsDialog::SaveDataAsDialog(
         SaveDataAsDialog const& rhs)

  : Base(rhs)

{
}
*/



SaveDataAsDialog::~SaveDataAsDialog()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
SaveDataAsDialog& SaveDataAsDialog::operator=(
         SaveDataAsDialog const& rhs)
{
  if(this != &rhs) {
  }

  return *this;
}
*/



void SaveDataAsDialog::browse()
{
  std::string name = qt::getOpenFileName(d_formats, this, 0);

  if(!name.empty()) {
    d_ui.d_nameEdit->setText(QString(name.c_str()));
  }
}



void SaveDataAsDialog::nameChanged(
         const QString& name)
{
  d_ui.d_saveButton->setEnabled(nameIsValid(name));
}



dal::Format const& SaveDataAsDialog::selectedFormat() const
{
  return d_formats[d_ui.d_formatCombo->currentIndex()];
}



std::string SaveDataAsDialog::name() const
{
  return dal::addExtensionIfNeeded(
         std::string(d_ui.d_nameEdit->text().trimmed().toUtf8().constData()),
         selectedFormat().extension()).string();
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

