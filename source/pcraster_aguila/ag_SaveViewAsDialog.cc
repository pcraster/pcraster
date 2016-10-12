#include "ag_SaveViewAsDialog.h"

// Library headers.
#include <boost/filesystem/operations.hpp>
#include <QComboBox>
#include <QLineEdit>
#include <QPushButton>
#include <QRadioButton>

// PCRaster library headers.
#include "dal_DataSpace.h"
#include "dal_FilesystemUtils.h"
#include "qt_Util.h"

// Module headers.



/*!
  \file
  This file contains the implementation of the SaveViewAsDialog class.
*/



namespace ag {

//------------------------------------------------------------------------------
// DEFINITION OF STATIC SAVEVIEWASDIALOG MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF SAVEVIEWASDIALOG MEMBERS
//------------------------------------------------------------------------------

SaveViewAsDialog::SaveViewAsDialog(
         std::string const& applicationName,
         dal::DataSpace const& space,
         dal::DataSpaceAddress const& address,
         std::vector<com::FileFormatInfo> const& formats,
         // std::string const& defaultName,
         QWidget* parent,
         bool modal,
         Qt::WindowFlags flags)

  : QDialog(parent, flags),
    d_applicationName(applicationName), d_formats(formats),
    d_space(space),
    d_address(address)

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

  for(size_t i = 0; i < d_formats.size(); ++i) {
    d_ui.d_formatCombo->insertItem(i, d_formats[i].description().c_str());
  }

  size_t index = d_space.indexOf(dal::Time);
  d_ui.d_allTimeStepsRadioButton->setEnabled(index < d_space.rank() &&
       d_space.dimension(index).isWide());

  // d_nameEdit->setText(defaultName);
}



/* NOT IMPLEMENTED
//! Copy constructor.
SaveViewAsDialog::SaveViewAsDialog(
         SaveViewAsDialog const& rhs)

  : Base(rhs)

{
}
*/



SaveViewAsDialog::~SaveViewAsDialog()
{
}



/* NOT IMPLEMENTED
//! Assignment operator.
SaveViewAsDialog& SaveViewAsDialog::operator=(
         SaveViewAsDialog const& rhs)
{
  if (this != &rhs) {
  }
  return *this;
}
*/



// bool SaveViewAsDialog::saveAllTimeSteps() const
// {
//   return d_allTimeStepsRadioButton->isChecked();
// }




//!
/*!
  \param     .
  \return    .
  \exception .
  \warning   .
  \sa        .
*/
dal::DataSpace SaveViewAsDialog::iterationSpace() const
{
  dal::DataSpace result(d_space, d_address);

  for(size_t i = 0; i < d_space.size(); ++i) {
    switch(d_space.dimension(i).meaning()) {
      case dal::Time: {
        if(d_ui.d_allTimeStepsRadioButton->isChecked()) {
          result.replaceDimension(i, d_space.dimension(i));
        }

        break;
      }
      default: {
        // assert(false);

        break;
      }
    }
  }

  return result;
}



com::FileFormatInfo const& SaveViewAsDialog::selectedFormat() const
{
  return d_formats[d_ui.d_formatCombo->currentIndex()];
}


std::string SaveViewAsDialog::name() const
{
  return dal::addExtensionIfNeeded(
         std::string(d_ui.d_nameEdit->text().trimmed().toUtf8().constData()),
         "." + selectedFormat().extension()).string();
}



bool SaveViewAsDialog::nameIsValid(QString const& name) const
{
  return !name.trimmed().isEmpty();
}



void SaveViewAsDialog::nameChanged(const QString& name)
{
  d_ui.d_saveButton->setEnabled(nameIsValid(name));
}



void SaveViewAsDialog::accept()
{
  QDialog::accept();
}



void SaveViewAsDialog::browse()
{
  std::string name = qt::getOpenFileName(d_formats, this, 0);

  if(!name.empty()) {
    d_ui.d_nameEdit->setText(QString(name.c_str()));
  }
}



//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

