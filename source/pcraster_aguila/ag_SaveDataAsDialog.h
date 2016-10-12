#ifndef INCLUDED_AG_SAVEDATAASDIALOG
#define INCLUDED_AG_SAVEDATAASDIALOG



// Library headers.
#include <QDialog>

// PCRaster library headers.
#include "dal_Formats.h"

// Module headers.
#include "ui_SaveDataAsDialogBase.h"



namespace ag {
  // SaveDataAsDialog declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \todo Refactor common stuff in this class and SaveViewAsDialog class.
  \todo Default knop is save.
*/
class SaveDataAsDialog: public QDialog
{

  friend class SaveDataAsDialogTest;

private:

  Q_OBJECT

  Ui::SaveDataAsDialogBase d_ui;

  dal::Formats d_formats;

  //! Assignment operator. NOT IMPLEMENTED.
  SaveDataAsDialog& operator=          (SaveDataAsDialog const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   SaveDataAsDialog    (SaveDataAsDialog const& rhs);

  static bool      nameIsValid         (QString const& name);

private Q_SLOTS:

  void             browse              ();

  void             nameChanged         (const QString& name);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   SaveDataAsDialog    (dal::Formats const& formats,
                                        QWidget* parent = 0,
                                        bool modal = false,
                                        Qt::WindowFlags flags = Qt::Widget);

  /* virtual */    ~SaveDataAsDialog   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

  dal::Format const& selectedFormat    () const;

  std::string      name                () const;

};



//------------------------------------------------------------------------------
// INLINE FUNCTIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------

} // namespace ag

#endif
