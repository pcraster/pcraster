#ifndef INCLUDED_AG_PREFERENCESDIALOG
#define INCLUDED_AG_PREFERENCESDIALOG



// Library headers.
#include <QDialog>

// PCRaster library headers.

// Module headers.
#include "ui_PreferencesDialogBase.h"



namespace ag {
  // PreferencesDialog declarations.
  class DataObject;
  class GeneralPreferencesWidget;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PreferencesDialog: public QDialog
{

  friend class PreferencesDialogTest;

private:

  Q_OBJECT

  Ui::PreferencesDialogBase d_ui;

  DataObject*      d_dataObject;

  GeneralPreferencesWidget* d_generalPreferences;

  //! Assignment operator. NOT IMPLEMENTED.
  PreferencesDialog& operator=         (PreferencesDialog const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   PreferencesDialog   (PreferencesDialog const& rhs);

private Q_SLOTS:

  void             accept              ();

  void             apply               ();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PreferencesDialog   (DataObject* dataObject,
                                        QWidget* parent=0,
                                        bool model=false,
                                        Qt::WindowFlags flags=Qt::Widget);

  /* virtual */    ~PreferencesDialog  ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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
