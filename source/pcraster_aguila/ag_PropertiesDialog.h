#ifndef INCLUDED_AG_PROPERTIESDIALOG
#define INCLUDED_AG_PROPERTIESDIALOG



// Library headers.
#include <memory>

// PCRaster library headers.
#include "qt_PropertiesDialog.h"

// Module headers.



namespace ag {
  // PropertiesDialog declarations.
  class PropertiesDialogPrivate;
  class PropertiesWidget;
}



namespace ag {



//! This class is for properties dialogs.
/*!
*/
class PropertiesDialog: public qt::PropertiesDialog
{

private:

  Q_OBJECT

  std::auto_ptr<PropertiesDialogPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  PropertiesDialog& operator=          (const PropertiesDialog&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PropertiesDialog    (const PropertiesDialog&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   PropertiesDialog    (PropertiesWidget* widget,
                                        QWidget* parent = 0);

  /* virtual */    ~PropertiesDialog();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace ag

#endif
