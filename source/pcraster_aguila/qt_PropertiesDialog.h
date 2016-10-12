#ifndef INCLUDED_QT_PROPERTIESDIALOG
#define INCLUDED_QT_PROPERTIESDIALOG



// Library headers.
#include <memory>
#include <QDialog>

// PCRaster library headers.

// Module headers.



namespace qt {
  // PropertiesDialog declarations.
  class PropertiesDialogPrivate;
  class PropertiesWidget;
}



namespace qt {



//! The PropertiesDialog class is an abstract base for properties dialogs.
/*!
  This class can provide all standard buttons needed in properties dialogs.
*/
class PropertiesDialog: public QDialog
{

private:

  Q_OBJECT

  std::auto_ptr<PropertiesDialogPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  PropertiesDialog& operator=          (const PropertiesDialog&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PropertiesDialog    (const PropertiesDialog&);

  void             createInterface     ();

private Q_SLOTS:

  void             accept              ();

  void             reject              ();

  void             apply               ();

protected:

                   PropertiesDialog    (PropertiesWidget* widget,
                                        QWidget* parent = 0,
                                        bool modal = false,
                                        Qt::WindowFlags flags = Qt::Widget);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~PropertiesDialog   ();

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



} // namespace qt

#endif
