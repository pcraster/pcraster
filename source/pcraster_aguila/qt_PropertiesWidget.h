#ifndef INCLUDED_QT_PROPERTIESWIDGET
#define INCLUDED_QT_PROPERTIESWIDGET



// Library headers.
#include <QWidget>

// PCRaster library headers.

// Module headers.



namespace qt {
  // PropertiesWidget declarations.
}



namespace qt {



//! This class is for properties edit widgets.
/*!
  It has virtual functions which the PropertiesDialog depends on.
*/
class PropertiesWidget: public QWidget
{

private:

  Q_OBJECT

  //! Assignment operator. NOT IMPLEMENTED.
  PropertiesWidget& operator=          (const PropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PropertiesWidget    (const PropertiesWidget&);

protected:

                   PropertiesWidget    (QWidget* parent = 0);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ~PropertiesWidget   ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     apply               ();

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
