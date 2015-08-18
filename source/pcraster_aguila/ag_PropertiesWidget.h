#ifndef INCLUDED_AG_PROPERTIESWIDGET
#define INCLUDED_AG_PROPERTIESWIDGET



// Library headers.
#include <memory>
#include <string>

// PCRaster library headers.
#include "qt_PropertiesWidget.h"

// Module headers.



class QGroupBox;
class QVBoxLayout;
namespace ag {
  // PropertiesWidget declarations.
  class PropertiesWidgetPrivate;
  class DataObject;
  class DataGuide;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class PropertiesWidget: public qt::PropertiesWidget
{

private:

  Q_OBJECT

  std::auto_ptr<PropertiesWidgetPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  PropertiesWidget&operator=           (const PropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   PropertiesWidget    (const PropertiesWidget&);

  void             createInterface     (const std::string& title);

protected:

                   PropertiesWidget    (const std::string& title,
                                        DataObject& dataObject,
                                        const DataGuide& dataGuide,
                                        QWidget* parent);

  QGroupBox*       groupBox            () const;

  QVBoxLayout*     groupBoxLayout      () const;

  DataObject&      dataObject          () const;

  const DataGuide& dataGuide           () const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~PropertiesWidget   ();

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
