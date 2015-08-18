#ifndef INCLUDED_AG_GENERALDATAPROPERTIESWIDGET
#define INCLUDED_AG_GENERALDATAPROPERTIESWIDGET



// Library headers.
#include <memory>

// PCRaster library headers.

// Module headers.
#include "ag_PropertiesWidget.h"



namespace ag {
  // GeneralDataPropertiesWidget declarations.
  class DataGuide;
  class DataObject;
  class GeneralDataPropertiesWidgetPrivate;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class GeneralDataPropertiesWidget: public PropertiesWidget
{

private:

  Q_OBJECT

  std::auto_ptr<GeneralDataPropertiesWidgetPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  GeneralDataPropertiesWidget& operator=
                                       (const GeneralDataPropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   GeneralDataPropertiesWidget
                                       (const GeneralDataPropertiesWidget&);

  void             createInterface     ();

  void             configureInterface  ();

private Q_SLOTS:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GeneralDataPropertiesWidget
                                       (DataObject& dataObject,
                                        const DataGuide& dataGuide,
                                        QWidget* parent);

  /* virtual */    ~GeneralDataPropertiesWidget();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             rescan              ();

  void             apply               ();

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
