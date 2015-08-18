#ifndef INCLUDED_AG_CLASSDRAWPROPERTIESWIDGET
#define INCLUDED_AG_CLASSDRAWPROPERTIESWIDGET



// Library headers.
#include <memory>

// PCRaster library headers.

// Module headers.
#include "ag_DrawPropertiesWidget.h"



namespace ag {
  // ClassDrawPropertiesWidget declarations.
  class ClassDrawPropertiesWidgetPrivate;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class ClassDrawPropertiesWidget: public DrawPropertiesWidget
{

private:

  Q_OBJECT

  std::auto_ptr<ClassDrawPropertiesWidgetPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  ClassDrawPropertiesWidget& operator= (const ClassDrawPropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClassDrawPropertiesWidget(const ClassDrawPropertiesWidget&);

  void             createInterface     ();

protected:

  virtual void     rescan              ();

  virtual void     apply               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClassDrawPropertiesWidget(DataObject& dataObject,
                                        const DataGuide& dataGuide,
                                        QWidget* parent);

  /* virtual */    ~ClassDrawPropertiesWidget();

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
