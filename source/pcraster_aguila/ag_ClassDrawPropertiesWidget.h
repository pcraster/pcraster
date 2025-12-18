#ifndef INCLUDED_AG_CLASSDRAWPROPERTIESWIDGET
#define INCLUDED_AG_CLASSDRAWPROPERTIESWIDGET

#include "ag_DrawPropertiesWidget.h"

#include <memory>


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

  std::unique_ptr<ClassDrawPropertiesWidgetPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  ClassDrawPropertiesWidget& operator= (const ClassDrawPropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   ClassDrawPropertiesWidget(const ClassDrawPropertiesWidget&);

  void             createInterface     ();

protected:

  void     rescan              () override;

  void     apply               () override;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   ClassDrawPropertiesWidget(DataObject& dataObject,
                                        const DataGuide& dataGuide,
                                        QWidget* parent);

  /* virtual */    ~ClassDrawPropertiesWidget() override;

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
