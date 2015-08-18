#ifndef INCLUDED_AG_GENERALPREFERENCESWIDGET
#define INCLUDED_AG_GENERALPREFERENCESWIDGET



// Library headers.
#include <QWidget>

// PCRaster library headers.

// Module headers.
#include "ui_GeneralPreferencesWidgetBase.h"



namespace ag {
  // GeneralPreferencesWidget declarations.
  class DataObject;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class GeneralPreferencesWidget: public QWidget
{

  friend class GeneralPreferencesWidgetTest;

private:

  Q_OBJECT

  Ui::GeneralPreferencesWidgetBase d_ui;

  DataObject*      d_dataObject;

  QColor           d_backgroundColour;

  //! Assignment operator. NOT IMPLEMENTED.
  GeneralPreferencesWidget& operator=  (GeneralPreferencesWidget const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   GeneralPreferencesWidget(GeneralPreferencesWidget const& rhs);

  void             updateInterface     ();

private Q_SLOTS:

  void             changeBackgroundColour();

  void             resetBackgroundColour();

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GeneralPreferencesWidget(
                                        DataObject* dataObject,
                                        QWidget* parent);

  /* virtual */    ~GeneralPreferencesWidget();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  void             apply               ();

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
