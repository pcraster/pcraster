#ifndef INCLUDED_AG_DRAWPROPERTIESWIDGET
#define INCLUDED_AG_DRAWPROPERTIESWIDGET



// Library headers.
#include <vector>

// PCRaster library headers.

// Module headers.
#include "ag_PropertiesWidget.h"



namespace com {
  class RawPalette;
}
namespace qtw {
  class PaletteBar;
}
namespace ag {
  // DrawPropertiesWidget declarations.
  class DrawPropertiesWidgetPrivate;
}



namespace ag {



//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
*/
class DrawPropertiesWidget: public ag::PropertiesWidget
{

private:

  Q_OBJECT

  std::auto_ptr<DrawPropertiesWidgetPrivate> d_data;

  //! Assignment operator. NOT IMPLEMENTED.
  DrawPropertiesWidget& operator=      (const DrawPropertiesWidget&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DrawPropertiesWidget(const DrawPropertiesWidget&);

  void             configurePaletteInterface();

  void             addPalettes
                        (const std::vector<const com::RawPalette*>& palettes);

private Q_SLOTS:

  void             paletteBarClicked   (qtw::PaletteBar *paletteBar,
                                        QMouseEvent *event);

protected:

                   DrawPropertiesWidget(DataObject& dataObject,
                                        const DataGuide& dataGuide,
                                        QWidget* parent);

  void             createPaletteInterface();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~DrawPropertiesWidget();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  virtual void     rescan              ();

  virtual void     apply               ();

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
