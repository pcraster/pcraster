#ifndef INCLUDED_AG_GEOMETRYLEGENDBODY
#define INCLUDED_AG_GEOMETRYLEGENDBODY

#include "ag_LegendBody.h"



namespace ag {
  // GeometryLegendBody declarations.
  class DataGuide;
  class DataObject;
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .

*/
// FEATURE
class GeometryLegendBody: public LegendBody
{

  friend class GeometryLegendBodyTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   GeometryLegendBody  (DataObject const& object,
                                        DataGuide const& guide,
                                        ViewerType type,
                                        QWidget* parent=nullptr);

                   GeometryLegendBody  (GeometryLegendBody const& other) = delete;

  GeometryLegendBody& operator=        (GeometryLegendBody const& other) = delete;

  /* virtual */    ~GeometryLegendBody              () override;

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
