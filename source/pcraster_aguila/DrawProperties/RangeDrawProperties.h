#ifndef INCLUDED_RANGEDRAWPROPERTIES
#define INCLUDED_RANGEDRAWPROPERTIES



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_DRAWPROPERTIES
#include "DrawProperties.h"
#define INCLUDED_DRAWPROPERTIES
#endif



namespace ag {
  // RangeDrawProperties declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class RangeDrawProperties: public DrawProperties
{

  friend class RangeDrawPropertiesTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   RangeDrawProperties ();

                   RangeDrawProperties (QPen const& pen,
                                        QPen const& attributePen,
                                        QBrush const& brush,
                                        Palette const& palette);

  /* virtual */    ~RangeDrawProperties();

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
