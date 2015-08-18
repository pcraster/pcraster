#ifndef INCLUDED_SCALARDRAWPROPERTIES
#define INCLUDED_SCALARDRAWPROPERTIES



// External headers.

// Project headers.

// Module headers.
#ifndef INCLUDED_ATTRIBUTEDRAWPROPERTIES
#include "AttributeDrawProperties.h"
#define INCLUDED_ATTRIBUTEDRAWPROPERTIES
#endif



namespace ag {
  // ScalarDrawProperties declarations.
}



namespace ag {

//! short_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED
/*!
  longer_description_HORRIBLE_LONG_STRING_TO_NOTICE_THAT_IT_SHOULD_BE_REPLACED

  \sa        .
*/
class ScalarDrawProperties: public AttributeDrawProperties
{

  friend class ScalarDrawPropertiesTest;

private:

protected:

                   ScalarDrawProperties();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  virtual          ~ScalarDrawProperties();

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
