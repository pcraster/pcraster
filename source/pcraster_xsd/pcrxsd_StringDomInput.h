#ifndef INCLUDED_PCRXSD_STRINGDOMINPUT
#define INCLUDED_PCRXSD_STRINGDOMINPUT



// #ifndef INCLUDED_STDDEFX
// #include "stddefx.h"
// #define INCLUDED_STDDEFX
// #endif

// Library headers.

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXSD_DOMINPUT
#include "pcrxsd_dominput.h"
#define INCLUDED_PCRXSD_DOMINPUT
#endif

namespace pcrxsd {
  // StringDomInput declarations.
}



namespace pcrxsd {

//! A DOMInput with string
/*
  \sa        .
*/
class StringDomInput: public DOMInput
{

  friend class StringDomInputTest;

private:

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   StringDomInput               (std::string const& contents,
                                                 bool validate=false,
                                                 EntityResolverType erType=DefaultEntityResolver);

  virtual         ~StringDomInput              ();

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

} // namespace pcrxsd

#endif
