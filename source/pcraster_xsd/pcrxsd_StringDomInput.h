#ifndef INCLUDED_PCRXSD_STRINGDOMINPUT
#define INCLUDED_PCRXSD_STRINGDOMINPUT

#include "pcrxsd_dominput.h"



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

          ~StringDomInput              () override;

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
