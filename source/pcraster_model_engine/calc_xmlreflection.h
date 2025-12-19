#ifndef INCLUDED_CALC_XMLREFLECTION
#define INCLUDED_CALC_XMLREFLECTION

#include "stddefx.h"

#include <string>


namespace pcrxml {
  class Script;
}

namespace calc {
  class ASTScript;

//! Script reflection in XML
class XMLReflection
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  XMLReflection&           operator=           (XMLReflection const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   XMLReflection               (XMLReflection const& rhs);

  pcrxml::Script  *d_script;
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   XMLReflection               (ASTScript const& script);

  /* virtual */    ~XMLReflection              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::string      toString                    () const;
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



} // namespace calc

#endif
