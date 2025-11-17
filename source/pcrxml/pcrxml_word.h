#ifndef INCLUDED_PCRXML_WORD
#define INCLUDED_PCRXML_WORD

#include "stddefx.h"
#include "pcrxml_string.h"


namespace pcrxml {



//! Word is a non-empty string with no white space allowed
class Word : public String
{

private:

  //  Assignment operator. DEFAULT
  // Word&           operator=           (const Word&);

  //  Copy constructor.  DEFAULT
  //             Word               (const Word&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Word               (const QDomNode& owningElement, const std::string& name, bool required);
                   Word               (const std::string& value);
                   Word               (const char*        value);
                   Word               ();

          ~Word              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

};



//------------------------------------------------------------------------------
// INLINE FUNCIONS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxml

#endif
