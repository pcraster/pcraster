#ifndef INCLUDED_PCRXML_WORD
#define INCLUDED_PCRXML_WORD



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
#endif


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

  virtual         ~Word              ();

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
