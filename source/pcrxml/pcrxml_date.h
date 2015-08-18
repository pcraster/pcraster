#ifndef INCLUDED_PCRXML_DATE
#define INCLUDED_PCRXML_DATE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef PCRXML_STRING_INCLUDED
#include "pcrxml_string.h"
#define PCRXML_STRING_INCLUDED
#endif



namespace pcrxml {



//! date value
/*!
   - still incomplete and not used yet
   - ADD WHAT DATE FORMATS ARE SUPPORTED
 */
class Date : public String
{

private:

  //! Assignment operator. NOT IMPLEMENTED
      Date&           operator=           (const Date&);

  //  Copy constructor. DEFAULT
  //               Date               (const Date&);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Date               (const QDomNode& owningElement, const std::string& name, bool required);
                   Date               ();

     virtual       ~Date              ();

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
