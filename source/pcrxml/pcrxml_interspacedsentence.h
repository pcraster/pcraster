#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCE
#define INCLUDED_PCRXML_INTERSPACEDSENTENCE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_PCRXML_STRING
#include "pcrxml_string.h"
#define INCLUDED_PCRXML_STRING
#endif

namespace pcrxml {



//! An InterSpacedSentence is a non empty string with no whitespace at start and end
class InterSpacedSentence : public String
{

private:

  //  DEFAULT  Assignment operator.
  //  InterSpacedSentence&           operator=           (const InterSpacedSentence&);

  //  Copy constructor. DEFAULT
  //               InterSpacedSentence               (const InterSpacedSentence&);

  void validate() const;

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   InterSpacedSentence               (const QDomNode& owningElement, const std::string& name, bool req);
                   InterSpacedSentence               ();
    InterSpacedSentence& operator=                   (const std::string& value);

     virtual      ~InterSpacedSentence               ();

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
