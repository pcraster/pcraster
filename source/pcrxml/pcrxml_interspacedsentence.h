#ifndef INCLUDED_PCRXML_INTERSPACEDSENTENCE
#define INCLUDED_PCRXML_INTERSPACEDSENTENCE

#include "stddefx.h"
#include "pcrxml_string.h"


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

          ~InterSpacedSentence               () override;

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
