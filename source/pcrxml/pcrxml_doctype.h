#ifndef INCLUDED_PCRXML_DOCTYPE
#define INCLUDED_PCRXML_DOCTYPE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif

namespace pcrxml {

//! xml version and DOCTYPE notation
class DocType
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DocType&           operator=           (const DocType&);

  //! Copy constructor. NOT IMPLEMENTED.
                   DocType               (const DocType&);

  //! name of document element
  const std::string d_documentElementName;
public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DocType         (const std::string& documentElementName);

  /* virtual */    ~DocType        ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  std::string asString() const;

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
