#ifndef INCLUDED_PCRXML_DOMDIFF
#define INCLUDED_PCRXML_DOMDIFF



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_QDOM
#include <qdom.h>
#define INCLUDED_QDOM
#endif

// PCRaster library headers.

// Module headers.
#ifndef INCLUDED_PCRXML_DOCUMENT
#include "pcrxml_document.h"
#define INCLUDED_PCRXML_DOCUMENT
#endif


namespace pcrxml {
  // DomDiff declarations.
}



namespace pcrxml {



//! Test two dom's for equality
/*!
 * White space only text elements are ignored in the comparison
 *
 *\todo make a switch that will do namespace processing, such that
  \code
     DomDiff d("<Main xmlns:pcr1='hx' b='c'>x<pcr1:T/> </Main>",
               "<Main xmlns:pcr2='hx' b='c'>x<pcr2:T/> </Main>");
  \endcode
  will be equal.

  Tests are in DomTest FTTB

  \warning this is not production quality code: use only in unit tests etc. for internal checking and debugging
*/
class DomDiff
{

  friend class DomDiffTest;

  Document d_doc1;
  Document d_doc2;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  DomDiff&           operator=           (DomDiff const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   DomDiff               (DomDiff const& rhs);

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   DomDiff               (const std::string& contents1,
                                          const std::string& contents2);

  /* virtual */    ~DomDiff              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  bool             equal                 (bool throwOnDiff) const;

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



} // namespace pcrxml

#endif
