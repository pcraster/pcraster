#ifndef INCLUDED_CALC_LINKINLIBRARY
#define INCLUDED_CALC_LINKINLIBRARY



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.
#ifndef INCLUDED_STRING
#include <string>
#define INCLUDED_STRING
#endif
// PCRaster library headers.

// Module headers.


namespace calc {
  // LinkInLibrary declarations.
  class LinkInLibraryPrivate;
}
namespace pcrxml {
  class LinkInExecuteInput;
  class LinkInCheckResult;
  class LinkInCheckInput;
  class LinkInLibraryManifest;
}



namespace calc {

class LinkInLibrary
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  LinkInLibrary&           operator=           (LinkInLibrary const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   LinkInLibrary               (LinkInLibrary const& rhs);

  LinkInLibraryPrivate      *d_data;

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   LinkInLibrary               (const std::string& name);

  /* virtual */    ~LinkInLibrary              ();

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------
  pcrxml::LinkInLibraryManifest const& manifest  () const;
  void              execute                      (pcrxml::LinkInExecuteInput const& li,
                                                  void ** transferArray) const;
  pcrxml::LinkInCheckResult check                (pcrxml::LinkInCheckInput const& in) const;
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
