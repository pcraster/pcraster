#ifndef INCLUDED_CALC_LINKINLIBRARY
#define INCLUDED_CALC_LINKINLIBRARY

#include "stddefx.h"

#include <string>


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
