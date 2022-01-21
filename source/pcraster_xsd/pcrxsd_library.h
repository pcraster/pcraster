#ifndef INCLUDED_PCRXSD_LIBRARY
#define INCLUDED_PCRXSD_LIBRARY


// Library headers.

// PCRaster library headers.
#ifndef INCLUDED_DEV_XERCESCLIENT
#include "dev_XercesClient.h"
#define INCLUDED_DEV_XERCESCLIENT
#endif

// Module headers.

namespace pcrxsd {
  // Library declarations.
}



namespace pcrxsd {



class Library: public dev::XercesClient
{

  friend class LibraryTest;

private:

  //! Assignment operator. NOT IMPLEMENTED.
  Library&           operator=           (Library const& rhs);

  //! Copy constructor. NOT IMPLEMENTED.
                   Library               (Library const& rhs);

protected:

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

                   Library               ();

  /* virtual */    ~Library              () override;

  //----------------------------------------------------------------------------
  // MANIPULATORS
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // ACCESSORS
  //----------------------------------------------------------------------------

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



} // namespace pcrxsd

#endif
