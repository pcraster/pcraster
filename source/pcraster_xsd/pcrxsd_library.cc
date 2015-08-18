#ifndef INCLUDED_PCRXSD_LIBRARY
#include "pcrxsd_library.h"
#define INCLUDED_PCRXSD_LIBRARY
#endif

// Library headers.

// PCRaster library headers.

// Module headers.

/*!
  \file
  This file contains the implementation of the Library class.
*/



namespace pcrxsd {

//------------------------------------------------------------------------------

/*
class LibraryPrivate
{
public:

  LibraryPrivate()
  {
  }

  ~LibraryPrivate()
  {
  }

};
*/



//------------------------------------------------------------------------------
// DEFINITION OF STATIC LIBRARY MEMBERS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF LIBRARY MEMBERS
//------------------------------------------------------------------------------

Library::Library()

  : dev::XercesClient()

{
  assert(dev::XercesClient::isInitialized());
}



Library::~Library()
{
}

//------------------------------------------------------------------------------
// DEFINITION OF FREE OPERATORS
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// DEFINITION OF FREE FUNCTIONS
//------------------------------------------------------------------------------



} // namespace pcrxsd

