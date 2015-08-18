#ifndef INCLUDED_COM_TEMPORALDATASOURCE
#define INCLUDED_COM_TEMPORALDATASOURCE



#ifndef INCLUDED_STDDEFX
#include "stddefx.h"
#define INCLUDED_STDDEFX
#endif

// Library headers.

// PCRaster library headers.

// Module headers.



namespace com {
  // TemporalDataSource declarations.
}



namespace com {



//! Mix-in base class for stuff that is temporal.
/*!
  This is an abstract base class.
*/
class TemporalDataSource
{

private:

  //! Assignment operator. NOT IMPLEMENTED.
  TemporalDataSource&    operator=     (const TemporalDataSource&);

  //! Copy constructor. NOT IMPLEMENTED.
                   TemporalDataSource  (const TemporalDataSource&);

protected:

                   TemporalDataSource  ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

  /* virtual */    ~TemporalDataSource ();

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



} // namespace com

#endif
