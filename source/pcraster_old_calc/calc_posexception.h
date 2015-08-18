#ifndef INCLUDED_CALC_POSEXCEPTION
#define INCLUDED_CALC_POSEXCEPTION

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
#ifndef INCLUDED_COM_EXCEPTION
#include "com_exception.h"
#define INCLUDED_COM_EXCEPTION
#endif

// Module headers.



namespace calc {
  // PosException declarations.
}



namespace calc {



//! exception containing positional (vi-friendly) information
class PosException : public com::Exception
{

private:
  //! Assignment operator. NOT IMPLEMENTED.
  PosException&           operator=           (const PosException&);

  //  Copy constructor. DEFAULT
  //               PosException               (const PosException&);

  //! Default constructor. NOT IMPLEMENTED.
                   PosException               ();

public:

  //----------------------------------------------------------------------------
  // CREATORS
  //----------------------------------------------------------------------------

   PosException(const std::string& message);
   PosException(
     const std::string& fileName,
     size_t lineNr,size_t charNr,
     const std::string& message);

  /* virtual */  ~PosException              ();

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



} // namespace calc

#endif
